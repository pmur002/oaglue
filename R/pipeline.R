
pipe <- function(src, srcname, dst, dstname) {
    list(src=c(component=unname(src), name=unname(srcname)),
         dst=c(component=unname(dst), name=unname(dstname)))
}

pipeline <- function(name, components=NULL, pipes, desc=NULL) {
    doc <- newXMLDoc(namespaces="http://www.openapi.org/2014/",
                     node=newXMLNode("pipeline", attrs=c(version="0.1"),
                         namespaceDefinitions="http://www.openapi.org/2014/"))
    root <- xmlRoot(doc)
    if (!is.null(pipes)) {
        pipeNodes <- lapply(pipes,
                            function(p) {
                                newXMLNode("pipe",
                                           newXMLNode("start",
                                                      attrs=p$src),
                                           newXMLNode("end",
                                                      attrs=p$dst))
                            })
    }
    if (!is.null(components)) {
        cnames <- names(components)
        if (is.null(cnames)) {
            crefs <- NULL
            cnames <- components
        } else {
            isref <- nchar(cnames)
            crefs <- ifelse(isref, components, "")
            cnames <- ifelse(isref, cnames, components)
        }
    } else {
        cnames <- NULL
        crefs <- NULL
    }
    # Add implicit components from pipes
    pipecnames <- unique(unlist(lapply(pipes,
                                       function(p) {
                                           c(p$src["component"],
                                             p$dst["component"])
                                       })))
    extranames <- pipecnames[!pipecnames %in% cnames]
    cnames <- c(cnames, extranames)
    crefs <- c(crefs, rep("", length(cnames) - length(crefs)))
    componentNodes <- mapply(function(name, ref) {
                                 if (nchar(ref)) {
                                     attrs <- c(name=name, ref=ref)
                                 } else {
                                     attrs <- c(name=name)
                                 }
                                 newXMLNode("component", attrs=attrs)
                             },
                             cnames, crefs)
    if (!is.null(desc)) {
        desc <- newXMLNode("description",
                           newXMLCDataNode(desc))
    }
    addChildren(root,
                kids=c(desc, componentNodes, pipeNodes))
}

writePipeline <- function(name, ..., dir="XML") {
    pipeline <- pipeline(name, ...)
    saveXML(pipeline, file.path(dir, paste0(name, ".xml")))    
}

require(RBGL)

edges <- function(component, pipes) {
    e <- unique(pipes[, "dst"][pipes[, "src"] == component])
    e <- e[!is.na(e)]
    if (!length(e)) {
        list()
    } else {
        list(edges=e)
    }
}

pipelineGraph <- function(components, pipes) {
    nodes <- components
    edgeList <- lapply(components, edges, pipes)
    names(edgeList) <- components
    new("graphNEL", nodes, edgeList, "directed")
}

readPipe <- function(x) {
    src <- getNodeSet(x, "oa:start",
                      namespaces=c(oa="http://www.openapi.org/2014/"))
    dst <- getNodeSet(x, "oa:end",
                      namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(src) && length(dst)) {
        src <- src[[1]]
        dst <- dst[[1]]
        c(src=xmlGetAttr(src, "component"),
          srcname=xmlGetAttr(src, "name"),
          dst=xmlGetAttr(dst, "component"),
          dstname=xmlGetAttr(dst, "name"))
    } else {
        stop("Invalid pipe")
    }
}

mergeInfo <- function(pipes, components) {
    # Stack up module info
    outputInfo <- lapply(components,
                         function(c) {
                             if (is.null(outputs(c))) {
                                 NULL
                             } else {
                                 cbind(outputs(c)[, c("name", "type", "format", "formatType"),
                                                 drop=FALSE],
                                       component=c$name)
                             }
                         })
    componentInfo <- do.call("rbind", outputInfo)
    # Merge module input/output info with pipe output info
    info <- as.matrix(merge(pipes, componentInfo, all.x=TRUE,
                            by.x=c("src", "srcname"),
                            by.y=c("component", "name")))
    info[,
         c("src", "srcname", "dst", "dstname", "type", "format", "formatType"),
         drop=FALSE]
}

readComponent <- function(x, path="XML") {
    name <- paste0(x, ".xml")
    if (absPath(name)) {
        file <- name
    } else{
        file <- findFile(name, path)
        if (is.null(file))
            stop("Unable to find component")
    }

    componentName <- basename(file_path_sans_ext(file))

    txt <- readRef(file)
    xml <- xmlParse(txt, asText=TRUE)
    componentType <- xmlName(xmlRoot(xml))

    if (componentType == "module") {
        readXMLModule(xml, componentName)
    } else if (componentType == "pipeline") {
        readXMLPipeline(xml, componentName)
    } else {
        stop("Invalid component")
    }
}

loadComponent <- function(x) {
    # If the component only has a 'name', use that to read the module
    # If the component has a 'ref', use that to read the module
    name <- xmlGetAttr(x, "name")
    ref <- xmlGetAttr(x, "ref")
    if (is.null(ref)) {
        readComponent(name)
    } else {
        readComponent(ref)
    }
}

readXMLPipeline <- function(x, name) {
    pipeline <- xmlRoot(x)
    version <- xmlGetAttr(pipeline, "version")
    descNodes <- getNodeSet(pipeline, "oa:desc",
                            namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(descNodes)) {
        desc <- xmlValue(descNodes[[1]])
    } else {
        desc <- ""
    }
    # Read the components
    componentNodes <- getNodeSet(pipeline, "oa:component",
                              namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(componentNodes)) {
        componentNames <- sapply(componentNodes, xmlGetAttr, "name")
        components <- lapply(componentNodes, loadComponent)
        names(components) <- componentNames
    } else {
        stop("Pipeline has no components")
    }
    # Read pipe information
    pipeNodes <- getNodeSet(pipeline, "oa:pipe",
                            namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(pipeNodes)) {
        pipes <- do.call("rbind", lapply(pipeNodes, readPipe))
    } else {
        pipes <- NULL
    }
    # Merge component input/output information with pipe information
    # (this gathers all the information necessary to get input/outputs
    #  for a component)
    pipeInfo <- mergeInfo(pipes, components)
    # Generate graph for pipeline and use that to determine component order
    graph <- pipelineGraph(componentNames, pipes)
    order <- tsort(graph)
    # Build pipeline object
    result <- list(name=name,
                   version=version,
                   desc=desc,
                   componentOrder=order,
                   components=components,
                   graph=graph,
                   pipes=pipeInfo)
    class(result) <- "pipeline"
    result
}

readPipeline <- function(x, path="XML") {
    name <- paste0(x, ".xml")
    if (absPath(name)) {
        file <- name
    } else{
        file <- findFile(name, path)
        if (is.null(file))
            stop("Unable to find pipeline")
    }

    pipelineName <- basename(file_path_sans_ext(file))

    txt <- readRef(file)
    xml <- xmlParse(txt, asText=TRUE)
    readXMLPipeline(xml, pipelineName)
}

inputs.pipeline <- function(x, ..., all=FALSE) {
    allInputs <- do.call("rbind", lapply(x$components, inputs))
    if (all) {
        allInputs
    } else {
        fedInputs <- allInputs[, "name"] %in% x$pipes[, "dstname"]
        if (all(fedInputs)) {
            NULL
        } else {
            allInputs[!fedInputs, , drop=FALSE]
        }
    }
}

outputs.pipeline <- function(x, ..., all=FALSE) {
    allOutputs <- stackOutputs(lapply(x$components, outputs))
    if (all) {
        allOutputs
    } else {
        consumedOutputs <- allOutputs[, "name"] %in% x$pipes[, "srcname"]
        if (all(consumedOutputs)) {
            NULL
        } else {
            allOutputs[!consumedOutputs, , drop=FALSE]
        }
    }
}

print.pipeline <- function(x, ...) {
    cat("Name:", x$name, "\n")
    cat(paste("    ", x$componentOrder), sep="\n")
}

plot.pipeline <- function(x, ...) {
    require(gridGraphviz)
    rag <- agopenTrue(x$graph, "",
                      attrs=list(node=
                          list(shape="ellipse")))
    grid.graph(rag)
}

resolveInputs <- function(compname, pipes, results) {
    if (is.null(results)) {
        return(NULL)
    }
    comppipes <- pipes[pipes[, "dst"] == compname, , drop=FALSE]
    inputs <- merge(comppipes, results,
                    by.x=c("src", "srcname", "type", "format", "formatType"),
                    by.y=c("compname", "name", "type", "format", "formatType"))
    if (nrow(inputs) == 0) {
        return(NULL)
    }
    result <- inputs[, c("dstname", "type", "ref", "format", "formatType")]
    names(result) <- c("name", "type", "ref", "format", "formatType")
    result
}

runComponent <- function(x, ...) {
    UseMethod("runComponent")
}

runComponent.module <- function(x, ...) {
    runModule(x, ...)
}

runComponent.pipeline <- function(x, ...) {
    runPipeline(x, ...)
}

runPipeline <- function(x, inputs=NULL, filebase="./Pipelines") {
    # 'x' may be just pipeline name for convenience
    if (!inherits(x, "pipeline") && is.character(x))
        x <- readPipeline(x)
    
    # create a directory for pipeline output
    if (!file.exists(filebase)) {
        dir.create(filebase)
    }
    pipename <- x$name
    pipepath <- file.path(filebase, pipename)
    if (file.exists(pipepath))
        unlink(pipepath, recursive=TRUE)
    dir.create(pipepath)
    compfilebase <- file.path(pipepath, "Components") 

    # Module order has been determined in readPipeline; run in order
    n <- length(x$components)
    compresults <- NULL
    if (n == 0)
        return()
    for (i in 1:n) {
        c <- x$components[[x$componentOrder[i]]]
        # FIXME:  efficiency could be gained by incrementally updating inputs
        #         rather than redoing every time AND by selecting only the
        #         relevant inputs to pass to runModule() each time
        inputs <- resolveInputs(c$name, x$pipes, compresults)
        newresults <- runComponent(c, inputs, filebase=compfilebase)
        compresults <- rbind(compresults, newresults)
    }
    # "rename" results to reflect fact that they are outputs from this pipeline
    # FIXME:  will eventually need something more sophisticated to allow
    # for two module outputs from distinct modules, but with same output name
    compresults[, "compname"] <- pipename
    compresults
}

