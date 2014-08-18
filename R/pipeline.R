
start <- function(oname, mref=NA, mname=NA, pref=NA, pname=NA) {
    # Need exactly ONE of module or pipeline ref name
    if ((is.na(mref) + is.na(mname) + is.na(pref) + is.na(pname)) != 3)
        stop("Invalid pipe start")
    c(outputName=oname, moduleRef=mref, moduleName=mname,
      pipelineRef=pref, pipelineName=pname)
}

end <- function(iname, mref=NA, mname=NA, pref=NA, pname=NA) {
    # Need exactly ONE of module or pipeline ref name
    if ((is.na(mref) + is.na(mname) + is.na(pref) + is.na(pname)) != 3)
        stop("Invalid pipe end")
    c(inputName=iname, moduleRef=mref, moduleName=mname,
      pipelineRef=pref, pipelineName=pname)
}

pipe <- function(start, end) {
    list(src=start, dst=end)
}

moduleRef <- function(filename, name=NA) {
    c(moduleRef=filename, moduleName=name, pipelineRef=NA, pipelineName=NA)
}

pipelineRef <- function(filename, name=NA) {
    c(moduleName=NA, moduleRef=NA, pipelineRef=filename, pipelineName=name)
}

# Strip NA elements
strip <- function(x) {
    x[!is.na(x)]
}

pipeline <- function(components=NULL, pipes, desc=NULL) {
    doc <- newXMLDoc(namespaces="http://www.openapi.org/2014/",
                     node=newXMLNode("pipeline", attrs=c(version="0.1"),
                         namespaceDefinitions="http://www.openapi.org/2014/"))
    root <- xmlRoot(doc)
    if (!is.null(pipes)) {
        pipeNodes <- lapply(pipes,
                            function(p) {
                                newXMLNode("pipe",
                                           newXMLNode("start",
                                                      attrs=strip(p$src)),
                                           newXMLNode("end",
                                                      attrs=strip(p$dst)))
                            })
    }
    if (is.null(components)) {
        compInfo <- NULL
    } else {
        compInfo <- do.call(rbind, components)
    }
    if (!is.null(pipes)) {
        pipeInfo <- do.call(rbind,
                            lapply(pipes,
                                   function(x) rbind(x$src[-1], x$dst[-1])))
        # Only interested in '*Ref' pipes
        # ('*Name' pipes have to have a component that resolves '*Ref')
        pipeInfo <- pipeInfo[!(is.na(pipeInfo[, "moduleRef"]) &
                               is.na(pipeInfo[, "pipelineRef"])), ]
        compInfo <- unique(rbind(compInfo, pipeInfo))
    }
    if (is.null(compInfo)) {
        componentNodes <- NULL
    } else {
        componentNodes <- apply(compInfo, 1,
                                function(x) {
                                    newXMLNode("component",
                                               attrs=strip(x))
                                })
    }
    if (!is.null(desc)) {
        desc <- newXMLNode("description",
                           newXMLCDataNode(desc))
    }
    addChildren(root,
                kids=c(desc, componentNodes, pipeNodes))
}

writePipeline <- function(filename, ..., dir="XML") {
    pipeline <- pipeline(...)
    saveXML(pipeline, file.path(dir, filename))    
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
        c(src=getComponentName(src),
          srcname=xmlGetAttr(src, "outputName"),
          dst=getComponentName(dst),
          dstname=xmlGetAttr(dst, "inputName"))
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
                                 outputs(c)[, c("name", "type",
                                                "format", "formatType"),
                                            drop=FALSE]
                             }
                         })
    componentInfo <- cbind(do.call("rbind", outputInfo),
                           component=names(components))
    # Merge module input/output info with pipe output info
    info <- as.matrix(merge(pipes, componentInfo, all.x=TRUE,
                            by.x=c("src", "srcname"),
                            by.y=c("component", "name")))
    info[,
         c("src", "srcname", "dst", "dstname", "type", "format", "formatType"),
         drop=FALSE]
}

loadComponent <- function(x, pipepath) {
    mref <- xmlGetAttr(x, "moduleRef")
    if (is.null(mref)) {
        pref <- xmlGetAttr(x, "pipelineRef")
        readPipeline(pref, pipepath)
    } else {
        readModule(mref, pipepath)
    }
}

getComponentName <- function(x) {
    mref <- xmlGetAttr(x, "moduleRef")
    mname <- xmlGetAttr(x, "moduleName")
    if (is.null(mref) && is.null(mname)) {
        pref <- xmlGetAttr(x, "pipelineRef")
        pname <- xmlGetAttr(x, "pipelineName")
        if (is.null(pname)) {
            nameFromFilename(pref)
        } else {
            pname
        }
    } else {
        if (is.null(mname)) {
            nameFromFilename(mref)
        } else {
            mname
        }
    }
}

readXMLPipeline <- function(x, pipepath) {
    pipeline <- xmlRoot(x)
    version <- xmlGetAttr(pipeline, "version")
    descNodes <- getNodeSet(pipeline, "oa:description",
                            namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(descNodes)) {
        desc <- xmlValue(descNodes[[1]])
    } else {
        desc <- ""
    }
    # Read the components
    componentNodes <-
        getNodeSet(pipeline, "oa:component",
                   namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(componentNodes)) {
        componentNames <- sapply(componentNodes, getComponentName)
        components <- lapply(componentNodes, loadComponent, pipepath)
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
    result <- list(version=version,
                   desc=desc,
                   componentOrder=order,
                   components=components,
                   graph=graph,
                   pipes=pipeInfo)
    class(result) <- "pipeline"
    result
}

readPipeline <- function(filename, path="XML") {
    if (absPath(filename)) {
        file <- filename
    } else{
        file <- findFile(filename, path)
        if (is.null(file))
            stop("Unable to find pipeline")
    }

    pipelinePath <- dirname(file)
        
    txt <- readRef(file)
    xml <- xmlParse(txt, asText=TRUE)
    readXMLPipeline(xml, pipelinePath)
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

runPipeline <- function(x, name=NULL, inputs=NULL, filebase="./Pipelines") {
    pipename <- name
    # 'x' may be just pipeline name for convenience
    if (inherits(x, "pipeline")) {
        if (is.null(pipename)) {
            pipename <- deparse(substitute(x))
        }
    } else if (is.character(x)) {
        if (is.null(pipename)) {
            pipename <- nameFromFilename(x)
        }
        x <- readPipeline(x)
    } else {
        stop("Invalid pipeline")
    }
    
    # create a directory for pipeline output
    if (!file.exists(filebase)) {
        dir.create(filebase)
    }
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
        cname <- x$componentOrder[i]
        c <- x$components[[cname]]
        # FIXME:  efficiency could be gained by incrementally updating inputs
        #         rather than redoing every time AND by selecting only the
        #         relevant inputs to pass to runModule() each time
        inputs <- resolveInputs(cname, x$pipes, compresults)
        newresults <- runComponent(c, cname, inputs, filebase=compfilebase)
        compresults <- rbind(compresults, newresults)
    }
    # "rename" results to reflect fact that they are outputs from this pipeline
    # FIXME:  will eventually need something more sophisticated to allow
    # for two module outputs from distinct modules, but with same output name
    compresults[, "compname"] <- pipename
    compresults
}

