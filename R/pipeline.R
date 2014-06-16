
pipe <- function(srcmod, srcname, dstmod, dstname) {
    list(src=c(module=srcmod, name=srcname),
         dst=c(module=dstmod, name=dstname))
}

pipeline <- function(name, modules=NULL, pipes) {
    doc <- newXMLDoc(namespaces="http://www.openapi.org/2014/",
                     node=newXMLNode("pipeline", 
                         namespaceDefinitions="http://www.openapi.org/2014/"))
    root <- xmlRoot(doc)
    if (!is.null(pipes)) {
        pipeNodes <- lapply(pipes,
                            function(p) {
                                newXMLNode("pipe",
                                           newXMLNode("source",
                                                      attrs=p$src),
                                           newXMLNode("destination",
                                                      attrs=p$dst))
                            })
    }
    if (!is.null(modules)) {
        mnames <- names(modules)
        if (!is.null(mnames)) {
            isref <- nchar(mnames)
            mrefs <- ifelse(isref, modules, "")
            mnames <- ifelse(isref, mnames, modules)
        } else {
            mrefs <- NULL
            mnames <- modules
        }
    } else {
        mnames <- NULL
        mrefs <- NULL
    }
    # Add implicit modules from pipes
    pipemnames <- unique(unlist(lapply(pipes,
                                       function(p) {
                                           c(p$src["module"],
                                             p$dst["module"])
                                       })))
    extranames <- pipemnames[!pipemnames %in% mnames]
    mnames <- c(mnames, extranames)
    mrefs <- c(mrefs, rep("", length(extranames)))
    moduleNodes <- mapply(function(name, ref) {
                              if (nchar(ref)) {
                                  attrs <- c(name=name, ref=ref)
                              } else {
                                  attrs <- c(name=name)
                              }
                              newXMLNode("module", attrs=attrs)
                          },
                          mnames, mrefs)        
    addChildren(root,
                kids=c(moduleNodes, pipeNodes))
}

writePipeline <- function(name, ..., dir="XML") {
    pipeline <- pipeline(name, ...)
    saveXML(pipeline, file.path(dir, paste0(name, ".xml")))    
}

require(RBGL)

edges <- function(module, pipes) {
    e <- unique(pipes[, "dstmod"][pipes[, "srcmod"] == module])
    e <- e[!is.na(e)]
    if (!length(e)) {
        list()
    } else {
        list(edges=e)
    }
}

pipelineGraph <- function(modules, pipes) {
    nodes <- modules
    edgeList <- lapply(modules, edges, pipes)
    names(edgeList) <- modules
    new("graphNEL", nodes, edgeList, "directed")
}

readPipe <- function(x) {
    src <- getNodeSet(x, "oa:source",
                      namespaces=c(oa="http://www.openapi.org/2014/"))
    dst <- getNodeSet(x, "oa:destination",
                      namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(src) && length(dst)) {
        src <- src[[1]]
        dst <- dst[[1]]
        c(srcmod=xmlGetAttr(src, "module"),
          srcname=xmlGetAttr(src, "name"),
          dstmod=xmlGetAttr(dst, "module"),
          dstname=xmlGetAttr(dst, "name"))
    } else {
        stop("Invalid pipe")
    }
}

mergeInfo <- function(pipes, modules) {
    # Stack up module info
    outputInfo <- lapply(modules,
                         function(m) {
                             if (is.null(m$outputs)) {
                                 NULL
                             } else {
                                 cbind(m$outputs[, c("name", "type"),
                                                 drop=FALSE],
                                       module=m$name)
                             }
                         })
    moduleInfo <- do.call("rbind", outputInfo)
    # Merge module input/output info with pipe output info
    info <- as.matrix(merge(pipes, moduleInfo, all.x=TRUE,
                            by.x=c("srcmod", "srcname"),
                            by.y=c("module", "name")))
    info[, c("srcmod", "srcname", "dstmod", "dstname", "type"), drop=FALSE]
}

loadModule <- function(x) {
    # If the module only has a 'name', use that to read the module
    # If the module has a 'ref', use that to read the module
    name <- xmlGetAttr(x, "name")
    ref <- xmlGetAttr(x, "ref")
    if (is.null(ref)) {
        readModule(name)
    } else {
        readModule(ref)
    }
}

readPipeline <- function(x) {
    xml <- xmlParse(file.path("XML", paste0(x, ".xml")))
    pipeline <- xmlRoot(xml)
    pipelineName <- x
    # Read the modules
    moduleNodes <- getNodeSet(pipeline, "oa:module",
                              namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(moduleNodes)) {
        moduleNames <- sapply(moduleNodes, xmlGetAttr, "name")
        modules <- lapply(moduleNodes, loadModule)
        names(modules) <- moduleNames
    } else {
        stop("Pipeline has no modules")
    }
    # Read pipe information
    pipeNodes <- getNodeSet(pipeline, "oa:pipe",
                            namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(pipeNodes)) {
        pipes <- do.call("rbind", lapply(pipeNodes, readPipe))
    } else {
        pipes <- NULL
    }
    # Merge module input/output information with pipe information
    # (this gathers all the information necessary to get input/outputs
    #  for a module)
    pipeInfo <- mergeInfo(pipes, modules)
    # Generate graph for pipeline and use that to determine module order
    graph <- pipelineGraph(moduleNames, pipes)
    order <- tsort(graph)
    # Build pipeline object
    result <- list(name=pipelineName,
                   moduleOrder=order,
                   modules=modules,
                   graph=graph,
                   pipes=pipeInfo)
    class(result) <- "pipeline"
    result
}

print.pipeline <- function(x, ...) {
    cat("Name:", x$name, "\n")
    cat(paste("    ", x$moduleOrder), sep="\n")
}

plot.pipeline <- function(x, ...) {
    require(gridGraphviz)
    rag <- agopenTrue(x$graph, "",
                      attrs=list(node=
                          list(shape="ellipse")))
    grid.graph(rag)
}

resolveInputs <- function(modname, pipes, results) {
    if (is.null(results)) {
        return(NULL)
    }
    modpipes <- pipes[pipes[, "dstmod"] == modname, , drop=FALSE]
    inputs <- merge(modpipes, results,
                    by.x=c("srcmod", "srcname", "type"),
                    by.y=c("modname", "name", "type"))
    if (nrow(inputs) == 0) {
        return(NULL)
    }
    result <- inputs[, c("dstname", "type", "ref")]
    names(result) <- c("name", "type", "ref")
    result
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
    pipepath <- file.path("Pipelines", pipename)
    if (file.exists(pipepath))
        unlink(pipepath, recursive=TRUE)
    dir.create(pipepath)
    modfilebase <- file.path(pipepath, "Modules") 

    # Module order has been determined in readPipeline; run in order
    n <- length(x$modules)
    modresults <- NULL
    if (n == 0)
        return()
    for (i in 1:n) {
        m <- x$modules[[x$moduleOrder[i]]]
        # FIXME:  efficiency could be gained by incrementally updating inputs
        #         rather than redoing every time AND by selecting only the
        #         relevant inputs to pass to runModule() each time
        inputs <- resolveInputs(m$name, x$pipes, modresults)
        newresults <- runModule(m, inputs, filebase=modfilebase)
        modresults <- rbind(modresults, newresults)
    }
}

