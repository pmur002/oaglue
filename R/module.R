
input <- function(name, type="internal", format="") {
    c(name=name, type=type, format=format)
}

output <- function(name, type="internal", format="", ref=NULL, path=NULL) {
    result <- c(name=name, type=type, format=format)
    if (!is.null(ref)) {
        if (type == "internal")
            stop("'ref' must not be specified for internal output")
        result <- c(result, ref=ref)
        if (!is.null(path)) {
            result <- c(result, path=path)
        }
    }
    result
}

src <- function(src=NULL, ref=NULL, path=NULL, order=NULL) {
    # Need exactly one of 'src' or 'ref'
    if ((is.null(src) && is.null(ref)) ||
        (!is.null(src) && !is.null(ref)))
        stop("Invalid source description")
    if (is.null(src)) 
        src <- ""
    result <- c(src=src)
    if (!is.null(ref)) {
        result <- c(result, ref=ref)
        if (!is.null(path)) {
            result <- c(result, path=path)
        }
    }
    if (!is.null(order)) {
        result <- c(result, order=order)
    }
    result
}

module <- function(name, platform, 
                   inputs=NULL, outputs=NULL,
                   src=NULL, desc=NULL) {
    doc <- newXMLDoc(namespaces="http://www.openapi.org/2014/",
                     node=newXMLNode("module", attrs=c(version="0.1"),
                         namespaceDefinitions="http://www.openapi.org/2014/"))
    root <- xmlRoot(doc)
    if (!is.null(inputs)) {
        if (is.atomic(inputs))
            inputs <- list(inputs)
        inputs <- lapply(inputs,
                         function(i) {
                             newXMLNode("input", attrs=i)
                         })
    }
    if (!is.null(outputs)) {
        if (is.atomic(outputs))
            outputs <- list(outputs)
        outputs <- lapply(outputs,
                          function(i) {
                              newXMLNode("output", attrs=i)
                          })
    }
    if (!is.null(src)) {
        if (is.atomic(src))
            src <- list(src)
        src <- lapply(src,
                      function(i) {
                          src <- i["src"]
                          newXMLNode("source",
                                     newXMLCDataNode(src),
                                     attrs=i[-1]) # i[-"src"]
                      })
    }
    if (!is.null(desc)) {
        desc <- newXMLNode("description",
                           newXMLCDataNode(desc))
    }
    platform <- newXMLNode("platform",
                           attrs=c(name=platform))
    addChildren(root,
                kids=list(platform, desc, inputs, outputs, src))
    doc
}

writeModule <- function(name, ..., dir="XML") {
    module <- module(name, ...)
    saveXML(module, file.path(dir, paste0(name, ".xml")))    
}

readSource <- function(x) {
    ref <- xmlGetAttr(x, "ref")
    path <- xmlGetAttr(x, "path")
    if (is.null(ref)) {
        sourceValue <- xmlValue(x)
    } else {
        if (absPath(ref)) {
            file <- ref
        } else {
            if (is.null(path)) {
                file <- findFile(ref)
            } else {
                file <- findFile(ref, path)
            }
            if (is.null(file)) 
                warning("Module source not found")
        }
        sourceValue <- readRef(file)
    }
    sourceValue
}

readInput <- function(x) {
    content <- c(name=xmlGetAttr(x, "name"),
                 type=xmlGetAttr(x, "type"),
                 format=xmlGetAttr(x, "format"))
    content
}

readOutput <- function(x) {
    type <- xmlGetAttr(x, "type")
    content <- c(name=xmlGetAttr(x, "name"),
                 type=type,
                 format=xmlGetAttr(x, "format"))
    ref <- xmlGetAttr(x, "ref")
    path <- xmlGetAttr(x, "path")
    if (is.null(ref)) {
        if (type == "external")
            stop("'ref' must be specified for external output")
        content <- c(content)
    } else {
        if (type == "internal")
            stop("'ref' must not be specified for internal output")
        content <- c(content, ref=ref)
        if (!is.null(path)) {
            content <- c(content, path=path)
        }
    }
    content
}

sourceOrder <- function(sourceNodes) {
    order <- lapply(sourceNodes, xmlGetAttr, "order")
    nullorder <- sapply(order, is.null)
    unordered <- which(nullorder)
    result <- unordered
    if (length(unordered) < length(order)) {
        ordered <- which(!nullorder)
        orderNum <- as.numeric(order[ordered])
        orderedOrdered <- ordered[order(orderNum)]
        nonPos <- sort(orderNum) <= 0
        pos <- sort(orderNum) > 0
        if (any(nonPos)) {
            result <- c(orderedOrdered[nonPos], result)
        }
        if (any(pos)) {
            result <- c(result, orderedOrdered[pos])
        }
    }
    result
}

stackOutputs <- function(x) {
    maxLength <- max(sapply(x, length))
    regular <- lapply(x,
                      function(y, max) {
                          n <- length(y)
                          if (n < max) {
                              y <- c(y, rep(NA, max - n))
                          }
                          y
                      },
                      maxLength)
    do.call("rbind", regular)
}

readXMLModule <- function(x, name) {
    module <- xmlRoot(x)

    version <- xmlGetAttr(module, "version")
    
    platformNode <- getNodeSet(module, "oa:platform",
                               namespaces=c(oa="http://www.openapi.org/2014/"))
    platformName <- xmlGetAttr(platformNode[[1]], "name")

    descNodes <- getNodeSet(module, "oa:description",
                            namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(descNodes)) {
        descValue <- xmlValue(descNodes[[1]])
    } else {
        descValue <- ""
    }
                            
    sourceNodes <- getNodeSet(module, "oa:source",
                              namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(sourceNodes)) {
        order <- sourceOrder(sourceNodes)    
        sourceValue <- unlist(lapply(sourceNodes[order], readSource))
    } else {
        sourceValue <- ""
    }
    
    inputNodes <- getNodeSet(module, "oa:input",
                             namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(inputNodes)) {
        inputs <- do.call("rbind", lapply(inputNodes, readInput))
        rownames(inputs) <- inputs[, "name"]
    } else {
        inputs <- NULL
    }

    outputNodes <- getNodeSet(module, "oa:output",
                             namespaces=c(oa="http://www.openapi.org/2014/"))
    if (length(outputNodes)) {
        outputs <- stackOutputs(lapply(outputNodes, readOutput))
        rownames(outputs) <- outputs[, "name"]
    } else {
        outputs <- NULL
    }

    result <- list(name=name,
                   version=version,
                   platform=platformName,
                   desc=descValue,
                   src=sourceValue,
                   inputs=inputs,
                   outputs=outputs)
    class(result) <- "module"
    result
}

readModule <- function(x, path="XML") {
    name <- paste0(x, ".xml")
    if (absPath(name)) {
        file <- name
    } else{
        file <- findFile(name, path)
        if (is.null(file))
            stop("Unable to find module")
    }

    moduleName <- basename(file_path_sans_ext(file))

    txt <- readRef(file)
    xml <- xmlParse(txt, asText=TRUE)
    readXMLModule(xml, moduleName)
}

print.module <- function(x, ...) {
    cat("Name:", x$name, "\n")
    if (!is.null(x$inputs)) {
        cat("  Inputs:", paste(paste0(x$inputs[, "name"],
                                      " (", x$inputs[, "format"], ")"),
                               collapse=", "), "\n")
    }
    if (!is.null(x$outputs)) {
        cat("  Outputs:", paste(paste0(x$outputs[, "name"],
                                       " (", x$outputs[, "format"], ")"),
                                collapse=", "), "\n")
    }
}

# Given a module (which includes its required inputs)
# AND the result of running a module
# AND pipe information,
# match inputs to results
# (to provide the information on inputs that are required
#  to run the module)
composeInputs <- function(x, results, pipeInfo) {
    start <- merge(results, pipeInfo,
                   by.x=c("modname", "name"),
                   by.y=c("startmod", "startname"))
    end <- merge(cbind(modname=x$name, x$inputs),
                 start[, c("ref", "endmod", "endname", "type")],
                 by.x=c("modname", "name", "type"),
                 by.y=c("endmod", "endname", "type"))
    end[, c("name", "type", "ref")]
}

evalSource <- function(src, inputs, outputs, modpath) {
    UseMethod("evalSource")
}

runModule <- function(x, inputs=NULL, filebase="./Modules") {
    # 'x' may be just module name for convenience
    if (!inherits(x, "module") && is.character(x))
        x <- readModule(x)
    
    # create a directory for module output
    if (!file.exists(filebase)) {
        dir.create(filebase)
    }
    modname <- x$name
    modpath <- file.path(filebase, modname)
    if (file.exists(modpath))
        unlink(modpath, recursive=TRUE)
    dir.create(modpath)

    src <- x$src
    class(src) <- x$platform
    
    result <- evalSource(src, inputs, x$outputs, modpath)

    if (is.null(result)) {
        result
    } else {
        cbind(modname=modname, result)
    }
}

# Convenience function to generate module, and run it to generate output,
# and return reference to output to use as input for runModule()
makeInput <- function(name, platform, src=NULL) {
    writeModule(name, platform, src=src(src),
                outputs=output(name, "internal"))
    runModule(name)
}

