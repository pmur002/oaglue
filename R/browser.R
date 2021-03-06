
# Functions to produce web pages of module or pipeline listings for
# easy browsing of available modules and pipelines (?)

filterList <- function(x, element) {
    xml <- xmlParse(x)
    if (xmlName(xmlRoot(xml)) == element)
        xml
    else
        NULL
}

makeList <- function(path, element) {
    p <- suppressWarnings(normalizePath(path))
    files <- list.files(p, pattern="[.]xml$", full.names=TRUE)
    xmlFiles <- lapply(files, filterList, element)
    keep <- !sapply(xmlFiles, is.null)
    list(files=files[keep], xml=xmlFiles[keep])
}

moduleInfo <- function(file, xml) {
    moduleName <- basename(file_path_sans_ext(file))
    modulePath <- dirname(file)
    m <- readXMLModule(xml, moduleName, modulePath)
    c(name=m$name, platform=m$platform, description=m$desc)
}

moduleListInfo <- function(x) {
    do.call("rbind", mapply(moduleInfo, x$files, x$xml, SIMPLIFY=FALSE))
}

moduleTable <- function(paths) {
    pathv <- splitPath(paths)
    fileList <- lapply(pathv, makeList, "module")
    moduleData <- lapply(fileList, moduleListInfo)
    do.call("rbind", moduleData)
}

pipelineInfo <- function(file, xml) {
    pipelineName <- basename(file_path_sans_ext(file))
    pipelinePath <- dirname(file)
    p <- readXMLPipeline(xml, pipelineName, pipelinePath)
    c(name=p$name, description=p$desc)    
}

pipelineListInfo <- function(x) {
    do.call("rbind", mapply(pipelineInfo, x$files, x$xml, SIMPLIFY=FALSE))
}

pipelineTable <- function(paths) {
    pathv <- splitPath(paths)
    fileList <- lapply(pathv, makeList, "pipeline")
    pipelineData <- lapply(fileList, pipelineListInfo)
    do.call("rbind", pipelineData)
}



