
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
    files <- list.files(p, pattern="[.]xml$", full=TRUE)
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
    p <- readXMLPipeline(xml, pipelineName)
    c(name=p$name, desc=p$desc)    
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

pipelineHTML <- function(file, xml, outdir, outfile) {
    pipelineName <- basename(file_path_sans_ext(file))
    p <- readXMLPipeline(xml, pipelineName)
    graphName <- paste0(pipelineName, ".svg")
    graphFile <- file.path(outdir, graphName)
    rag <- agopenTrue(p$graph, "", attrs=list(node=list(shape="ellipse")))
    pdf(NULL, width=graphWidth(rag), height=graphHeight(rag))
    grid.graph(rag)
    grid.export(graphFile)
    dev.off()
    cat(paste('<tr><td style="border-style: solid; border-width: 1px">',
              pipelineName,
              '</td><td style="border-style: solid; border-width: 1px">',
              p$desc,
              '</td><td style="border-style: solid; border-width: 1px">',
              "<img src='", graphName, "'/>",
              "</tr>"),
        file=file.path(outdir, outfile), append=TRUE)
}

pipelineToHTML <- function(x, dir, file) {
    # Zero the file
    writeLines("", file.path(dir, file))
    cat('<table>', file=file.path(dir, file), append=TRUE)
    mapply(pipelineHTML, x$files, x$xml, MoreArgs=list(dir, file))
    cat("</table>", file=file.path(dir, file), append=TRUE)
}

pipelineHTMLTable <- function(paths, file="pipelines.html", dir="HTML") {
    require(gridGraphviz)
    require(gridSVG)
    if (!file.exists(dir))
        dir.create(dir)
    pathv <- splitPath(paths)
    fileList <- lapply(pathv, makeList, "pipeline")
    lapply(fileList, pipelineToHTML, dir, file)
}

# Example usage ...
#  library(googleVis)
#  plot(gvisTable(as.data.frame(moduleTable("XML"))))

