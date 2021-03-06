
.OA.global <- new.env()

assign("options",
       list(logDir=".",
            logFile=".oa.log"),
       .OA.global)

initWD <- function() {
    assign("wd", getwd(), .OA.global)
}

getWD <- function() {
    get("wd", .OA.global)
}

initPath <- function() {
    assign("path", ".//|${ROOT}//", .OA.global)
}

getPath <- function() {
    get("path", .OA.global)
}

# Logging information
defaultLogFile <- function() {
    options <- get("options", .OA.global) 
    file.path(options$logDir, options$logFile)
}

initLog <- function(logFile=defaultLogFile()) {
    cat("", file=logFile)
}

log <- function(x, logFile=defaultLogFile()) {
    cat(paste(x, collapse="\n"), "\n", file=logFile, append=TRUE)
}

logExpr_q <- function(x, logFile=defaultLogFile()) {
    log(deparse(x), logFile)
}

logExpr <- function(x, logFile=defaultLogFile()) {
    log(deparse(substitute(x)), logFile)
}

initOA <- function() {
    initLog()
    initWD()
    initPath()
}

fileExists <- function(x) {
    UseMethod("fileExists")
}

fileExists.plain <- function(x) {
    file.exists(x)
}

fileExists.http <- function(x) {
    require("RCurl")
    url.exists(x)
}

fileExists.https <- fileExists.http

refExists <- function(x) {
    ft <- fileType(x)
    class(x) <- c(ft, class(x))
    fileExists(x)
}

resolve <- function(x, modpath) {
    ref <- x["ref"]
    if (absPath(ref)) {
        if (!refExists(ref))
            stop(paste("Reference does not exist:", ref))
    } else {
        path <- x["path"]
        if (is.na(path)) {
            foundRef <- findFile(ref, cd=modpath)
        } else {
            foundRef <- findFile(ref, path, cd=modpath)
        }
        if (is.null(foundRef)) {
            stop(paste("Failed to resolve reference:", ref))
        } else {
            ref <- foundRef
        }
    }
    x["ref"] <- ref
    x
}

resolveOutput <- function(x, modpath) {
    if (is.null(x)) {
        NULL
    } else {
        t(apply(x, 1, resolve, modpath))
    }
}

nameFromFilename <- function(x) {
    basename(file_path_sans_ext(x))
}
