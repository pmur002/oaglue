
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

resolve <- function(x, modpath) {
    ref <- suppressWarnings(normalizePath(x["ref"]))
    if (!absPath(ref)) {
        path <- x["path"]
        if (is.na(path)) {
            ref <- findFile(ref, cd=modpath)
        } else {
            ref <- findFile(ref, path, cd=modpath)
        }
    }
    c(x[c("name", "type")], ref=ref)
}

resolveOutput <- function(x, modpath) {
    if (is.null(x)) {
        NULL
    } else {
        t(apply(x, 1, resolve, modpath))
    }
}
