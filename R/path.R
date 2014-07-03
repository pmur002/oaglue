
pathsep <- "|"

# Is a path absolute?
absPath <- function(x) {
    # If find ":" in path then either ...
    # ... a Windows path starting with volume name
    # ... or a URI, which implies an absolute path (within the URI "authority")
    # OR if find "/" at the start then it is absolute
    # OTHERWISE it is relative
    grepl(":|^/", x)
}

# Find a resource given a starting directory,
# searching recursively if desired
# FIXME:  This is REALLY slow and brute force, just to get it going

# Split set of paths into separate paths
splitPath <- function(x) {
    strsplit(x, pathsep, fixed=TRUE)[[1]]
}

# Expand variables and 
expandPath <- function(x, cd) {
    expanded <- gsub("${ROOT}", getWD(), x, fixed=TRUE)
    oldwd <- setwd(cd)
    on.exit(setwd(oldwd))
    # Suppress warnings so we don't hear about missing paths
    suppressWarnings(normalizePath(expanded))
}

# Combine the path with the default glue system path
combinePath <- function(x) {
    # If path starts or ends with |, then append or prepend
    append <- grepl(paste0("^[", pathsep, "]"), x)
    prepend <- grepl(paste0("[", pathsep, "]$"), x)
    if (append && prepend)
        stop("Invalid path value")
    if (append) {
        result <- paste0(getPath(), x)
    } else {
        if (prepend) {
            result <- paste0(x, getPath())
        } else {
            result <- x
        }
    }
    result
}

findFileInPath <- function(x, path, recurse) {
    files <- list.files(path, full.names=TRUE, include.dirs=TRUE,
                        recursive=recurse)
    matches <- grep(paste0(x, "$"), files)
    nm <- length(matches)
    if (nm > 0) {
        if (nm > 1)
            warning(paste(paste("More than one matching resource for", x),
                          files[matches], collapse="\n"))
        files[matches[1]]
    } else {
        NULL
    }
}

findFile <- function(x, paths=NULL, cd=".") {
    if (absPath(x))
        return(x)
    if (is.null(paths)) {
        paths <- getPath()
    } else {
        paths <- combinePath(paths)
    }
    pathv <- splitPath(paths)
    recurse <- grepl("//$", pathv)
    fpaths <- unique(sapply(pathv, expandPath, cd))
    result <- NULL
    while (length(fpaths) && is.null(result)) {
        result <- findFileInPath(x, fpaths[1], recurse[1])
        fpaths <- fpaths[-1]
        recurse <- recurse[-1]
    }
    result
}
