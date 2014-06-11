
# shell run-time
# Must know about ...
#   running shell commands
# ... BUT THAT IS ALL

# Shell variables are just character values, so share them via text files

saveShellOutput <- function(x, modpath, logfile) {
    name <- as.name(x["name"])
    filename <- file.path(modpath, paste0(name, ".env"))
    type <- as.character(x["type"])
    if (type == "internal") {
        log(paste0("echo ${", name, "} > ", filename), logfile)
        c(x[c("name", "type")], ref=filename)
    } else {
        x
    }
}

loadShellInput <- function(x, logfile) {
    name <- as.character(x["name"])
    filename <- as.character(x["ref"])
    type <- as.character(x["type"])
    if (type == "internal") {
        value <- readLines(filename, n=1)
        log(paste0(name, "=", value), logfile)
    } else {
        log(paste0(name, "=", filename), logfile)
    }
}

evalSource.shell <- function(src, inputs, outputs, modpath) {
    logfile <- file.path(modpath, "script.sh")

    result <- NULL
    
    cmd <- paste0("sh ", logfile)
    log(paste("#", cmd), logfile)
    
    log("# Load module input", logfile)
    if (length(inputs)) {
        apply(inputs, 1, loadShellInput, logfile)
    }

    log("# Set working directory", logfile)
    log(paste0("oldwd=`pwd`"), logfile)
    log(paste0("cd ", modpath), logfile)

    log("# Module source", logfile)
    log(src, logfile)

    log("# Reset working directory", logfile)
    log("cd ${oldwd}", logfile)

    log("# Save module output", logfile)
    if (length(outputs)) {
        result <- t(apply(outputs, 1, saveShellOutput, modpath, logfile))
    }
    
    system(cmd)

    resolveOutput(result, modpath)
}
