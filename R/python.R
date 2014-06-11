
savePythonOutput <- function(x, modpath, logfile) {
    name <- as.name(x["name"])
    filename <- file.path(modpath, paste0(name, ".pickle"))
    type <- as.character(x["type"])
    if (type == "internal") {
        log(paste0("with open('", filename, "', 'wb') as f:"), logfile)
        log(paste0("\tpickle.dump(", name, ", f)"), logfile)
        c(x[c("name", "type")], ref=filename)
    } else {
        x
    }
}

loadPythonInput <- function(x, logfile) {
    name <- as.character(x["name"])
    filename <- as.character(x["ref"])
    type <- as.character(x["type"])
    if (type == "internal") {
        log(paste0("with open('", filename, "', 'rb') as f:"), logfile)
        log(paste0("\t", name, " = pickle.load(f)"), logfile)
    } else {
        log(paste0(name, " = '", filename, "'"), logfile)
    }
}

evalSource.python <- function(src, inputs, outputs, modpath) {
    logfile <- file.path(modpath, "script.py")

    result <- NULL

    cmd <- paste0("python ", logfile)
    log(paste("#", cmd), logfile)

    log("import os", logfile)
    log("import pickle", logfile)
    
    log("# Load module input", logfile)
    if (length(inputs)) {
        apply(inputs, 1, loadPythonInput, logfile)
    }

    log("# Set working directory", logfile)
    log("oldwd = os.getcwd()", logfile)
    log(paste0("os.chdir('", modpath, "')"), logfile)

    log("# Module source", logfile)
    log(src, logfile)

    log("# Reset working directory", logfile)
    log("os.chdir(oldwd)", logfile)

    log("# Save module output", logfile)
    if (length(outputs)) {
        result <- t(apply(outputs, 1, savePythonOutput, modpath, logfile))
    }
    
    system(cmd)

    resolveOutput(result, modpath)
}
