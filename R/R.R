
# R run-time
# Must know about ...
#   running R code
#   reading in R objects from file
#   writing R objects to file
# ... BUT THAT IS ALL

saveROutput <- function(x, modpath, logfile) {
    name <- as.name(x["name"])
    filename <- file.path(modpath, paste0(name, ".rds"))
    type <- as.character(x["type"])
    if (type == "internal") {
        logExpr_q(substitute(saveRDS(x, y), list(x=name, y=filename)), logfile)
        c(x[c("name", "type")], ref=filename)
    } else {
        x
    }
}

loadRInput <- function(x, logfile) {
    name <- as.character(x["name"])
    filename <- as.character(x["ref"])
    type <- as.character(x["type"])
    if (type == "internal") {
        logExpr_q(substitute(assign(x, readRDS(y)), list(x=name, y=filename)),
                  logfile)
    } else {
        logExpr_q(substitute(assign(x, y), list(x=name, y=filename)), logfile)
    }
}

evalSource.R <- function(src, inputs, outputs, modpath) {
    logfile <- file.path(modpath, "script.R")

    result <- NULL
    
    cmd <- paste0("Rscript ", logfile)
    log(paste("#", cmd), logfile)

    log("# Work in local environment so that functions are saved with env",
        logfile)
    log("local({", logfile)
    
    log("# Load module input", logfile)
    if (length(inputs)) {
        apply(inputs, 1, loadRInput, logfile)
    }

    log("# Set working directory", logfile)
    logExpr_q(substitute(oldwd <- setwd(x), list(x=modpath)), logfile)

    log("# Module source", logfile)
    log(src, logfile)

    log("# Reset working directory", logfile)
    logExpr(setwd(oldwd), logfile)

    log("# Save module output", logfile)
    if (length(outputs)) {
        result <- t(apply(outputs, 1, saveROutput, modpath, logfile))
    }

    log("}) # End local()", logfile)
    
    system(cmd)

    resolveOutput(result, modpath)
}
