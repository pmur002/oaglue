
saveFileSystemOutput <- function(x) {
    x
}

evalSource.fileSystem <- function(src, inputs, outputs, modpath) {
    if (length(inputs)) {
        stop("fileSystem platform does not accept inputs")
    }

    if (length(outputs)) {
        if (any(outputs[, "type"] == "internal"))
            stop("fileSystem platform does not produce internal outputs")
        result <- t(apply(outputs, 1, saveFileSystemOutput))
    }
    
   resolveOutput(result, modpath)
}
