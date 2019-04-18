##' Source scripts from directory.
##'
##' Source all \code{R}-related scripts from  a given directory.
##'
##' The function looks for all files with extension matching \code{R}, \code{r},
##' \code{S}, \code{s}, \code{Q} and \code{q} in the given directory and sources
##' them with \code{source()}.
##' @param path the path to the directory to source. 
##' @param trace if \code{TRUE}, the names of the sourced files are printed to
##' the terminal in order to facilitate tracing of errors upon sourcing.
##' @inheritParams base::source
##' @param ... further arguments passed on to \code{source}.
##' @seealso \code{\link{source}}
##' @source see the respective example in \code{?source}.
##' @author Thomas Münch
sourceDir <- function(path, trace = TRUE, local = FALSE, ...) {
    
    cat("Sourcing files...\n")
    
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        
        if (trace) cat(nm, ":")
        source(file.path(path, nm), local = local, ...)
        if (trace) cat("\n")
    }
}

