##' Load graphical parameters.
##'
##' This function returns a list of the graphical parameters specified as its
##' function arguments, which can then be set via a call to \code{par}. Default
##' function parameters are my default plotting parameters; different or
##' additional parameters can be specified via \code{...}. This wrapper function
##' provides a convenient way to set new graphical parameters and save their old
##' values for later restoring at the same time; see the example. See
##' \code{?par} for information on the individual parameters.
##' @return A list of graphical parameters to be used with \code{par()};
##' i.e. per default:
##' \itemize{
##'   \item mar = c(5, 5, 0.5, 0.5)
##'   \item lwd = 1
##'   \item las = 1
##'   \item font.lab = 1
##'   \item font.axis = 1
##'   \item cex.main = 1.5
##'   \item cex.lab = 1.5
##'   \item cex.axis = 1.25
##' }
##' @author Thomas Münch
##' @seealso \code{\link{par}}
##' @examples
##' op <- par(LoadGraphicsPar())
##' plot(1 : 10, main = "Example plot",
##'      xlab = "X title", ylab = "Y title", type = "l")
##' par(op)
##' @export
LoadGraphicsPar <- function(mar = c(5, 5, 0.5, 0.5), lwd = 1, las = 1,
                            font.lab = 1, font.axis = 1, cex.main = 1.5,
                            cex.lab = 1.5, cex.axis = 1.25, ...) {

    par <- c(as.list(environment()), list(...))
    return(par)

}
