##' Open quartz device
##'
##' Wrapper to open a quartz device with my default dimensions and graphical
##' parameters for on-screen plotting or saving to a file.
##' @param file path to a file for storing a hardcopy of the plot including a
##' supported file extension to set the \code{type} of output (e.g. ".pdf" or
##' ".png"); i.e. the function extracts the extension from \code{file} and uses
##' it as the \code{type} argument for the call to
##' \code{\link{quartz}}. Defaults to \code{NULL} for on-screen plotting.
##' @param type the type of output to use. Defaults to \code{"native"} for
##' on-screen plotting. If \code{file} is not \code{NULL}, \code{type} is
##' determined from its extension.
##' @param height the height of the plotting area in inches.  Default ‘6’.
##' @param width the width of the plotting area in inches.  Default ‘8’.
##' @param ... further arguments passed on to \code{\link{quartz}}.
##' @seealso \code{\link{quartz}}
##' @examples
##'
##' # Create an empty on-screen quartz device
##' Quartz()
##'
##' # Store empty plot in pdf format in local directory
##' \dontrun{
##' Quartz(file = file.path(getwd(), "test-quartz.pdf"))
##' dev.off()
##' }
##' @author Thomas Münch
##' @export
Quartz <- function(file = NULL, type = "native",
                   height = 6, width = 8, ...) {

    # Determine file type from file extension
    if (!is.null(file)) {

        type <- tools::file_ext(file)
        if (nchar(type) == 0)
            stop("No file extension found for setting 'type'.")
        
    }

    # Set default graphics parameters
    plot.par <- LoadGraphicsPar()

    # Open device
    quartz(height = height, width = width, file = file, type = type, ...)
    par(plot.par)

}
