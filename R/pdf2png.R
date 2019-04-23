##' pdf to png conversion
##'
##' Convert an existing pdf file to a png file using the Mac OS X command line
##' tool \code{sips}.
##' @param file the root file name of the pdf file to convert, i.e. the filename
##' without the \code{.pdf} extension.
##' @return the default output from \code{\link{system}}.
##' @author Thomas Münch
##' @source https://robservatory.com/use-sips-to-quickly-easily-and-freely-convert-image-files/
##' @export
pdf2png <- function(file) {

    cmd <- sprintf("sips -s format png %s --out %s",
                   paste0(file, ".pdf"), paste0(file, ".png"))
    
    system(cmd)

}

