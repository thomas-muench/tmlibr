##
## collection of little helper functions for base R plotting
##

##' Font selection for bquote
##'
##' A wrapper function for \code{\link{bquote}} to select text font according to
##' the function argument \code{font}.
##' @param expr A language object.
##' @param font An integer which specifies which font to use for text; see
##' \code{?par} for the available options.
##' @return A language object.
##' @author Thomas Münch
bquote.font <- function(expr, font = 1) {

    if (!is.numeric(font) | is.na(match(font, 1 : 5))) {
        stop("Unknown font setting.")
    }

    if (font == 1) {
        res <- bquote(plain(.(expr)))
    } else if (font == 2) {
        res <- bquote(bold(.(expr)))
    } else if (font == 3) {
        res <- bquote(italic(.(expr)))
    } else if (font == 4) {
        res <- bquote(bolditalic(.(expr)))
    } else if (font == 5) {
        res <- bquote(symbol(.(expr)))
    }

    return(res)
}

##' Isotope delta notation label
##'
##' Provides the axis label for isotopic composition in delta notation and units
##' of permil.
##'
##' If \code{x} is set to "oxy" or "dtr", axis labels are formatted according to
##' the delta notation for oxygen or deuterium isotopic composition,
##' respectively; else, \code{x} is used directly as the axis label variable.
##' @param x either a character string to set the isotopic species, or a general
##' character string to use as axis label; see details. Defaults to oxygen
##' isotopic composition.
##' @param font An integer which specifies which font to use for text. Defaults
##' to the axis font currently set by \code{par}; see \code{?par} for the
##' available options.
##' @return A language object with the axis label.
##' @author Thomas Münch
##' @seealso \code{\link{bquote}}
##' @examples
##' plot(1 : 10, xlab = ax.isolab(), ylab = ax.isolab("dtr"))
##' plot(1 : 10, xlab = ax.isolab(font = 2),
##'              ylab = ax.isolab("d-excess", font = 2))
ax.isolab <- function(x = "oxy", font = par()$font.axis) {

    iso <- NULL

    if (x == "oxy") {

        iso <- c("18", "O")
        
    } else if (x == "dtr") {

        iso <- c("2", "H")

    }

    permil <- " (\u2030)"

    if (!length(iso)) {

        expr <- bquote(.(x) * .(permil))
        lab <- bquote.font(expr, font = font)

    } else {

        expr <- bquote(delta^{.(iso[1])} * .(iso[2]) * .(permil))
        lab <- bquote.font(expr, font = font)
    }

    return(lab)

}

##' Temperature axis label
##'
##' Provides the axis label for temperature variables in units of degree
##' Celsius.
##' @param label the label of the temperature variable, defaults to
##' "Temperature".
##' @inheritParams ax.isolab
##' @return A language object with the axis label.
##' @author Thomas Münch
##' @seealso \code{\link{bquote}}
##' @examples
##' plot(1 : 10, xlab = ax.celsiuslab(),
##'              ylab = ax.celsiuslab("Sea surface temperature", font = 2))
ax.celsiuslab <- function(label = "Temperature", font = par()$font.axis) {

    expr <- bquote(.(label) * " (" * degree * "C)")
    lab <- bquote.font(expr, font = font)

    return(lab)

}

##' Frequency axis label
##'
##' Provides the frequency axis label for spectral plots in optional units.
##' @param label the label for the frequency axis; defaults to "Frequency" for
##' \code{inverse = FALSE}, else to "Time period".
##' @param unit the time unit of the corresponding frequency axis; defaults to
##' years ("yr").
##' @param inverse logical; if \code{TRUE}, use inverse frequency units,
##' i.e. time periods.
##' @inheritParams ax.isolab
##' @return A language object with the axis label.
##' @author Thomas Münch
##' @examples
##' plot(1 : 10, xlab = ax.freqlab(),
##'              ylab = ax.freqlab(unit = "s", inverse = TRUE, font = 2))
ax.freqlab <- function(label = if (!inverse) "Frequency" else "Time period",
                       unit = "yr", font = par()$font.axis, inverse = FALSE) {

    if (inverse) {
        expr <- bquote(.(label) * " (" * .(unit) * ")")
    } else {
        expr <- bquote(.(label) * " (" * .(unit)^{"-1"} * ")")
    }

    lab <- bquote.font(expr, font = font)

    return(lab)

}
    
##' Power spectral density axis label
##'
##' Provides the power spectral density axis label for spectral plots in
##' optional units.
##' @param label the label for the spectrum axis; defaults to "Power spectral
##' density".
##' @param v.unit the unit of the spectral variable; defaults to permil.
##' @param t.unit the frequency unit of the spectrum; defaults to years ("yr").
##' @inheritParams ax.isolab
##' @return A language object with the axis label.
##' @author Thomas Münch
##' @examples
##' plot(1 : 10, xlab = ax.freqlab(), ylab = ax.psdlab())
ax.psdlab <- function(label = "Power spectral density",
                      v.unit = "\u2030", t.unit = "yr",
                      font = par()$font.axis) {

    expr <- bquote(.(label) * " (" * .(v.unit)^{"2"} ~ .(t.unit) * ")")
    lab <- bquote.font(expr, font = font)

    return(lab)

}
