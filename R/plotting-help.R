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
##' @export
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
##' @export
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
##' @export
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
##' @export
ax.psdlab <- function(label = "Power spectral density",
                      v.unit = "\u2030", t.unit = "yr",
                      font = par()$font.axis) {

    expr <- bquote(.(label) * " (" * .(v.unit)^{"2"} ~ .(t.unit) * ")")
    lab <- bquote.font(expr, font = font)

    return(lab)

}

##' Add error bars
##'
##' Add error bars to an existing point plot.
##' @param x x coordinates of the points for which error bars are to be added.
##' @param y y coordinates of the points for which error bars are to be added.
##' @param upper upper value of the error interval with respect to \code{y}.
##' @param lower lower value of the error interval with respect to \code{y};
##' default is to assume symmetric error intervals.
##' @param width the width of the error bar drawing as a fraction of the x user
##' coordinate range (\code{diff(range(par("usr")[1 : 2]))}).
##' @param col the colour of the error bars; default "black".
##' @param lwd the line width of the error bars; default \code{1}.
##' @param lty the line type of the error bars; default are solid lines
##' (\code{lty = 1}).
##' @author Thomas Münch
##' @examples
##' x <- 1 : 10
##' plot(x); ErrorBars(x, y = x, upper = 2.5, lower = 1)
##' @export
ErrorBars <- function(x, y, upper, lower = upper, width = 0.05,
                      col = "black", lwd = 1, lty = 1) {

    if (length(x) != length(y)) stop("x and y lengths differ.")

    usr <- par("usr")
    w <- width * diff(range(usr[1 : 2]))
    width <- w / 2

    segments(x, y - lower, x,y + upper, col = col,
             lwd = lwd, lty = lty, lend = 1)
    segments(x - width, y - lower, x + width, y - lower, col = col,
             lwd = lwd, lty = lty, lend = 1)
    segments(x - width, y + upper, x + width, y + upper, col = col,
             lwd = lwd, lty = lty, lend = 1)

}

##' Relative histograms
##'
##' Compute and plot relative histograms with respect to the total or the
##' maximum of the counts.
##' @param max.normalize if \code{TRUE}, normalize the counts to their maximum
##' value; else normalize with respect to the sum of the counts (the default).
##' @param add logical. If ‘TRUE’, only the bars are added to the current plot.
##' @inheritParams graphics::hist
##' @param ... further arguments passed on to \code{plot}.
##' @return Invisibly, the resulting object of class \code{"histogram"}.
##' @seealso \code{\link{hist}}
##' @author Thomas Münch
##' @examples
##' x <- rnorm(1000)
##'
##' # **counts** sum up to one:
##' h <- rel.hist(x)
##' sum(h$counts)
##' # per default, histogram **area** sums up to one:
##' h <- hist(x, plot = FALSE)
##' plot(h, freq = FALSE)
##' sum(h$density) * diff(h$breaks)[1]
##'
##' # counts normalized to maximum value:
##' rel.hist(x, max.normalize = TRUE)
##' @export
rel.hist <- function(x, breaks = "Sturges",
                     max.normalize = FALSE, add = FALSE, ...) {

    h <- hist(x, breaks = breaks, plot = FALSE)

    if (max.normalize) {
        norm <- max(h$counts)
    } else {
        norm <- sum(h$counts)
    }

    h$counts <- h$counts / norm
    plot(h, freq = TRUE, add = add, ...)

    invisible(h)

}

##' Add plot session information
##'
##' Add session information to an existing plot: script name, date of plot
##' creation, etc.
##' @param script the name of the plot creating script.
##' @param add.info additional information to be added (optional).
##' @param incr fraction of the user coordinate range to use as incremental
##' distance from the default position of the information text in the upper
##' right corner of the plot region.
##' @author Thomas Münch
##' @examples
##' plot(1 : 10, type = "l")
##' PlotStamp(script = "interactive",
##'           add.info = "Illustrated usage of PlotStamp function")
##' @export
PlotStamp <- function(script = "", add.info = NULL, incr = 0.01) {

    if (is.null(add.info)) {
        info <- sprintf("Date: %s\nsrc: %s", Sys.time(), script)
    } else {
        info <- sprintf("Date: %s\nsrc: %s\nInfo:\n%s.",
                        Sys.time(), script, add.info)
    }

    usr <- par("usr")
    add <- incr * c(diff(range(usr[1 : 2])), diff(range(usr[3 : 4])))
    text(usr[1] + add[1], usr[4] - add[2], info, adj = c(0, 1),
         cex = 0.75 * par("cex"))

}

##' Default color palette
##'
##' Wrapper to obtain my default colour palette for filled contour plots, etc.
##' @param RCB.pal a palette name from the RColorBrewer lists; defaults to the
##' diverging \code{RdYlBu} palette.
##' @param n number of different colours in the palette, minimum 3, maximum
##' depending on the palette. Default is \code{10} for \code{RdYlBu}.
##' @return a function that takes an integer argument (the required number of
##' colours) and returns a character vector of colours.
##' @author Thomas Münch
##' @seealso \code{\link{colorRampPalette}};
##' \code{\link{RColorBrewer::brewer.pal}}
##' @export
ColorPal <- function(RCB.pal = "RdYlBu", n = 10) {

    require(RColorBrewer)
    pal <- colorRampPalette(rev(RColorBrewer::brewer.pal(n, RCB.pal)))
}

