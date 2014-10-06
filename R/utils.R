# Need to be able to read in rasters in-memory
# Expects a base64 encoded image string, returns a native raster (or NULL)
readBase64Image <- function(x) {
    # Cairo only uses png, jpeg, or URI references
    # ignore URI references
    pngHeader <- "data:image/png;base64,"
    jpegHeader <- "data:image/jpeg;base64,"
    header <-
        if (substring(x, 1, nchar(pngHeader)) == pngHeader)
            pngHeader
        else if (substring(x, 1, nchar(jpegHeader)) == jpegHeader)
            jpegHeader
        else
            NULL # Give up, could be a URI
    if (is.null(header))
        return(NULL)
    base64Data <- substring(x, nchar(header) + 1, nchar(x))
    if (header == pngHeader)
        readPNG(base64decode(base64Data), native = TRUE)
    else
        readJPEG(base64decode(base64Data), native = TRUE)
}

svgStyleToList <- function(styleAttr, defs) {
    if (is.null(styleAttr))
        return(list())
    attrs <- strsplit(styleAttr, ";")[[1]]
    attrs <- attrs[nzchar(attrs)]
    attrs <- unlist(strsplit(attrs, " "))
    splitAttrs <- strsplit(attrs[nzchar(attrs)], ":")
    gparNames <- sapply(splitAttrs,
        function(x) {
            switch(x[1],
                   fill = "fill",
                   "font-family" = "fontfamily",
                   "fill-opacity" = "fillAlpha",
                   "font-style" = "fontstyle",
                   "font-size" = "fontsize",
                   "font-weight" = "fontweight",
                   opacity = "alpha",
                   stroke = "col",
                   "stroke-dasharray" = "lty",
                   "stroke-linecap" = "lineend",
                   "stroke-linejoin" = "linejoin",
                   "stroke-miterlimit" = "linemitre",
                   "stroke-opacity" = "colAlpha",
                   "stroke-width" = "lwd",
                   x[1])
        })
    gpars <- lapply(splitAttrs, function(x) {
        # Attempt to cast to numeric, if NA, must be char
        num <- suppressWarnings(as.numeric(x[2]))
        if (! is.na(num)) {
            num
        } else {
            # Switch a few SVG values to grid parlance
            switch(x[2],
                   nonzero = "winding",
                   x[2])
        }
    })

    names(gpars) <- gparNames

    if ("lty" %in% names(gpars))
        gpars$lty <- parseLty(gpars$lty)

    # Also change all of the references to gradient or pattern
    # fills/strokes to patternFill="" or gradientStroke="", etc
    if ("col" %in% names(gpars) && is.urlref(gpars$col)) {
        id <- urlToID(gpars$col)
        colType <- class(getDef(defs, id))
        if (colType == "PicturePattern")
            gpars$patternStroke <- id
        else
            gpars$gradientStroke <- id
        gpars$col <- NULL
    }
    if ("fill" %in% names(gpars) && is.urlref(gpars$fill)) {
        id <- urlToID(gpars$fill)
        fillType <- class(getDef(defs, id))
        if (fillType == "PicturePattern")
            gpars$patternFill <- id
        else
            gpars$gradientFill <- id
        gpars$fill <- NULL
    }

    gpars
}

parseLty <- function(x) {
    if (x == "none")
        1 # solid
    else
        as.numeric(strsplit(x, ",")[[1]])
}

# Pull out all of the gpar settings
svgStyleListToGpar <- function(style) {
    style <- resolveColours(style)
    gparNames <- c("alpha", "cex", "col", "fill", "font", "fontfamily",
                   "fontsize", "lex", "lineend", "lineheight",
                   "linejoin", "linemitre", "lty", "lwd",
                   "gradientFill", "gradientStroke",
                   "patternFill", "patternStroke")
    styleNames <- names(style)
    gparInds <- which(styleNames %in% gparNames)
    if (! length(gparInds))
        return(gpar())
    else {
        # Fix up broken spelling
        gparSub <- style[gparInds]
        ljoinInd <- which("linejoin" == names(gparSub))
        if (length(ljoinInd) && gparSub[[ljoinInd]] == "miter")
            gparSub[[ljoinInd]] <- "mitre"
        do.call("gpar", gparSub)
    }
}

# Turn fill-opacity & fill, along with stroke-opacity and stroke from
# [0,1] + rgb(%,%,%) into a hex quad, i.e. #RRGGBBAA
resolveColours <- function(x) {
    rgba2col <- function(col, alpha) {
        if (col == "none")
            return("#FFFFFF00")
        # Remove "rgb(" and ")"
        # Also split into numbers
        col <-
            as.numeric(
                strsplit(
                    strsplit(substring(col, 5, nchar(col) - 1), ",")[[1]],
                    "%"))
        if (is.na(alpha))
            rgb(col[1], col[2], col[3], maxColorValue = 100)
        else
            rgb(col[1], col[2], col[3],
                as.numeric(alpha) * 100, maxColorValue = 100)
    }
    gparNames <- names(x)
    # col
    if ("colAlpha" %in% gparNames && "col" %in% gparNames) {
        if (! is.urlref(x$col)) {
            x$col <- rgba2col(x$col, x$colAlpha)
            x <- x[gparNames != "colAlpha"]
        }
    } else if ("col" %in% gparNames) {
        if (! is.urlref(x$col))
            x$col <- rgba2col(x$col, NA)
    }
    # fill
    if ("fillAlpha" %in% gparNames && "fill" %in% gparNames) {
        if (! is.urlref(x$fill)) {
            x$fill <- rgba2col(x$fill, x$fillAlpha)
            x <- x[gparNames != "fillAlpha"]
        }
    } else if ("fill" %in% gparNames) {
        if (! is.urlref(x$fill))
            x$fill <- rgba2col(x$fill, NA)
    }
    x
}

is.urlref <- function(x) {
    substring(x, 1, 3) == "url"
}

urlToID <- function(x) {
    # Remove first five chars "url(#" and last char ")"
    substring(x, 6, nchar(x) - 1)
}

hrefToID <- function(x) {
    # Remove "#"
    substring(x, 2, nchar(x))
}

isRectangular <- function(x) {
    # Pull out the line points because they will have the close path
    # segments too (necessary!)
    xs <- convertX(x$children[[2]]$x, "native", valueOnly = TRUE)
    ys <- convertY(x$children[[2]]$y, "native", valueOnly = TRUE)
    # Do not need to check for ys because they are guaranteed to be
    # the same length
    if (length(xs) != 5) {
        FALSE
    } else {
        # Line points must join up
        if (xs[1] != xs[5] || ys[1] != ys[5])
            return(FALSE)
        xs <- xs[1:4]
        ys <- ys[1:4]

        # Need to find the centre of of the rectangle
        # Then if the square of the distances are equal then we
        # know we have a rectangle
        cx <- mean(xs)
        cy <- mean(ys)
        dists <- (cx - xs)^2 + (cy - ys)^2
        # Check that all are equal with a small amount of tolerance
        isRect <- all(abs(dists - mean(dists)) < 0.00001)
        if (! isRect)
            return(FALSE)
        # Now to check whether there has been any rotation applied
        # FIXME: For now assume that we can't deal with rotation
        length(unique(xs) == 2) && length(unique(ys) == 2)
    }
}

toRectGrobpicComplexPath <- function(x) {
    lg <- x$children[[2]]
    xs <- convertX(lg$x[1:4], "native", valueOnly = TRUE)
    ys <- convertY(lg$y[1:4], "native", valueOnly = TRUE)

    picRectGrob(x = min(xs), y = min(ys),
                width = max(xs) - min(xs), height = max(ys) - min(ys),
                just = c("left", "bottom"), gp = lg$gp,
                default.units = "native")
}

stopsToCol <- function(stopStyle) {
    tmpgp <- gpar(col = stopStyle$`stop-color`,
                  colAlpha = stopStyle$`stop-opacity`)
    resolveColours(tmpgp)$col
}

parseTransform <- function(x) {
    if (is.null(x))
        return(x)
    # Remove "matrix(" and ")"
    pieces <- strsplit(substring(x, nchar("matrix(") + 1, nchar(x) - 1),
                       ",")[[1]]
    rbind(matrix(as.numeric(pieces), ncol = 3),
          c(0, 0, 1))
}

.grImport2Env <- new.env()

prefixName <- function(name) {
    paste0(get("prefix", envir = .grImport2Env), name)
}

generateNewPrefixGen <- function(n = 0) {
    counter <- n
    function() {
        counter <<- counter + 1
        paste("import", counter, "", sep = ".")
    }
}

generateNewPrefix <- generateNewPrefixGen()

setPrefix <- function(prefix = "") {
    assign("prefix", prefix, envir = .grImport2Env)
}

updateIdGen <- function() {
    index <- 0
    function(id) {
        index <<- index + 1
        paste(id, "update", index, sep="-")
    }
}

updateId <- updateIdGen()
