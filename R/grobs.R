# Custom grobs that scale their line widths relative the the dimensions
# of the image. For example, lines should be thicker when the image is
# larger.

makeContext.picRect <- function(x) {
    x$gp <- scaleLwd(x$gp)
    x
}

makeContent.picRect <- function(x) {
    rectGrob(x$x, x$y, x$width, x$height,
             just = x$just, gp = x$gp, vp=x$vp,
             default.units = x$default.units)
}

picRectGrob <- function(x, y, width, height, just, gp, default.units, vp=NULL) {
    grob(x = x, y = y, width = width, height = height,
         just = just, gp = gp, default.units = default.units, vp = vp,
         cl = "picRect")
}

makeContent.picPath <- function(x) {
    pathGrob(x$x, x$y, id.lengths = x$id.lengths, rule = x$rule,
             gp = x$gp, default.units = x$default.units)
}

picPathGrob <- function(x, y, id.lengths, rule, gp, default.units) {
    grob(x = x, y = y, id.lengths = id.lengths, rule = rule,
         gp = gp, default.units = default.units, cl = "picPath")
}

makeContent.picPolyline <- function(x) {
    polylineGrob(x$x, x$y, id.lengths = x$id.lengths,
                 gp = x$gp, default.units = x$default.units)
}

picPolylineGrob <- function(x, y, id.lengths, gp, default.units) {
    grob(x = x, y = y, id.lengths = id.lengths,
         gp = gp, default.units = default.units,
         cl = "picPolyline")
}

makeContext.picComplexPath <- function(x) {
    x$gp <- scaleLwd(x$gp)
    x
}

picComplexPathGrob <- function(linePoints, pathPoints,
                               lineIDLengths, pathIDLengths,
                               rule, gp) {
    gTree(children = gList(
        picPathGrob(unlist(pathPoints$x), unlist(pathPoints$y),
                    id.lengths = pathIDLengths,
                    rule = rule, gp = gpar(col = "#FFFFFF00"),
                    default.units = "native"),
        picPolylineGrob(unlist(linePoints$x), unlist(linePoints$y),
                        id.lengths = lineIDLengths,
                        gp = gpar(fill = "#FFFFFF00"),
                        default.units = "native")
    ), gp = gp, cl = "picComplexPath")
}

scaleLwd <- function(gp) {
    lwd <- gp$lwd
    lty <- gp$lty
    # Calculate the current "resolution"
    res <- 96 # pdf, png, jpeg, postscript, all 1/96 per lwd
    scaleFactor <-
        res * min(abs(convertWidth(unit(1, "native"),
                                   "inches", valueOnly = TRUE)),
                  abs(convertHeight(unit(1, "native"),
                                    "inches", valueOnly = TRUE)))
    if (! is.null(lwd))
        lwd <- lwd * scaleFactor

    # FIXME CONSIDER CASE WHEN LWD IS NOT PRESENT
    if (! is.null(lty))
        lty <- paste(as.hexmode(pmax(pmin(round((lty * scaleFactor) / lwd), 15), 1)),
                     collapse = "")

    gp$lwd <- lwd
    gp$lty <- lty
    gp
}

# Apply gridSVG features to grid grobs
# FIXME: Make this more efficient by using registration
gridSVGAddFeatures <- function(grob, gp, defs,
                               mask = character(0),
                               filter = character(0)) {
    gparNames <- names(gp)
    if ("gradientFill" %in% gparNames) {
        gradDef <- getDef(defs, gp$gradientFill)
        if (! is.null(gradDef)) {
            # Assume a fillAlpha of 1 because there will be no
            # fill property (due to gradient being there instead)
            fillAlpha <- 1
            grob <- gradientFillGrob(grob,
                                     label = prefixName(gp$gradientFill),
                                     alpha = fillAlpha)
        }
    }
    if ("gradientStroke" %in% gparNames) {
        # Do nothing, not supported by gridSVG (yet)
        #def <- getDef(defs, gp$gradientStroke)
    }
    if ("patternFill" %in% gparNames) {
        patDef <- getDef(defs, gp$patternFill)
        if (! is.null(patDef)) {
            fillAlpha <- 1
            grob <- patternFillGrob(grob,
                                    label = prefixName(gp$patternFill),
                                    alpha = fillAlpha)
        }
    }
    if ("patternStroke" %in% gparNames) {
        # Do nothing, not supported by gridSVG (yet)
        #def <- getDef(defs, gp$patternStroke)
    }
    # Now for masks and filters
    if (length(mask))
        grob <- maskGrob(grob, label = prefixName(mask))
    if (length(filter))
        grob <- filterGrob(grob, label = prefixName(filter))
    grob
}

# Viewport from picture
pictureVP <- function(picture, expansion = 0.05,
                      xscale = NULL, yscale = NULL,
                      distort = FALSE, ...) {
    if (is.null(xscale) || is.null(yscale)) {
        xscale <- picture@summary@xscale
    	yscale <- picture@summary@yscale
    }
    xscale <- xscale + expansion * c(-1, 1) * diff(xscale)
    yscale <- yscale + expansion * c(-1, 1) * diff(yscale)

    # If distort=TRUE, having the two layers of viewports is
    # massively redundant, BUT I'm keeping it so that either
    # way there is the same viewport structure, which I think
    # is beneficial if anyone ever wants to make use of
    # these viewports (otherwise they would need to figure
    # out whether a picture grob has one or two viewports).
    vpStack(viewport(name = "picture.shape", ...,
                     layout = grid.layout(1, 1,
                       widths = abs(diff(xscale)),
                       heights = abs(diff(yscale)),
                       respect = ! distort)),
            viewport(name = "picture.scale",
                     layout.pos.col = 1,
                     xscale = xscale,
                     yscale = yscale,
                     clip = "on"))
}

clipVP <- function(xscale, yscale) {
    # Note: SVG y-scales are reversed
    viewport(x = xscale[1], y = yscale[2],
             width = abs(diff(xscale)), height = abs(diff(yscale)),
             xscale = xscale, yscale = yscale,
             default.units = "native", just = c("left", "top"),
             clip = "on")
}

registerDefs <- function(defs) {
    content <- defs@content
    ids <- names(content)
    for (i in seq_len(length(content))) {
        def <- content[[i]]
        label <- prefixName(ids[i])
        if (class(def) == "PicturePattern")
            registerPatternFill(label, grobify(def))
        if (class(def) == "PictureFilter")
            registerFilter(label, grobify(def))
        if (class(def) == "PictureMask")
            registerMask(label, grobify(def))
        if (class(def) == "PictureClipPath")
            registerClipPath(label, clipPath(grobify(def)))
        if (any(class(def) == c("PictureLinearGradient",
                                "PictureRadialGradient")))
            registerGradientFill(label, grobify(def))
    }
}
