setMethod("grobify",
          signature(object = "PictureClipPath"),
          function(object, defs, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              if (is.null(object@content) || ! length(object@content))
                  return(gTree())
              children <- lapply(object@content, grobify,
                                 defs = defs, gpFUN = gpFUN,
                                 ext = ext)
              gTree(children = do.call("gList", children))
          })

setMethod("grobify",
          signature(object = "PictureFeColorMatrix"),
          function(object) {
              feColorMatrix(input = object@input,
                            type = object@type,
                            values = object@values)
          })

setMethod("grobify",
          signature(object = "PictureFilter"),
          function(object, ...) {
              # defs are not needed, nor gpFUN, and gridSVG is implied
              filterEffect(grobify(object@content),
                           filterUnits = object@filterUnits,
                           x = object@x, y = object@y,
                           width = object@width, height = object@height,
                           just = c("left", "bottom"))
          })

setMethod("grobify",
          signature(object = "PictureMask"),
          function(object, defs, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              if (is.null(object@content) || ! length(object@content))
                  return(mask(gTree()))
              children <- lapply(object@content, grobify,
                                 defs = defs, gpFUN = gpFUN,
                                 ext = ext)
              mask(gTree(children = do.call("gList", children)))
          })

setMethod("grobify",
          signature(object = "PicturePattern"),
          function(object, ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              # Ignoring gpFUN because it will only be applied to a
              # raster and gp settings do not apply to rasters.

              # dims in inches
              # We need this because the device needs to be only as large
              # as the pattern (as parsed by grImport), no more.
              win <- abs(convertWidth(unit(object@width, "native"),
                                      "inches", valueOnly = TRUE))
              hin <- abs(convertHeight(unit(object@height, "native"),
                                      "inches", valueOnly = TRUE))

              # Grobify all pattern content
              patternContent <- lapply(object@definition, grobify, ext=ext, ...)
              # Construct bounding box for pattern
              # bbox <- getbbox(object@definition)
              bbox <- c(0, abs(object@width), 0, abs(object@height))
              # Generate gTree containing all content
              # AND with viewport that has scale based on content bbox
              tilegrob <- gTree(children=do.call("gList", patternContent),
                                vp=viewport(xscale=bbox[1:2],
                                            yscale=bbox[3:4]))
              
              # Draw a pattern.
              # The pattern has some dimensions, but these dimensions
              # must be matched by the width of the pattern tile device.
              pattern(tilegrob,
                      x = object@x, y = object@y,
                      width = object@width, height = object@height,
                      default.units = "native", just = c("left", "bottom"),
                      dev.width =  win, dev.height = hin)
          })

setMethod("grobify",
          signature(object = "PictureImage"),
          function(object, ext = c("none", "clipbbox", "gridSVG"), ...) { 
              ext <- match.arg(ext)
              # Note gpFUN is not used here because it has no effect on
              # rasterGrobs. The 'grid' documentation points out that all
              # gpar settings are *ignored*.
              # IF the PictureImage has an angle, we need to generate
              # a viewport to draw the raster within (to perform the rotation)
              if (!length(object@angle))
                  object@angle <- 0
              if (object@angle != 0) {
                  r <- rasterGrob(object@image, 
                                  x=0, y=1, width=1, height=-1,
                                  just = c("left", "bottom"),
                                  vp = angleVP(object, image=TRUE))
              } else {
                  r <- rasterGrob(object@image,
                                  x = object@x,
                                  y = unit(object@y + object@height, "native"),
                                  width = object@width, height = -object@height,
                                  default.units = "native",
                                  just = c("left", "bottom"))
              }
              # Note, could add gridSVG features but we know only masks
              # can be applied to an <image>
              if (ext == "gridSVG" &&
                  length(object@maskRef) && length(object@maskRef))
                  maskGrob(r, label = prefixName(object@maskRef))
              else
                  r
          })

setMethod("grobify",
          signature(object = "PictureRadialGradient"),
          function(object, ...) {
              stops <- lapply(object@stops, grobify)
              offsets <- sapply(stops, function(x) x$offset)
              cols <- sapply(stops, function(x) x$col)
              radialGradient(col = cols,
                             stops = offsets,
                             gradientUnits = "coords",
                             x = object@x,
                             y = object@y,
                             r = object@r,
                             fx = object@fx,
                             fy = object@fy,
                             spreadMethod = object@spreadMethod,
                             default.units = "native")
          })

setMethod("grobify",
          signature(object = "PictureLinearGradient"),
          function(object, ...) {
              stops <- lapply(object@stops, grobify)
              offsets <- sapply(stops, function(x) x$offset)
              cols <- sapply(stops, function(x) x$col)
              linearGradient(col = cols,
                             stops = offsets,
                             gradientUnits = "coords",
                             x0 = object@x0,
                             x1 = object@x1,
                             y0 = object@y0,
                             y1 = object@y1,
                             spreadMethod = object@spreadMethod,
                             default.units = "native")
          })

setMethod("grobify",
          signature(object = "PictureGradientStop"),
          function(object, ...) {
              list(offset = object@offset, col = object@col)
          })

# Only called when we already know object@transform is non-NULL
angleVP <- function(object, image=FALSE) {
    # Get new x, y, width, height, and angle from
    # original x, y, width, height, and transform
    if (image)
        object@angle <- -object@angle
    viewport(x = object@x, y = object@y,
             width = object@width, height = object@height,
             just = c("left", "bottom"),
             default.units = "native",
             angle = 180 * object@angle / pi,
             name = "anglevp")
}

setMethod("grobify",
          signature(object = "PictureRect"),
          function(object, defs, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              object@gp <- gpFUN(object@gp)
              # IF the PictureRect has an angle, we need to generate
              # a viewport to draw the rect within (to perform the rotation)
              if (!length(object@angle))
                  object@angle <- 0
              if (object@angle != 0) {
                  grob <- picRectGrob(.5, .5, 1, 1, "centre", 
                                      object@gp, "npc",
                                      vp = angleVP(object))
              } else {
                  grob <- picRectGrob(object@x, object@y,
                                      object@width, object@height,
                                      just = c("left", "bottom"),
                                      default.units = "native",
                                      gp = object@gp)
              }
              if (ext == "gridSVG")
                  grob <- gridSVGAddFeatures(grob, object@gp, defs)
              grob
          })

setMethod("grobify",
          signature(object = "PicturePath"),
          function(object, defs, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              # Due to the complex nature of SVG paths, we require a bit
              # of calculation in order to translate these into R paths.
              ismt <- sapply(object@d@segments, is, "PathMoveTo")
              nsp <- sum(ismt) # number of subpaths, always at least 1
              starts <- which(ismt)
              groupSize <- diff(c(starts, length(ismt) + 1))
              ngroups <- length(groupSize)

              # Lines should have fewer points than a path (when there
              # are close paths present
              linePoints <- getPoints(object@d, line = TRUE)
              pathPoints <- getPoints(object@d, line = FALSE)

              if (ngroups == 1) {
                  # No need to split into sub-grobs because we only have
                  # a single region to consider
                  pidlen <- lidlen <- NULL
              } else {
                  # Complex case where we need to consider how the sub-grobs
                  # are split into different groups. Collecting the lengths
                  # of each sub-grob group
                  pidlen <- lidlen <- numeric(ngroups)
                  for (i in seq_len(ngroups)) {
                      groupInds <- starts[i] + 0:(groupSize[i] - 1)
                      lidlen[i] <- sum(sapply(object@d@segments[groupInds],
                                              function(x) {
                          length(getPoints(x, line = TRUE)$x)
                      }))
                      pidlen[i] <- sum(sapply(object@d@segments[groupInds],
                                              function(x) {
                          length(getPoints(x, line = FALSE)$x)
                      }))

                      # If pidlen is less than lidlen *and* pidlen is only
                      # length 1 this means that the path data is simply a
                      # MOVETO and a CLOSEPATH. In other words, this subpath
                      # does nothing.
                      # Just assume that we have a LINETO instead (which draws
                      # no line), this makes things a bit easier than removing.
                      # This is because the subpath will still match the
                      # associated polyline fragment. Furthermore, it means
                      # that when exporting via gridSVG, the sub-grob ID
                      # is a bit more sensible (because it is not removing an
                      # empty group).
                      if (pidlen[i] < lidlen[i] && pidlen[i] == 1) {
                          # i == 1 is a simple append case
                          if (i == 1) {
                              pathPoints$x <- c(pathPoints$x[1],
                                                pathPoints$x)
                              pathPoints$y <- c(pathPoints$y[1],
                                                pathPoints$y)
                          } else {
                              # last position plus one (current)
                              pos <- sum(pidlen[1:(i - 1)])
                              pathPoints$x <- append(pathPoints$x,
                                                     pathPoints$x[pos], after = pos)
                              pathPoints$y <- append(pathPoints$y,
                                                     pathPoints$y[pos], after = pos)
                          }

                          # We now have two points, so use that
                          pidlen[i] <- 2
                      }
                  }
              }

              object@gp <- gpFUN(object@gp)

              # Create a complex path grob.
              # The complication with this grob arises due to the fact that
              # SVG <polyline>s can have a fill region, whereas the R (and
              # therefore grid) graphics engine cannot.
              # A polyline draws the stroke, while a path draws the fill
              # region of the *SVG* path.
              # This produces the same effect when the stroke is drawn above
              # the filled path (which picComplexPathGrob attempts to ensure)
              grob <- picComplexPathGrob(linePoints, pathPoints,
                                         lidlen, pidlen,
                                         object@rule, object@gp)
              if (ext == "gridSVG")
                  grob <- gridSVGAddFeatures(grob, object@gp, defs)
              grob
          })

setMethod("grobify",
          signature(object = "PictureGroup"),
          function(object, defs, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"), ...) {
              ext <- match.arg(ext)
              clipvp <-
                  if (! is.null(object@clip) && ext == "clipbbox") {
                      cp <- object@clip
                      bbox <- getbbox(cp)
                      clipVP(bbox[1:2], bbox[3:4])
                  } else {
                      NULL
                  }
              object@gp <- gpFUN(object@gp)
              childTree <- lapply(object@content,
                                  grobify, defs = defs,
                                  gpFUN = gpFUN, ext = ext)
              groupGrob <- gTree(children = do.call("gList", childTree),
                                 gp = object@gp,
                                 vp = clipvp)
              if (! is.null(object@clip) && ext == "gridSVG") {
                  cp <- object@clip
                  groupGrob <- clipPathGrob(groupGrob,
                                            label = prefixName(cp@label))
              }
              if (ext == "gridSVG")
                  groupGrob <-
                      gridSVGAddFeatures(groupGrob, object@gp, defs,
                                         object@maskRef, object@filterRef)
              groupGrob
          })

setMethod("grobify",
          signature(object = "Picture"),
          function(object, gpFUN = identity,
                   ext = c("none", "clipbbox", "gridSVG"),
                   expansion = 0.05, xscale = NULL, yscale = NULL,
                   distort = FALSE, name = NULL, ...) {
              ext <- match.arg(ext)
              pvp <- pictureVP(object, expansion = expansion,
                               xscale = xscale, yscale = yscale,
                               distort = distort, ...)
              if (ext == "gridSVG") {
                  if (! require(gridSVG)) {
                      warning("the 'gridSVG' package is required for advanced graphical features, reverting to 'clipbbox'")
                      ext <- "clipbbox"
                  } else {
                      # Things like gradients and patterns need to be aware of
                      # native scales at the time of registration. Because of
                      # this we also set the gradient and pattern labels to be
                      # the same as the SVG IDs.
                      pushViewport(pvp, recording = FALSE)
                      registerDefs(object@defs)
                      # Up 2 because we pushed a vpStack of 2 viewports
                      upViewport(2, recording = FALSE)
                  }
              }

              children <- lapply(object@content,
                                 grobify, defs = object@defs,
                                 gpFUN = gpFUN, ext = ext)
              gt <- gTree(children = do.call("gList", children),
                          name = name, vp = pvp)
              gt$name <- prefixName(gt$name)
              gt
          })
