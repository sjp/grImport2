setClass("PathData",
         representation(segments = "list"))
setValidity("PathData",
            function(object) {
                all(sapply(object@segments, is, "PathSegment"))
            })

setClass("PathSegment",
         representation(x = "numeric",
                        y = "numeric"))
setValidity("PathSegment",
            function(object) length(object@x) == length(object@y))

setClass("PathMoveTo", representation("PathSegment"))
setClass("PathLineTo", representation("PathSegment"))
setClass("PathCurveTo", representation("PathSegment"))
setClass("PathClosePath", representation("PathSegment"))

# Convenience constructors
moveTo <- function(x, y) new("PathMoveTo", x = x, y = y)
lineTo <- function(x, y) new("PathLineTo", x = x, y = y)
closePath <- function(x, y) new("PathClosePath", x = x, y = y)
curveTo <- function(x, y) {
    # Perform calculations on an off-screen NULL device
    pdf(file = NULL)
    # The unit itself doesn't actually matter, so long as it is absolute
    xs <- unit(x, "bigpts")   
    ys <- unit(y, "bigpts")
    points <- bezierPoints(bezierGrob(xs, ys))
    ct <- lapply(points, function(x) {
        convertUnit(x, "bigpts", valueOnly = TRUE)
    })
    dev.off() # close NULL device
    new("PathCurveTo", x = ct$x, y = ct$y)
}

# We will need to get the points necessary to create a line or a path.
# Create methods used to get these points
setGeneric("getPoints", function(object, line) standardGeneric("getPoints"))

setMethod("getPoints", signature(object = "PathSegment", line = "logical"),
          function(object, line) list(x = object@x, y = object@y))
setMethod("getPoints", signature(object = "PathClosePath", line = "logical"),
          function(object, line)
              # If we have a path that we're parsing where a closePath is
              # present, we want to know what the stroke looks like, which is
              # separate from the fill.
              # This is because, interestingly, SVG can fill a polyline but
              # grid (and R graphics) cannot.
              # This means when we need to draw the stroke for the entire
              # path we want to know where the stroke ends up, while a fill
              # does not need to know the final points.
              if (line)
                  list(x = object@x, y = object@y)
              else
                  list(x = NULL, y = NULL))
# Now to get points from *all* segments
setMethod("getPoints",
          signature(object = "PathData", line = "logical"),
          function(object, line) {
              list(x = unlist(sapply(object@segments,
                                     function(ps) getPoints(ps, line)$x)),
                   y = unlist(sapply(object@segments,
                                     function(ps) getPoints(ps, line)$y)))
          })

setMethod("applyTransform",
          signature(object = "PathData", tm = "matrix"),
          function(object, tm) {
              object@segments <- lapply(object@segments,
                                        function(pathSegment) {
                                            applyTransform(pathSegment, tm)
                                        })
              object
          })

setMethod("applyTransform",
          signature(object = "PathSegment", tm = "matrix"),
          function(object, tm) {
              nr <- length(object@x)
              locs <- matrix(c(object@x, object@y, rep(1, nr)), ncol = 3)
              for (i in seq_len(nrow(locs)))
                  locs[i, ] <- tm %*% locs[i, ]
              object@x <- locs[, 1]
              object@y <- locs[, 2]
              object
          })

# Expects to take a string from a <path>'s d=""
parsePathData <- function(x) {
    # Path data comes in the form of "M x y L x y C x y x y x y [Z]"
    # We want the type, x and y for each command
    pieces <- strsplit(x, " ")[[1]]
    # Some paths have d="", just return NULL in that case
    # because we'll never see anything
    if (! length(pieces))
        return(NULL)
    # Attempt to cast to numeric
    # Any that are NA *must* be commands
    ncommands <- suppressWarnings(length(which(is.na(as.numeric(pieces)))))
    pathData <- vector("list", length(ncommands))
    dataInd <- 0
    # Need to maintain a current position (because of curveTo)
    pos <- numeric(2)
    # Also start position (for closePath)
    startPos <- numeric(2)
    ind <- 1
    while (ind <= length(pieces)) {
        command <- pieces[ind]
        # Special case to remove trailing "M" command on older
        # versions of Cairo
        if (command == "M" && dataInd == (ncommands - 1)) {
            # BUT watch out for case where path data consist of only
            # trailing "M"
            if (ncommands == 1)
                return(NULL)
            # Need to truncate list to remove unused command
            pathData <- pathData[1:dataInd]
            break
        }
        # Regular cases follow
        if (command == "M") {
            startPos <- pos <- xy <- as.numeric(pieces[ind + 1:2])
            dataInd <- dataInd + 1
            pathData[[dataInd]] <- moveTo(xy[1], xy[2])
            ind <- ind + 3
        }
        if (command == "L") {
            pos <- xy <- as.numeric(pieces[ind + 1:2])
            dataInd <- dataInd + 1
            pathData[[dataInd]] <- lineTo(xy[1], xy[2])
            ind <- ind + 3
        }
        if (command == "C") {
            xy <- as.numeric(pieces[ind + 1:6])
            dataInd <- dataInd + 1
            pathData[[dataInd]] <- curveTo(c(pos[1], xy[c(TRUE, FALSE)]),
                                           c(pos[2], xy[c(FALSE, TRUE)]))
            pos <- xy[5:6]
            ind <- ind + 7
        }
        if (command == "Z") {
            dataInd <- dataInd + 1
            pathData[[dataInd]] <- closePath(startPos[1], startPos[2])
            ind <- ind + 1
        }
    }
    new("PathData", segments = pathData)
}
