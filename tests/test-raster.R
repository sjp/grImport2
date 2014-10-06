library(grid)
library(grImport2)

pic <- readPicture("test-raster-input.svg")

postscript("test-raster-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

if (! all(readLines("test-raster-output.ps") ==
          readLines("test-raster-output.ps.save")))
    stop("raster output not equal to expected output")

