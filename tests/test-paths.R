library(grid)
library(grImport2)

pic <- readPicture("test-path-simple-input.svg")
postscript("test-path-simple-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

pic <- readPicture("test-path-complex-input.svg")
postscript("test-path-complex-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

if (! all(readLines("test-path-simple-output.ps") ==
          readLines("test-path-simple-output.ps.save")))
    stop("simplepath expected not equal to expected output")

if (! all(readLines("test-path-complex-output.ps") ==
          readLines("test-path-complex-output.ps.save")))
    stop("complexpath output not equal to expected output")

