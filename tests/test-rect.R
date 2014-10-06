library(grid)
library(grImport2)

pic <- readPicture("test-rect-input.svg")

postscript("test-rect-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

if (! all(readLines("test-rect-output.ps") ==
          readLines("test-rect-output.ps.save")))
    stop("rect output not equal to expected output")

