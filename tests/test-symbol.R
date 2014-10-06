library(grid)
library(grImport2)

pic <- readPicture("test-symbol-input.svg")

postscript("test-symbol-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

if (! all(readLines("test-symbol-output.ps") ==
          readLines("test-symbol-output.ps.save")))
    stop("symbol/use output not equal to expected output")

