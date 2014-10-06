library(grid)
library(grImport2)

pic <- readPicture("test-clip-input.svg")

postscript("test-noclip-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

postscript("test-bboxclip-output.ps", paper = "special",
           width = 6, height = 6, horizontal = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

if (! all(readLines("test-noclip-output.ps") ==
          readLines("test-noclip-output.ps.save")))
    stop("noclip output not equal to expected output")

if (! all(readLines("test-bboxclip-output.ps") ==
          readLines("test-bboxclip-output.ps.save")))
    stop("bboxclip output not equal to expected output")

