### data01
###
### $LastChangedDate$

do.data01 <- function () {
   infile <- "rawdata.txt";
   rdafile <- "data01.rda";
   txtfile <- "data01.txt";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   dt <- read.csv(infile);

   ## remove bad subjects:
   ## 1. remove wms based on SSP's lab book (lots of bad keypresses)
   dt <- dt[dt$identifier != "wms", ];
   dt$sub <- factor(dt$identifier);

   ## clean up some variables
   dt$target <- 2 - dt$probeType; # probeType == 1 -> target; probeType == 2 -> distractor
   dt$acc <- 1 - dt$error;

   ## remove practice blocks, bad keypresses, and negative RTs (which should
   ## just indicate bad key presses)
   dt <- dt[dt$block == 2 & dt$wrongKeyFlag == 0 & !is.na(dt$RT) & dt$RT > 0, ];

   dt$soa <- dt$SOA;
   dt$rt <- dt$RT;

   data01 <- dt[, c("sub", "cueDuration", "postProbeDuration", "gapOnset",
                    "gapDuration", "nObjects", "trial", "nTargets", "soa",
                    "target", "acc", "rt" )];
   
   save(data01, file=rdafile)
   invisible(data01)
}

do.data01()
rm(do.data01)
