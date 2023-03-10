### data01
###
### $LastChangedDate$

do.data01 <- function () {
   infile <- "rawdata.txt";
   outfile <- "data01.rda";
   thisfile <- "data01.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (file.exists(outfile) &&
       file.info(outfile)$mtime > file.info(thisfile)$mtime &&
       file.info(outfile)$mtime > file.info(infile)$mtime) {
      return(NULL);
   }
   dt <- read.csv(infile);

   ## remove bad subjects:
   ## 1. remove wms based on SSP's lab book (lots of bad keypresses)
   dt <- dt[dt$identifier != "wms", ];
   dt$sub <- factor(dt$identifier);

   ## remove practice blocks, bad keypresses, and negative RTs (which should
   ## just indicate bad key presses)
   dt <- dt[dt$block == 2 & dt$wrongKeyFlag == 0 & !is.na(dt$RT) & dt$RT > 0, ];

   ## rename and recode variables
   dt$target <- 2 - dt$probeType; # probeType == 1 -> target; probeType == 2 -> distractor
   dt$acc <- 1 - dt$error;
   dt$rt <- dt$RT;

   factors <- list(sub = dt$sub, gapdur = dt$gapDuration, target = dt$target, ntargets = dt$nTargets, soa = dt$SOA);
   data01 <- aggregate(dt$acc, factors, length);
   names(data01)[names(data01) == "x"] <- "nobs";
   data01$pcor <- aggregate(dt$acc, factors, mean)$x;

   rt.cor <- rt.all <- rep(NA, dim(data01)[1]);
   for (i in 1:dim(data01)[1]) {
      index <- (dt$sub == data01$sub[i] & dt$target == data01$target[i] & dt$gapDuration == data01$gapdur[i] &
                dt$nTargets == data01$ntargets[i] & dt$SOA == data01$soa[i]);
      if (any(index)) rt.all[i] <- mean(dt[index, "RT"]);
      index <- index & dt$acc == 1;
      if (any(index)) rt.cor[i] <- mean(dt[index, "RT"]);
   }
   data01$rt.all <- rt.all;
   data01$rt.cor <- rt.cor;

   save(data01, file=outfile)
   invisible(data01)
}

print(system.time(do.data01()));
rm(do.data01);
