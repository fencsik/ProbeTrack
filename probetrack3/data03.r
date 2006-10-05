### data03
###
### $LastChangedDate$

do.data03 <- function () {
   infile <- "rawdata.txt";
   outfile <- "data03.rda";
   thisfile <- "data03.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (file.exists(outfile) &&
       file.info(outfile)$mtime > file.info(thisfile)$mtime &&
       file.info(outfile)$mtime > file.info(infile)$mtime) {
      warning("Output file is up to date, no action taken");
      return(NULL);
   }
   dt <- read.csv(infile);

   ## remove bad subjects:
   ## 1. remove tjo based on SSP's lab book (not listed in the lab book)
   ## 2. optionally remove jtd and rsh  because the weibull fits are weird
   dt <- dt[dt$identifier != "tjo", ];
   ##dt <- dt[dt$identifier != "tjo" & dt$identifier != "jtd" & dt$identifier != "rsh", ];
   dt$sub <- factor(dt$identifier);

   ## remove practice blocks, bad keypresses, and negative RTs (which should
   ## just indicate bad key presses)
   dt <- dt[dt$block == 2 & dt$wrongKeyFlag == 0 & !is.na(dt$RT) & dt$RT > 0, ];

   ## rename and recode variables
   dt$acc <- 1 - dt$error;
   dt$rt <- dt$RT;
   dt[dt$gapDuration == 0, "SOA"] <- 0; # set SOA to 0 on no-gap trials

   factors <- list(sub = dt$sub, gapdur = dt$gapDuration, ntargets = dt$nTargets, soa = dt$SOA);
   data03 <- aggregate(dt$acc, factors, length);
   names(data03)[names(data03) == "x"] <- "nobs";
   data03$ncor <- aggregate(dt$acc, factors, sum)$x;
   data03$pcor <- data03$ncor / data03$nobs;

   rt.cor <- rt.all <- rep(NA, dim(data03)[1]);
   for (i in 1:dim(data03)[1]) {
      index <- (dt$sub == data03$sub[i] & dt$gapDuration == data03$gapdur[i] &
                dt$nTargets == data03$ntargets[i] & dt$SOA == data03$soa[i]);
      if (any(index)) rt.all[i] <- mean(dt[index, "RT"]);
      index <- index & dt$acc == 1;
      if (any(index)) rt.cor[i] <- mean(dt[index, "RT"]);
   }
   data03$rt.all <- rt.all;
   data03$rt.cor <- rt.cor;

   save(data03, file=outfile)
   invisible(data03)
}

print(system.time(do.data03()));
rm(do.data03);
