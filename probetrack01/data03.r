### data03

do.data03 <- function () {
   infile <- "rawdata.rda";
   outfile <- "data03.rda";
   load(infile);
   dt <- alldata;

   ## remove bad subjects:
   ## 1. remove nw based on SSP's lab book (very low accuracy)
   ## 2. optionally remove ssf and wz because their weibull fits are weird
   dt <- dt[dt$sub != "nw", ];
   ##dt <- dt[dt$sub != "nw" & dt$sub != "wz", ];
   ##dt <- dt[dt$sub != "nw" & dt$sub != "ssf", ];
   ##dt <- dt[dt$sub != "nw" & dt$sub != "ssf" & dt$sub != "wz", ];
   dt$sub <- factor(dt$sub);

   ## remove practice blocks, bad keypresses, and negative RTs (which should
   ## just indicate bad key presses)
   dt <- dt[dt$block == 2 & dt$badkey == 0 & !is.na(dt$rt) & dt$rt > 0, ];

   ## rename and recode variables
   dt$acc <- 1 - dt$error;
   dt$soa <- dt$soa * 1000 / 75; # convert to ms
   dt$soa[dt$gapdur == 0] <- 0; # set SOA to 0 on no-gap trials

   ## extract factors for all trials and for all correct trials
   dt2 <- dt[dt$acc == 1, ];
   factorsA <- with(dt, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));
   factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));

   ## collapse across the factors
   data03 <- aggregate(data.frame(nobs = dt$acc), factorsA, length);
   data03$ncor <- aggregate(dt$acc, factorsA, sum)$x;
   data03$pcor <- data03$ncor / data03$nobs;
   data03$rt <- aggregate(dt2$rt, factorsC, mean)$x;
   data03$rt.all <- aggregate(dt$rt, factorsA, mean)$x;

   save(data03, file=outfile)
   invisible(data03)
}

print(system.time(do.data03()));
rm(do.data03);
