### data01

do.data01 <- function () {
   infile <- "rawdata.rda";
   outfile <- "data01.rda";
   load(infile);
   dt <- alldata;

   ## remove bad subjects:
   ## 1. remove nw based on SSP's lab book (very low accuracy)
   dt <- dt[dt$sub != "nw", ];
   dt$sub <- factor(dt$sub);

   ## remove practice blocks, bad keypresses, and negative RTs (which should
   ## just indicate bad key presses)
   dt <- dt[dt$block == 2 & dt$badkey == 0 & !is.na(dt$rt) & dt$rt > 0, ];

   ## rename and recode variables
   dt$target <- 2 - dt$probeType; # probeType == 1 -> target; probeType == 2 -> distractor
   dt$acc <- 1 - dt$error;
   dt$soa[dt$gapdur == 0] <- 0; # set SOA to 0 on no-gap trials
   dt$soa <- dt$soa * 1000 / 75; # convert to ms

   ## extract factors for all trials and for all correct trials
   dt2 <- dt[dt$acc == 1, ];
   factorsA <- with(dt, list(soa=soa, target=target, ntargets=ntargets, gapdur=gapdur, sub=sub));
   factorsC <- with(dt2, list(soa=soa, target=target, ntargets=ntargets, gapdur=gapdur, sub=sub));

   ## collapse across the factors
   data01 <- aggregate(data.frame(nobs = dt$acc), factorsA, length);
   data01$ncor <- aggregate(dt$acc, factorsA, sum)$x;
   data01$pcor <- data01$ncor / data01$nobs;
   data01$rt <- aggregate(dt2$rt, factorsC, mean)$x;
   data01$rt.all <- aggregate(dt$rt, factorsA, mean)$x;

   save(data01, file=outfile)
   invisible(data01)
}

print(system.time(do.data01()));
rm(do.data01);
