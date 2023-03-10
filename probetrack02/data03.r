### data03
###
### $LastChangedDate$

do.data03 <- function () {
   infile <- "rawdata.txt";
   outfile <- "data03.rda";
   thisfile <- "data03.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   dt <- read.csv(infile);

   ## remove bad subjects:
   ## 1. optionally remove ssf because the weibull fit is weird
   ##dt <- dt[dt$sub != "ssf", ];
   ## 2. or remove ssf and el because of poor performance
   ##dt <- dt[dt$sub != "ssf" & dt$sub != "el", ];
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
