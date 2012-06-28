### data10
###
### $LastChangedDate$

do.data10 <- function () {
   infile <- "rawdata.txt";
   outfile <- "data10.rda";
   thisfile <- "data10.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   dt <- read.csv(infile);

   ## remove bad subjects:
   ## 1. optionally remove ssf because the weibull fit is weird
   ##dt <- dt[dt$sub != "ssf", ];
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
   factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));

   ## collapse across the factors
   data10 <- aggregate(data.frame(rt = dt2$rt), factorsC, median);
   data10$nobs <- aggregate(data.frame(rt = dt2$rt), factorsC, length)$rt;

   save(data10, file=outfile)
   invisible(data10)
}

print(system.time(do.data10()));
rm(do.data10);
