### data01
###
### $LastChangedDate$

do.data01 <- function () {
   infile <- "rawdata.txt";
   outfile <- "data01.rda";
   thisfile <- "data01.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   dt <- read.csv(infile);

   sub.pre <- length(unique(dt$sub))
   ## remove bad subjects:
   ## 1. optionally remove ssf because the weibull fit is weird
   ##dt <- dt[dt$sub != "ssf", ];
   dt$sub <- factor(dt$sub);
   cat(sprintf("Dropped %0.0f of %0.0f subject(s)\n",
               sub.pre - length(unique(dt$sub)), sub.pre))

   ## remove practice blocks, bad keypresses, and responses that are too
   ## fast or too slow
   dt <- dt[dt$block == 2, ]
   trials.pre <- nrow(dt)
   cat(sprintf("Starting with %0.0f experimental trials\n", trials.pre))
   dt <- dt[dt$badkey == 0 & !is.na(dt$rt), ]
   cat(sprintf("Dropped %0.0f trial(s) for invalid responses\n",
               trials.pre - nrow(dt)))
   trials.pre <- nrow(dt)
   dt <- dt[dt$rt > 50 & dt$rt < 5000, ]
   cat(sprintf("Dropped %0.0f trial(s) for being too fast/slow\n",
               trials.pre - nrow(dt)))
   cat(sprintf("%0.0f experimental trials remaining\n", nrow(dt)))

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
