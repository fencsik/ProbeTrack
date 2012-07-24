### data13

do.data13 <- function () {
   infile <- "rawdata.txt"
   outfile <- "data13.rda"
   thisfile <- "data13.r"

   if (!file.exists(infile)) stop("cannot open file ", infile)
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken")
      return(invisible(NULL))
   }
   dt <- read.csv(infile)

   sub.pre <- length(unique(dt$sub))
   ## remove bad subjects:
   ## 1. remove tjo based on SSP's lab book (not listed in the lab book)
   ## 2. optionally remove jtd and rsh  because the weibull fits are weird
   dt <- dt[dt$sub != "tjo", ]
   ##dt <- dt[dt$sub != "tjo" & dt$sub != "jtd" & dt$sub != "rsh", ]
   dt$sub <- factor(dt$sub)
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
   dt$acc <- 1 - dt$error
   dt$soa <- dt$soa * 1000 / 75 # convert to ms
   dt$gapdur <- round(dt$gapdur * 1000 / 75)
   dt$soa[dt$gapdur == 0] <- 0 # set SOA to 0 on no-gap trials

   ## extract factors for all trials and for all correct trials
   dt2 <- dt[dt$acc == 1, ]
   factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, sub=sub))

   ## collapse across the factors
   data13 <- aggregate(data.frame(rt=dt2$rt), factorsC, median)
   data13$nobs <- aggregate(data.frame(rt=dt2$rt), factorsC, length)$rt

   save(data13, file=outfile)
   invisible(data13)
}

do.data13()
rm(do.data13)
