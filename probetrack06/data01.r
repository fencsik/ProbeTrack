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

   ## remove bad subjects:
   ## 1. remove wms based on SSP's lab book (lots of bad keypresses)
   dt <- dt[dt$sub != "wms", ];
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
   data01$rt.all <- aggregate(dt$rt, factorsA, mean)$x;

   data01$rt <- rep(NA, length(data01$nobs));
   for (subject in levels(data01$sub)) {
      for (gd in levels(data01$gapdur)) {
         for (nt in levels(data01$ntargets)) {
            for (targ in levels(data01$target)) {
               for (s in levels(data01$soa)) {
                  filter.dt2 <- with(dt2, sub == subject & gapdur == gd &
                                     ntargets == nt & target == targ & soa == s);
                  if (!any(filter.dt2)) next;
                  filter.data01 <- with(data01, sub == subject & gapdur == gd &
                                        ntargets == nt & target == targ & soa == s);
                  data01[filter.data01, "rt"] <- mean(dt2[filter.dt2, "rt"]);
               }
            }
         }
      }
   }
   ##data01$rt <- aggregate(dt2$rt, factorsC, mean)$x;

   save(data01, file=outfile)
   invisible(data01)
}

print(system.time(do.data01()));
rm(do.data01);
