### data10

do.data10 <- function () {
   infile <- "rawdata.rda";
   outfile <- "data10.rda";
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
   factorsC <- with(dt2, list(soa=soa, ntargets=ntargets, gapdur=gapdur, sub=sub));

   ## collapse across the factors
   data10 <- aggregate(data.frame(rt = dt2$rt), factorsC, median);

   save(data10, file=outfile)
   invisible(data10)
}

print(system.time(do.data10()));
rm(do.data10);
