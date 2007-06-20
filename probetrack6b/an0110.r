### an0110: generate table of average accuracy as a function of set size and gap
### duration for each subject
###
### $LastChangedDate$

do.an0110 <- function () {
   thisfile <- "an0110.r";
   infile <- "data01.rda";
   outfile <- "an0110.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);

   data01$sub <- as.character(data01$sub);
   data01$ntargets <- as.numeric(as.character(data01$ntargets));
   data01$gapdur <- as.numeric(as.character(data01$gapdur));
   data01$cond <- with(data01, sprintf("gap%02d-%d", gapdur, ntargets));

   sink(outfile);
   cat("Proportion correct at each combination of gap duration and tracking load\n");
   print(round(with(data01, tapply(pcor, list(cond, sub), mean)), 2));
}

do.an0110();
rm(do.an0110);
