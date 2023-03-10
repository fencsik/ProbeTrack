### an0103: t-tests comparing RT on gap trials at each probe delay to RT on
### no-gap trials
###
### $LastChangedDate$

do.an0103 <- function () {
   thisfile <- "an0103.r";
   infile <- "data01.rda";
   outfile <- "an0103.txt";
   rdafile <- "an0103.rda";

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
   data01$soa <- as.numeric(as.character(data01$soa));

   d.gap <- with(data01[data01$gapdur == "10",],
                 tapply(rt, list(sub, soa, target), mean));
   d.nogap <- with(data01[data01$gapdur == "0",],
                   tapply(rt, list(sub, target), mean));

   results <- array(dim = c(dim(d.gap)[2], dim(d.gap)[3], 3),
                    dimnames = list(dimnames(d.gap)[[2]], c("0", "1"), c("t", "p", "ci")));

   for (target in dimnames(d.gap)[[3]]) {
      for (soa in dimnames(d.gap)[[2]]) {
         g <- t.test(d.gap[, soa, target], d.nogap[, target], paired = T, var.equal = T);
         results[soa, target, "t"] <- round(g$statistic, 2);
         results[soa, target, "p"] <- round(g$p.value, 3);
         results[soa, target, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
      }
   }

   sink(outfile);
   cat(c("Paired t-tests comparing correct RTs between gap and no-gap trials",
         "at each probe delay.  The first array lists the t-statistics, the",
         "second the corresponding p-values, and the third 95% CIs.  In each",
         "array, the rows indicate probe delay and the columns indicate probe",
         "type with 0 corresponding to probe-distractor trials and 1 to",
         "probe-target trials.\n"), sep = "\n");
   print(results);

   an0103 <- results;
   save(an0103, file = rdafile);
}

do.an0103();
rm(do.an0103);
