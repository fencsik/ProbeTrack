### an0303: t-tests comparing RT on gap trials at each probe delay and gap
### duration to RT on no-gap trials
###
### $LastChangedDate$

do.an0303 <- function () {
   thisfile <- "an0303.r";
   infile <- "data03.rda";
   outfile <- "an0303.txt";
   rdafile <- "an0303.rda";

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
   data03$soa <- as.numeric(as.character(data03$soa)) * 1000 / 75;
   data03$gapdur <- round(as.numeric(as.character(data03$gapdur)) * 1000 / 75, 0);

   d.gap <- with(data03[data03$gapdur != 0,],
                 tapply(rt.cor, list(sub, soa, gapdur), mean));
   d.nogap <- with(data03[data03$gapdur == 0,],
                   tapply(rt.cor, list(sub), mean));

   results <- array(dim = c(dim(d.gap)[2], dim(d.gap)[3], 3),
                    dimnames = list(dimnames(d.gap)[[2]], dimnames(d.gap)[[3]], c("t", "p", "ci")));

   for (soa in dimnames(d.gap)[[2]]) {
      for (gapdur in dimnames(d.gap)[[3]]) {
         g <- t.test(d.gap[, soa, gapdur], d.nogap, paired = T, var.equal = T);
         results[soa, gapdur, "t"] <- round(g$statistic, 2);
         results[soa, gapdur, "p"] <- round(g$p.value, 3);
         results[soa, gapdur, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
      }
   }

   sink(outfile);
   cat(c("Paired t-tests comparing correct RTs between gap and no-gap trials",
         "at each gap duration and probe delay.  The array dimensions are",
         "SOA (ms), gap duration (ms), and statistic, with statistics being",
         "the t-value, p-value, and 95% CIs.\n"), sep = "\n");
   print(results);

   an0303 <- results;
   save(an0303, file = rdafile);
}

do.an0303();
rm(do.an0303);
