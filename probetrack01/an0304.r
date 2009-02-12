### an0304: t-tests comparing accuracy on gap trials at each probe delay to
### accuracy on no-gap trials
###
### $LastChangedDate$

do.an0304 <- function () {
   thisfile <- "an0304.r";
   infile <- "data03.rda";
   outfile <- "an0304.txt";
   rdafile <- "an0304.rda";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (IsFileUpToDate(c(rdafile, outfile), c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   load(infile);
   data03$soa <- as.numeric(as.character(data03$soa));

   d.gap <- with(data03[data03$gapdur == "10",],
                 tapply(pcor, list(sub, soa), mean));
   d.nogap <- with(data03[data03$gapdur == "0",],
                   tapply(pcor, list(sub), mean));

   results <- array(dim = c(dim(d.gap)[2], 3),
                    dimnames = list(dimnames(d.gap)[[2]], c("t", "p", "ci")));

   for (soa in dimnames(d.gap)[[2]]) {
      g <- t.test(d.gap[, soa], d.nogap, paired = T, var.equal = T);
      results[soa, "t"] <- round(g$statistic, 2);
      results[soa, "p"] <- round(g$p.value, 3);
      results[soa, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
   }

   sink(outfile);
   cat(c("Paired t-tests comparing accuracy between gap and no-gap trials",
         "at each probe delay.  The first column lists the t-statistics, the",
         "second the corresponding p-values, and the third 95% CIs.  Rows",
         "indicate probe delay.\n"), sep = "\n");
   print(results);

   an0304 <- results;
   save(an0304, file = rdafile);
}

do.an0304();
rm(do.an0304);
