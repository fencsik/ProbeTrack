### an0202: t-tests comparing d' on gap trials at each probe delay to d'
### on no-gap trials
###
### $LastChangedDate$

do.an0202 <- function () {
   thisfile <- "an0202.r";
   infile <- "data02.rda";
   outfile <- "an0202.txt";
   rdafile <- "an0202.rda";

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
   data02$soa <- as.numeric(as.character(data02$soa));

   d.gap <- with(data02[data02$gapdur == "10",],
                 tapply(dprime, list(sub, soa), mean));
   d.nogap <- with(data02[data02$gapdur == "0",],
                   tapply(dprime, list(sub), mean));

   results <- array(dim = c(dim(d.gap)[2], 3),
                    dimnames = list(dimnames(d.gap)[[2]], c("t", "p", "ci")));

   for (soa in dimnames(d.gap)[[2]]) {
      g <- t.test(d.gap[, soa], d.nogap, paired = T, var.equal = T);
      results[soa, "t"] <- round(g$statistic, 2);
      results[soa, "p"] <- round(g$p.value, 3);
      results[soa, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
   }

   sink(outfile);
   cat(c("Paired t-tests comparing d' between gap and no-gap trials at each",
         "probe delay.  The first column lists the t-statistics, the second",
         "the corresponding p-values, and the third 95% CIs.  Rows indicate",
         "probe delay.\n"), sep = "\n");
   print(results);

   an0202 <- results;
   save(an0202, file = rdafile);
}

do.an0202();
rm(do.an0202);
