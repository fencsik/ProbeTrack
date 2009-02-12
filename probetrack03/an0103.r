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
   data01$gapdur <- as.numeric(as.character(data01$gapdur));

   d.gap <- with(data01[data01$gapdur != "0",],
                 tapply(rt, list(sub, soa, target, gapdur), mean));
   d.nogap <- with(data01[data01$gapdur == "0",],
                   tapply(rt, list(sub, target), mean));

   results <- array(dim = c(dim(d.gap)[2], dim(d.gap)[3], dim(d.gap)[4], 3),
                    dimnames = list(dimnames(d.gap)[[2]], dimnames(d.gap)[[3]], dimnames(d.gap)[[4]], c("t", "p", "ci")));

   for (soa in dimnames(d.gap)[[2]]) {
      for (target in dimnames(d.gap)[[3]]) {
         for (gapdur in dimnames(d.gap)[[4]]) {
            g <- t.test(d.gap[, soa, target, gapdur], d.nogap[, target], paired = T, var.equal = T);
            results[soa, target, gapdur, "t"] <- round(g$statistic, 2);
            results[soa, target, gapdur, "p"] <- round(g$p.value, 3);
            results[soa, target, gapdur, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
         }
      }
   }

   sink(outfile);
   cat(c("Paired t-tests comparing correct RTs between gap and no-gap trials",
         "at each gap duration/probe delay.  The array dimensions are SOA (ms),",
         "probe type (1 = target), gap duration (ms), and statistic, with",
         "statistics being the t-value, p-value, and 95% CIs.\n"), sep = "\n");
   print(results);

   an0103 <- results;
   save(an0103, file = rdafile);
}

do.an0103();
rm(do.an0103);
