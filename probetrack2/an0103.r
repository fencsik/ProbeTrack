### an0103: t-tests comparing RT on gap trials at each probe delay to RT on
### no-gap trials
###
### $LastChangedDate$

do.an0103 <- function () {
   thisfile <- "an0103.r";
   infile <- "data01.rda";
   outfile <- "an0103.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (file.exists(outfile) &&
       file.info(outfile)$mtime > file.info(thisfile)$mtime &&
       file.info(outfile)$mtime > file.info(infile)$mtime) {
      warning("Output file is up to date, no action taken");
      invisible(NULL);
   }
   load(infile);
   data01$soa <- as.numeric(as.character(data01$soa)) * 1000 / 75;

   d.gap <- with(data01[data01$gapdur == "10",],
                 tapply(rt.cor, list(sub, soa, target), mean));
   d.nogap <- with(data01[data01$gapdur == "0",],
                   tapply(rt.cor, list(sub, target), mean));

   results <- array(dim = c(dim(d.gap)[2], dim(d.gap)[3], 2),
                    dimnames = list(dimnames(d.gap)[[2]], c("0", "1"), c("t", "p")));

   for (target in dimnames(d.gap)[[3]]) {
      for (soa in dimnames(d.gap)[[2]]) {
         g <- t.test(d.gap[, soa, target], d.nogap[, target], paired = T, var.equal = T);
         results[soa, target, "t"] <- round(g$statistic, 2);
         results[soa, target, "p"] <- round(g$p.value, 3);
      }
   }

   sink(outfile);
   cat(c("Paired t-tests comparing correct RTs between gap and no-gap trials",
       "at each probe delay.  The first array lists the t-statistics, and",
       "the second is the corresponding p-values.  In each array, the rows",
       "indicate probe delay and columns indicate probe type with 0",
       "corresponding to probe-distractor trials and 1 to probe-target",
       "trials.\n"), sep = "\n");
   print(results);
}

do.an0103();
rm(do.an0103);
