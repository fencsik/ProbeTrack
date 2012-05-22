### an0402: t-tests comparing RT on gap trials at each probe delay to RT on
### no-gap trials

do.an0402 <- function () {
   infile <- "data04.rda";
   outfile <- "an0402.txt";
   rdafile <- "an0402.rda";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   load(infile);
   dt <- data04$data;
   dt$sub <- as.character(dt$sub);
   dt$gapdur <- as.numeric(as.character(dt$gapdur));
   dt$ntargets <- as.numeric(as.character(dt$ntargets));
   dt$soa <- as.numeric(as.character(dt$soa));

   baseline <- with(data04$fit, tapply(baseline, list(sub, gapdur, ntargets), mean));
   dp <- with(dt, tapply(rt, list(sub, soa, gapdur, ntargets), mean));
   dn <- dimnames(dp);
   results <- array(dim = c(dim(dp)[2:4], 3),
                    dimnames = list(dn[[2]], dn[[3]], dn[[4]], c("t", "p", "ci")));

   for (soa in dn[[2]]) {
      for (gd in dn[[3]]) {
         for (nt in dn[[4]]) {
            g <- t.test(dp[, soa, gd, nt], baseline[, gd, nt], paired = T, var.equal = T);
            results[soa, gd, nt, "t"] <- round(g$statistic, 2);
            results[soa, gd, nt, "p"] <- round(g$p.value, 3);
            results[soa, gd, nt, "ci"] <- diff(as.numeric(g$conf.int)) / 2;
         }
      }
   }

   sink(outfile);
   cat(c("Paired t-tests comparing correct RTs between observed values and",
         "estimated baselines at each probe delay.\n"), sep = "\n");
   print(results);

   an0402 <- results;
   save(an0402, file = rdafile);
}

do.an0402();
rm(do.an0402);
