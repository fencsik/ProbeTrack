### an0101: anova on RT as a function of probe delay and probe type
###
### $LastChangedDate$

do.an0101 <- function () {
   thisfile <- "an0101.r";
   infile <- "data01.rda";
   outfile <- "an0101.txt";

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(infile)) stop("cannot open input file ", infile);
   if (file.exists(outfile) &&
       file.info(outfile)$mtime > file.info(thisfile)$mtime &&
       file.info(outfile)$mtime > file.info(infile)$mtime) {
      return(NULL);
   }
   load(infile);

   sink(outfile);
   cat("ANOVA on RT as a function of probe type and probe delay\n");
   print(summary(aov(rt.cor ~ target * soa + Error(sub / (target * soa)),
                     data01[data01$gapdur == "0",])));
}

do.an0101();
rm(do.an0101);
