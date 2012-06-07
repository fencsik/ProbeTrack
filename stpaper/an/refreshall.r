### refreshall.r: refresh data and analyses for all experiments relevant to the
### ProbeTrack paper

f.refreshall <- function()
{
   thisfile <- "refreshall.r";
   infile <- "refresh.r";
   exp.dirs <- paste(file.path("..", "..", "probetrack"),
                     c("01", "02", "03", "06b", "09", "10"), sep="")
   exit.function <- function () {
      if (exists("op")) options(op);
      if (exists("owd") && owd != getwd()) setwd(owd);
   }
   on.exit(exit.function());
   cat("\nRefreshing files in", getwd(), "\n");

   op <- options(warn = 1);

   owd <- getwd();

   ## refresh all *.r files
   for (dirname in exp.dirs) {
      if (!file.exists(dirname)) {
         cat("Directory ", dirname, " cannot be found\n");
         next;
      }
      if (!file.exists(file.path(dirname, infile))) {
         cat("Directory ", dirname, " has no refresh file\n");
         next;
      }
      cat("Refreshing", dirname, "\n");
      setwd(dirname);
      source(infile);
      setwd(owd);
   }
   cat("\n");
}

f.refreshall();
rm(f.refreshall);
