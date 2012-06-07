### refreshall.r: refresh data and analyses for all experiments relevant to the
### ProbeTrack paper

f.refreshall <- function()
{
   thisfile <- "refreshall.r";
   makefile <- "Makefile"
   refreshfile <- "refresh.r";
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
          cat(sprintf("Directory %s cannot be found\n", dirname));
          next;
      }
      if (file.exists(file.path(dirname, makefile))) {
          cat(sprintf("Refreshing %s using make\n", dirname))
          setwd(dirname)
          system("make")
      } else if (file.exists(file.path(dirname, refreshfile))) {
          cat(sprintf("Refreshing %s using R refresh file\n", dirname))
          setwd(dirname);
          source(refreshfile);
      } else {
          cat(sprintf("Directory %s has no refresh method\n", dirname))
          next
      }
      cat("\n")
      setwd(owd);
   }
   cat("\n");
}

f.refreshall();
rm(f.refreshall);
