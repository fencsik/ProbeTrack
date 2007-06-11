### refresh.r: refresh all data and analysis files in the current directory
###
### $LastChangedDate$

refresh.all <- function()
{
   thisfile <- "refresh.r";
   exit.function <- function () {
      if (exists("op")) options(op);
   }
   on.exit(exit.function());
   cat("\nRefreshing files in", getwd(), "\n");

   op <- options(warn = 1);

   ## refresh all *.r files
   for (fname in list.files(pattern="^.*\.r$")) {
      if (fname == thisfile) next;
      if (!file.exists(fname)) stop("File ", fname, " disappeared");
      cat("Refreshing", fname, "\n");
      flush.console();
      source(fname);
   }
   
   cat("\n");
}

refresh.all();
rm(refresh.all);
