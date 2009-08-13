### refresh.r: refresh all plotting files
###
### $LastChangedDate$

refresh.all <- function()
{
   infiles <- c("pb1.r", "pb2.r",
                "pb3a.r", "pb3b.r",
                "pb4a.r", "pb4b.r",
                "pb5.r");
   thisfile <- "refresh.r";
   exit.function <- function () {
      if (exists("op")) options(op);
   }
   on.exit(exit.function());
   cat("\nRefreshing files in", getwd(), "\n");

   op <- options(warn = 1);

   ## refresh listed files
   for (fname in infiles) {
      if (!file.exists(fname)) stop("File ", fname, " not found");
      cat("Refreshing", fname, "\n");
      source(fname);
   }
   
   cat("\n");
}

refresh.all();
rm(refresh.all);
