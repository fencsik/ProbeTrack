### refresh.r: refresh all data and analysis files in the current directory
###
### $LastChangedDate$

refresh.all <- function()
{
   cat("\nRefreshing files in", getwd(), "\n");

   ## refresh data files
   for (fname in list.files(pattern="^data[0-9]+.r$")) {
      if (!file.exists(fname)) stop("File ", fname, " disappeared");
      cat("Refreshing", fname, "\n");
      flush.console();
      source(fname);
   }
   
   # refresh analysis files
   for (fname in list.files(pattern="^an[0-9]+.r$")) {
      if (!file.exists(fname)) stop("File ", fname, " disappeared");
      cat("Refreshing", fname, "\n");
      flush.console();
      source(fname);
   }
   # refresh analysis files
   for (fname in list.files(pattern="^fig[0-9]+.r$")) {
      if (!file.exists(fname)) stop("File ", fname, " disappeared");
      cat("Refreshing", fname, "\n");
      flush.console();
      source(fname);
   }
   cat("\n");
}

refresh.all();
rm(refresh.all);
