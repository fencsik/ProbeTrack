### gatherData.r: collects data from individual subject's data files and outputs
### to one comma-delimited text data file
###
### $LastChangedDate$

do.gatherData <- function () {
   datadir <- "data";
   infiles <- dir(datadir);
   outfile <- "rawdata.txt";

   varnames <- NULL;
   varnamesFromFile <- NULL;
   alldata <- NULL;

   for (f in infiles) {
      f <- file.path(datadir, f);
      if (!file.exists(f)) stop("cannot find input file ", f);
      cat("Opening data file ", f, "...", sep = "");
      dt <- read.csv(f);

      if (is.null(varnames)) {
         varnames <- colnames(dt);
         varnamesFromFile <- f;
      } else if (dim(dt)[2] != length(varnames) || !all(names(dt) == varnames)) {
         warning("column names in ", f, " do not match those in ", varnamesFromFile);
         dt <- dt[, varnames];
      }

      if (is.null(alldata)) {
         alldata <- dt;
      } else {
         alldata <- rbind(alldata, dt);
      }
      cat("done\n");
   }

   write.csv(alldata, file = outfile, quote = F, row.names = F);
}

do.gatherData();
rm(do.gatherData);


