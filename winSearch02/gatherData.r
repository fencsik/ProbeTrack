### gatherData.r: collects data from individual subject's data files and outputs
### to one tab-delimited text data file
###
### $LastChangedDate$

do.gatherData <- function () {
   datadir <- "data";
   infiles <- dir(datadir);
   outfile <- "winsearch02Data";

   varnames <- c("exp","code","version","sub","computer","blocktime","prac",
                 "trial","trialtime","refreshdur","nrefreshes","velT","velD",
                 "setsize","target","resp","acc","rt","dur","preptime","stimdur",
                 "framedurmin","framedurmean","framedurmax",
                 "drawtimemin","drawtimemean","drawtimemax");

   for (f in infiles) {
      f <- file.path(datadir, f);
      if (!file.exists(f)) stop("cannot find input file ", f);
      cat("Opening data file ", f, "...", sep = "");
      dt <- read.csv(f);

      if (dim(dt)[2] != length(varnames) || !all(names(dt) == varnames)) {
         warning("bad column names in ", f);
         dt <- dt[, varnames];
      }

      if (!exists("alldata")) {
         alldata <- dt;
      } else {
         alldata <- rbind(alldata, dt);
      }
      cat("done\n");
   }

   write.table(alldata, file = outfile, sep = ",", quote = F, row.names = F);
}

do.gatherData();
rm(do.gatherData);


