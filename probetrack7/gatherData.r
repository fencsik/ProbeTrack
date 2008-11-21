### gatherData.r: collects data from individual subject's data files and outputs
### to one comma-delimited text data file
###
### $LastChangedDate: 2008-08-07 21:33:18 -0700 (Thu, 07 Aug 2008) $

f.gatherData <- function () {
    datadir <- "data";
    outfile <- "rawdata.txt";
    thisfile <- "gatherData.r";
    filelist <- "gatherDataList.rda";

    infiles <- file.path(datadir, dir(datadir));
    foundNewFile <- FALSE;
    if (file.exists(filelist)) {
        load(filelist);
        if (length(filesLoaded) != length(infiles) || any(filesLoaded != infiles)) {
            foundNewFile <- TRUE;
        }
        if (!foundNewFile) {
            warning("No new input files found");
        }
    }
    if (!foundNewFile) {
        if (IsFileUpToDate(outfile, c(thisfile, infiles))) {
            warning("Output file is up to date, no action taken");
            return(invisible(NULL));
        }
    }

    varnames <- NULL;
    varnamesFromFile <- NULL;
    alldata <- NULL;
    filesLoaded <- infiles;

    for (f in infiles) {
        if (!file.exists(f)) stop("cannot find input file ", f);
        cat("Opening data file ", f, "...", sep = "");
        dt <- read.delim(f);

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

    write.table(alldata, file = outfile, sep = "\t", quote = F, row.names = F);
    save(filesLoaded, file = filelist);
}

f.gatherData();
rm(f.gatherData);
