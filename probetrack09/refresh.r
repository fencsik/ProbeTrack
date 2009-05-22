### refresh.r: refresh all data and analysis files in the current directory
###
### $LastChangedDate$

refresh.all <- function()
{
    ## gather data files together
    gatherData <- "gatherData.r";
    if (file.exists(gatherData)) {
        cat("\nRefreshing", gatherData, "\n");
        flush.console();
        source(gatherData);
    }

    ## refresh data files
    for (fname in list.files(pattern="^data[0-9]+.r$")) {
        cat("Refreshing", fname, "\n");
        flush.console();
        source(fname);
    }

    ## refresh table-generating files
    for (fname in list.files(pattern="^tab[0-9]+.r$")) {
        cat("Refreshing", fname, "\n");
        flush.console();
        source(fname);
    }

    ## refresh figure-generating files
    for (fname in list.files(pattern="^fig[0-9]+.r$")) {
        cat("Refreshing", fname, "\n");
        flush.console();
        source(fname);
    }
    cat("\n");
}

refresh.all();
rm(refresh.all);
