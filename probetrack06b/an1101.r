### an1101: generate table of parameters of fitted model for each subject
###
### $LastChangedDate$

f.an1101 <- function () {
    thisfile <- "an1101.r";
    infile <- "data11.rda";
    outfile <- "an1101.txt";

    exit.function <- function () {
        while (sink.number() > 0) sink();
    }
    on.exit(exit.function());

    if (!file.exists(infile)) stop("cannot open input file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    dt <- data11$parameters;
    dt$sub <- as.character(dt$sub);
    dt$gapdur <- as.numeric(as.character(dt$gapdur));
    dt$ntargets <- as.numeric(as.character(dt$ntargets));
    dt$r.sq <- dt$r^2;
    category <- c(with(dt, sprintf("%s-%02d-%1dtarg", sub, gapdur, ntargets)),
                  "MEAN");
    dt <- rbind(dt, apply(as.matrix(dt), 2, mean, na.rm = TRUE));
    rownames(dt) <- category;

    sink(outfile);
    ##    cat("Parameter estimates from model fit\n");
    ##    print(round(dt[, c("rtime", "baseRT")], 3));

    ##    cat("\n\n\n");
    ##    cat("Fitting output and goodness-of-fit statistics\n");
    ##    print(round(dt[, c("iter", "code", "sse", "rmse", "r.sq")], 3));

    ##    cat("\n\n\n");
    cat("Average rtime, with 95% confidence intervals\n");
    y <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(ntargets), mean));
    ci <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(ntargets),
                            function(x) qt(.975, length(x) - 1) * sqrt(var(x) / length(x))));
    for (nt in dimnames(y)[[1]]) {
        cat(sprintf("%1s targets: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", nt, y[nt], ci[nt],
                    y[nt] - ci[nt], y[nt] + ci[nt]));
    }
    y <- dt$rtime[rownames(dt) != "MEAN"];
    ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y));
    cat(sprintf("     MEAN: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
                mean(y) - ci, mean(y) + ci));

    cat("\n\n");
    cat("Goodness of fit statistics for overall data\n");
    cat(sprintf("R-squared = %0.5f\n", data11$r.all ^ 2));

    cat("\n\n");
    cat("Goodness of fit statistics for averaged fit\n");
    obsey <- with(data11$data, aggregate(obs, list(soa, ntargets, gapdur), mean))$x;
    predy <- with(data11$data, aggregate(pred, list(soa, ntargets, gapdur), mean))$x;
    cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2));

    cat("\n\n");
    cat("Goodness of fit statistics for fit from averaged parameters\n");
    m.rtime <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(ntargets), mean));
    m.baseRT <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(ntargets), mean));
    obse <- with(data11$data, tapply(obs, list(soa, ntargets), mean));
    pred <- with(data11$data, tapply(pred, list(soa, ntargets), mean));
    x <- as.numeric(dimnames(obse)[[1]]);
    for (nt in dimnames(m.rtime)[[1]]) {
        pred[, nt] <- data11$model(x, m.rtime[nt], m.baseRT[nt]);
    }
    cat(sprintf("R-squared = %0.5f\n", cor(as.vector(obse), as.vector(pred)) ^ 2));
}

f.an1101();
rm(f.an1101);
