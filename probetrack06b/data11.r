### data11.r: fit reacquire-time model to RT by probe delay functions from
### data10, allowing different reacquisition times for each tracking load
###
### $LastChangedDate$

f.data11 <- function () {
    thisfile <- "data11.r";
    infile <- "data10.rda";
    outfile <- "data11.rda";

    exit.function <- function () {
        if (exists("old.opt")) options(old.opt);
    }
    on.exit(exit.function());
    old.opt <- options(warn = 1);

    if (!file.exists(infile)) stop("cannot open input file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    ## define parameters
    p0 <- c(0, 500);
    names(p0) <- c("rtime", "baseRT");
    
    reacquire.model <- function (soa, rtime, baseRT) {
        baseRT + ifelse(rtime - soa > 0, rtime - soa, 0);
    }

    ## Calculate SSE goodness of fit.  Assumes obs is a array with length ==
    ## length(soa).  Parameters are rtime and baseRT.
    GoodnessOfFit <- function(p, soa, obs) {
        if (p[1] >= 0 && p[2] >= 0) {
            sum( (obs - reacquire.model(soa, rtime = p[1], baseRT = p[2])) ^ 2 );
        } else {
            10^12;
        }
    }

    ## Generate data array
    data.array <- with(data10[data10$gapdur != "0", ], tapply(rt, list(soa, ntargets, gapdur, sub), mean));

    ## extract levels of variables
    allSubjects <- dimnames(data.array)[[4]]; nSubjects <- length(allSubjects);
    allGapDurs <- dimnames(data.array)[[3]]; nGapDurs <- length(allGapDurs);
    allNTargets <- dimnames(data.array)[[2]]; nNTargets <- length(allNTargets);
    allSOAs <- dimnames(data.array)[[1]];

    ## set up place to store parameters and predicted results
    param <- array(NA, dim = c(7, nNTargets, nGapDurs, nSubjects),
                   dimnames = list(c("rtime", "baseRT", "iter", "code", "sse", "rmse", "r"),
                     allNTargets, allGapDurs, allSubjects));
    pred.array <- array(NA, dim = dim(data.array), dimnames = dimnames(data.array));

    for (sub in allSubjects) {
        for (gd in allGapDurs) {
            for (nt in allNTargets) {
                p <- p0;
                p["baseRT"] <- mean(data.array[, nt, gd, sub]);
                out <- nlm(GoodnessOfFit, p, print.level = 0, steptol = 1e-05, fscale = 1000, 
                           obs = data.array[, nt, gd, sub], soa = as.numeric(allSOAs));
                p <- out$estimate;
                pred.array[, nt, gd, sub] <- reacquire.model(as.numeric(allSOAs), p[1], p[2]);
                param[, nt, gd, sub] <- c(p[1], p[2],
                                          out$iterations, out$code, out$minimum,
                                          sqrt(mean((data.array[, nt, gd, sub] - pred.array[, nt, gd, sub])^2)),
                                          cor(data.array[, nt, gd, sub], pred.array[, nt, gd, sub]));
            }
        }
    }

    ## Convert observed and predicted data to data frame
    data <- as.data.frame(as.table(data.array));
    names(data) <- c("soa", "ntargets", "gapdur", "sub", "obs");
    data$pred <- as.data.frame(as.table(pred.array))$Freq;

    ## Convert parameters/output to data frame
    first <- TRUE;
    for (i in dimnames(param)[[1]]) {
        if (first) {
            param2 <- as.data.frame(as.table(param[i, , , , drop = FALSE]))[, -1];
            names(param2) <- c("ntargets", "gapdur", "sub", i);
            first <- FALSE;
        } else {
            eval(parse(text = sprintf("param2$%s <- as.data.frame(as.table(param[i, , , ]))$Freq", i)));
        }
    }

    print(summary(param2$code));

    data11 <- list(data = data, parameters = param2,
                   fit = sum((data$obs - data$pred)^2),
                   r.all = cor(data$obs, data$pred),
                   model = reacquire.model,
                   exp = "ProbeTrack6b");
    save(data11, file=outfile);
}

f.data11();
rm(f.data11);
