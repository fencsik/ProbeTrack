### data22: compute RT by tracking load intercepts and slopes for each probe
### delay, gap duration, and subject; from data20
###
### $LastChangedDate$

f.data22 <- function () {
    infile <- "data20.rda";
    outfile <- "data22.rda";
    thisfile <- "data22.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    data20 <- data20[data20$gapdur > 0,];
    all.ntargets <- levels(data20$ntargets);
    all.sub <- levels(data20$sub);
    all.soa <- levels(data20$soa);
    data20$ntargets <- as.numeric(as.character(data20$ntargets));

    ## Compute slopes and intercepts for each subject
    p.slopes <- t.slopes <- slopes <- intercepts <-
        array(dim = c(length(all.soa), length(all.sub)),
              dimnames = list(all.soa, all.sub));
    for (soa in dimnames(slopes)[[1]]) {
        for (sub in dimnames(slopes)[[2]]) {
            g <- lm(rt ~ ntargets,
                    data = data20[data20$soa == soa &
                      data20$sub == sub, ]);
            intercepts[soa, sub] <- g$coef[[1]];
            slopes[soa, sub] <- g$coef[[2]];
            gs <- summary(g);
            t.slopes[soa, sub] <- gs$coef[2, 3];
            p.slopes[soa, sub] <- gs$coef[2, 4];
        }
    }
    regr <- as.data.frame(as.table(intercepts));
    names(regr) <- c("soa", "sub", "intercept");
    regr$slope <- as.data.frame(as.table(slopes))$Freq;
    regr$t.slope <- as.data.frame(as.table(t.slopes))$Freq;
    regr$p.slope <- as.data.frame(as.table(p.slopes))$Freq;

    ## Compute slopes and intercepts for averaged data
    avg <- with(data20, aggregate(data.frame(rt = rt),
                                  list(soa = soa, ntargets = ntargets), mean));
    p.slopes <- t.slopes <- slopes <- intercepts <-
        array(dim = c(length(all.soa)), dimnames = list(all.soa));
    for (soa in dimnames(slopes)[[1]]) {
        g <- lm(rt ~ ntargets, data = avg[avg$soa == soa, ]);
        intercepts[soa] <- g$coef[[1]];
        slopes[soa] <- g$coef[[2]];
        gs <- summary(g);
        t.slopes[soa] <- gs$coef[2, 3];
        p.slopes[soa] <- gs$coef[2, 4];
    }
    avg <- as.data.frame(as.table(intercepts));
    names(avg) <- c("soa", "intercept");
    avg$slope <- as.data.frame(as.table(slopes))$Freq;
    avg$t.slope <- as.data.frame(as.table(t.slopes))$Freq;
    avg$p.slope <- as.data.frame(as.table(p.slopes))$Freq;

    ## collect and save output
    data22 <- list(data = data20, regr = regr, avg = avg);
    save(data22, file=outfile);
}

f.data22();
rm(f.data22);
