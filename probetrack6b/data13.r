### data13: compute RT by tracking load intercepts and slopes for each probe
### delay, gap duration, and subject, eliminating tracking load 1; from data10
###
### $LastChangedDate$

f.data13 <- function () {
    infile <- "data10.rda";
    outfile <- "data13.rda";
    thisfile <- "data13.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    data10 <- data10[data10$gapdur > 0 & data10$ntargets != "1",];
    all.ntargets <- levels(data10$ntargets);
    all.sub <- levels(data10$sub);
    all.soa <- levels(data10$soa);
    data10$ntargets <- as.numeric(as.character(data10$ntargets));

    ## Compute slopes and intercepts for each subject
    p.slopes <- t.slopes <- slopes <- intercepts <-
        array(dim = c(length(all.soa), length(all.sub)),
              dimnames = list(all.soa, all.sub));
    for (soa in dimnames(slopes)[[1]]) {
        for (sub in dimnames(slopes)[[2]]) {
            g <- lm(rt ~ ntargets,
                    data = data10[data10$soa == soa &
                      data10$sub == sub, ]);
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
    avg <- with(data10, aggregate(data.frame(rt = rt),
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
    data13 <- list(data = data10, regr = regr, avg = avg);
    save(data13, file=outfile);
}

f.data13();
rm(f.data13);
