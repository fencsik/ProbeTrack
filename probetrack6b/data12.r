### data12: compute RT by tracking load intercepts and slopes for each probe
### delay, gap duration, and subject; from data10
###
### $LastChangedDate$

f.data12 <- function () {
    infile <- "data10.rda";
    outfile <- "data12.rda";
    thisfile <- "data12.r";

    if (!file.exists(infile)) stop("cannot open file ", infile);
    if (IsFileUpToDate(outfile, c(thisfile, infile))) {
        warning("Output file is up to date, no action taken");
        return(invisible(NULL));
    }
    load(infile);

    data10 <- data10[data10$gapdur > 0,];
    all.ntargets <- levels(data10$ntargets);
    all.sub <- levels(data10$sub);
    all.soa <- levels(data10$soa);
    data10$soa <- as.numeric(as.character(data10$soa));

    ## Compute slopes and intercepts for each subject
    p.slopes <- t.slopes <- slopes <- intercepts <-
        array(dim = c(length(all.ntargets), length(all.sub)),
              dimnames = list(all.ntargets, all.sub));
    for (nt in dimnames(slopes)[[1]]) {
        for (sub in dimnames(slopes)[[2]]) {
            g <- lm(rt ~ soa,
                    data = data10[data10$ntargets == nt &
                      data10$sub == sub, ]);
            intercepts[nt, sub] <- g$coef[[1]];
            slopes[nt, sub] <- g$coef[[2]];
            gs <- summary(g);
            t.slopes[nt, sub] <- gs$coef[2, 3];
            p.slopes[nt, sub] <- gs$coef[2, 4];
        }
    }
    regr <- as.data.frame(as.table(intercepts));
    names(regr) <- c("ntargets", "sub", "intercept");
    regr$slope <- as.data.frame(as.table(slopes))$Freq;
    regr$t.slope <- as.data.frame(as.table(t.slopes))$Freq;
    regr$p.slope <- as.data.frame(as.table(p.slopes))$Freq;

    ## Compute slopes and intercepts for averaged data
    avg <- with(data10, aggregate(data.frame(rt = rt),
                                  list(soa = soa, ntargets = ntargets), mean));
    p.slopes <- t.slopes <- slopes <- intercepts <-
        array(dim = c(length(all.ntargets)),
              dimnames = list(all.ntargets));
    for (nt in dimnames(slopes)[[1]]) {
        g <- lm(rt ~ soa, data = avg[avg$ntargets == nt, ]);
        intercepts[nt] <- g$coef[[1]];
        slopes[nt] <- g$coef[[2]];
        gs <- summary(g);
        t.slopes[nt] <- gs$coef[2, 3];
        p.slopes[nt] <- gs$coef[2, 4];
    }
    avg <- as.data.frame(as.table(intercepts));
    names(avg) <- c("ntargets", "intercept");
    avg$slope <- as.data.frame(as.table(slopes))$Freq;
    avg$t.slope <- as.data.frame(as.table(t.slopes))$Freq;
    avg$p.slope <- as.data.frame(as.table(p.slopes))$Freq;

    ## collect and save output
    data12 <- list(data = data10, regr = regr, avg = avg);
    save(data12, file=outfile);
}

f.data12();
rm(f.data12);
