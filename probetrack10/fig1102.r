### fig1102.r: plot observed and predicted RT as a function of probe delay,
### separated by tracking load, averaged across subjects.

f.fig1102 <- function () {
    infile <- "data11.rda"
    outfile <- "fig1102.pdf"
    thisfile <- "fig1102.r"

    on.exit(if (exists("opar")) par(opar))
    on.exit(if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off(),
            TRUE)

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (IsFileUpToDate(outfile, c(infile, thisfile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)

    obse <- with(data11$data, tapply(rt, list(soa), mean))
    x <- as.numeric(dimnames(obse)[[1]])
    rtime.avg <- mean(data11$fit[, "rtime"])
    baseRT.avg <- mean(data11$fit[, "baseRT"])
    rtime.all <- with(data11$fit, tapply(rtime, list(sub), mean))
    baseRT.all <- with(data11$fit, tapply(baseRT, list(sub), mean))

    ## fit model to each subject in each condition
    x.fit <- 0:max(x)
    nx <- length(x.fit)
    pred.avg <- data11$model(x.fit, rtime.avg, baseRT.avg)
    names(pred.avg) <- as.character(x.fit)
    pred.all <- array(dim=c(nx, dim(rtime.all)[1]),
                      dimnames=list(as.character(x.fit),
                        dimnames(rtime.all)[[1]]))
    for (sub in dimnames(pred.all)[[2]]) {
        pred.all[, sub] <- data11$model(x.fit, rtime.all[sub], baseRT.all[sub])
    }
    pred.all <- apply(pred.all, 1, mean)

    ## plot settings
    col <- "black"
    pch <- 21
    lwd.all <- 1
    lty.all <- 3
    lwd.avg <- 2
    lty.avg <- 2

    pdf(outfile, width=6, height=6, pointsize=12)
    opar <- par(mfrow=c(1, 1), las=1, pty="m", cex.axis=.6,
                xpd=F, bg="white")

    ylim <- c(550, 700)

    plot(x, obse, type="p", bty="n",
         axes=F, ylim=ylim,
         col=col, pch=pch,
         xlab="Probe delay (ms)", ylab="Probe RT (ms)",
         main="ProbeTrack10")
    axis(1, x)
    axis(2)
    abline(v=0, col="gray50", lty=1)
    matlines(x.fit, pred.all, col=col, lwd=lwd.all, lty=lty.all)
    matlines(x.fit, pred.avg, col=col, lwd=lwd.avg, lty=lty.avg)
    legend("bottomleft", c("Across Subj", "Avg Pars"),
           bty="n", cex=.8, inset=c(.05, 0),
           lwd=c(lwd.all, lwd.avg), lty=c(lty.all, lty.avg))
}

f.fig1102()
rm(f.fig1102)
