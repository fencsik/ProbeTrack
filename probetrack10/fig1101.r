### fig1101.r: plot observed and predicted RT as a function of probe delay,
### separated by tracking load, separately for each subject

f.fig1101 <- function () {
    infile <- "data11.rda"
    outfile <- "fig1101.pdf"
    thisfile <- "fig1101.r"

    on.exit(if (exists("opar")) par(opar))
    on.exit(if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off(),
            TRUE)

    if (!file.exists(infile)) stop("cannot open input file ", infile)
    if (IsFileUpToDate(outfile, c(infile, thisfile))) {
        warning("Output file is up to date, no action taken")
        return(invisible(NULL))
    }
    load(infile)

    obse <- with(data11$data, tapply(rt, list(soa, sub), mean))
    rtime <- with(data11$fit, tapply(rtime, list(sub), mean))
    baseRT <- with(data11$fit, tapply(baseRT, list(sub), mean))
    x <- as.numeric(dimnames(obse)[[1]])
    x.fit <- 0:max(x)
    pred <- numeric(length(x.fit))
    names(pred) <- as.character(x.fit)

    ## plot settings
    col <- "black"
    pch <- 21
    lwd <- c(1, 1)
    lty <- c(1, 2)
    ylim <- c(400, 700)
    pt.bg <- rep("white", 2)

    pdf(outfile, width=8, height=8, pointsize=12)
    opar <- par(mfrow=c(2, 2), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    counter <- 0
    for (sub in dimnames(obse)[[2]]) {
        matplot(x, obse[, sub], type="p", bty="n",
                axes=F, cex=.75, #ylim=ylim,
                col=col, pch=pch, lty=lty[1], lwd=lwd[1], bg=pt.bg,
                xlab="", ylab="", main=sprintf("ProbeTrack10 - %s", sub))
        pred <- data11$model(x.fit, rtime[sub], baseRT[sub])
        matlines(x.fit, pred,
                 col=col, lty=lty[2], lwd=lwd[2])
        ## matlines(x, pred[, , sub], type="o",
        ##          col=col, lty=lty[2], lwd=lwd[2], pch=pch[2])
        axis(1, x)
        axis(2)
        if (counter %% 4 > 1) title(xlab="Probe delay (ms)")
        if (counter %% 2 == 0) title(ylab="Probe RT (ms)")
        counter <- counter + 1
    }
}

f.fig1101()
rm(f.fig1101)
