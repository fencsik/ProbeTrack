### fig1101.r: plot fit of model against observed data for each subject

do.fig1101 <- function () {
    infile <- "data11.rda"
    outfile <- "fig1101.pdf"
    exit.function <- function () {
        if (exists("opar")) par(opar)
        if (any(names(dev.cur()) == c("postscript", "pdf"))) dev.off()
    }
    on.exit(exit.function())

    load(infile)
    model <- data11$model

    dt <- with(data11$data, tapply(rt, list(sub, soa, cond), mean))

    ## gather parameters for model fits
    attach(data11$fit)
    factors <- list(sub, cond)
    rtime <- tapply(rtime, factors, mean)
    baseRT <- tapply(baseRT, factors, mean)
    detach()

    ## fit model to each subject in each condition
    Subjects <- dimnames(rtime)[[1]]
    predx <- seq(0, 400, by=1)
    predy <- array(dim=c(length(Subjects), length(predx), dim(rtime)[2]),
                   dimnames=list(Subjects, 1:length(predx), dimnames(rtime)[[2]]))
    for (sub in dimnames(predy)[[1]]) {
        for (gt in dimnames(predy)[[3]]) {
            predy[sub, , gt] <- model(predx, rtime[sub, gt], baseRT[sub, gt])
        }
    }

    x <- as.numeric(dimnames(dt)[[2]])
    condList <- dimnames(dt)[[3]]

    ## plot settings
    nCond <- length(condList)
    col <- rainbow(nCond)
    names(col) <- condList
    pch <- c(21, 23, 24)
    names(pch) <- condList

    pdf(outfile, width=8, height=8, pointsize=12)
    opar <- par(mfrow=c(2, 2), las=1, pty="m", cex.axis=.6,
                xpd=NA, bg="white")

    ylim <- c(300, 600)

    counter <- 0
    for (sub in Subjects) {
        matplot(x, dt[sub, , ], type="n", bty="n",
                axes=F,
                xlab="Probe delay (ms)", ylab="Probe RT (ms)",
                main=sprintf("TrackDT02 - %s", sub))
        for (gt in condList) {
            axis(1, x)
            axis(2)
            for (nt in condList) {
                abline(h=baseRT[sub, gt], xpd=F,
                       col=col[gt], lwd=2, lty=3)
                lines(predx, predy[sub, , gt], type="l",
                      col=col[gt], lwd=2, lty=1)
                points(x, dt[sub, , gt], type="p",
                       col=col[gt], bg="transparent", pch=21, cex=1.5, lwd=1)
            }
        }
        if (counter %% 4 == 0) {
            legend("bottomright", sprintf("%s", condList), inset=c(-.4, -.45),
                   bty="n", col=col, lwd=2, ncol=1)
        }
        counter <- counter + 1
    }
}

do.fig1101()
rm(do.fig1101)
