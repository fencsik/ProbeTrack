### tab1101: generate table of parameters of fitted model for each subject

do.tab1101 <- function () {
    infile <- "data11.rda"
    outfile <- "tab1101.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    load(infile)

    dt <- data11$fit
    dt$sub <- as.character(dt$sub)
    dt$cond <- as.character(dt$cond)
    dt$r.sq <- dt$r^2
    category <- c(with(dt, sprintf("%s-%s", sub, cond)), "MEAN")
    dt <- dt[, -(1:2)]
    dt <- rbind(dt, apply(as.matrix(dt), 2, mean))
    rownames(dt) <- category

    sink(outfile)
    cat("Parameter estimates from model fit\n")
    print(round(dt[, c("rtime", "baseRT")], 3))

    cat("\n\n\n")
    cat("Fitting output and goodness-of-fit statistics\n")
    print(round(dt[, c("iter", "code", "sse", "rmse", "r.sq", "chisq")], 3))

    dt <- data11$fit
    cat("\n\n\n")
    cat("Average rtime, with 95% confidence intervals\n")
    y <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(cond), mean))
    ci <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(cond),
                            function(x) qt(.975, length(x) - 1) * sqrt(var(x) / length(x))))
    for (gd in dimnames(y)[[1]]) {
        cat(sprintf("Gap %3s: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", gd, y[gd], ci[gd],
                    y[gd] - ci[gd], y[gd] + ci[gd]))
    }
    y <- dt$rtime[rownames(dt) != "MEAN"]
    ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y))
    cat(sprintf("   MEAN: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
                mean(y) - ci, mean(y) + ci))

    cat("\n\n\n")
    cat("Average baseRT, with 95% confidence intervals\n")
    y <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(cond), mean))
    ci <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(cond),
                            function(x) qt(.975, length(x) - 1) * sqrt(var(x) / length(x))))
    for (gd in dimnames(y)[[1]]) {
        cat(sprintf("Gap %3s: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", gd, y[gd], ci[gd],
                    y[gd] - ci[gd], y[gd] + ci[gd]))
    }
    y <- dt$rtime[rownames(dt) != "MEAN"]
    ci <- qt(.975, length(y) - 1) * sqrt(var(y) / length(y))
    cat(sprintf("   MEAN: %0.2f +/- %0.2f (%0.2f, %0.2f)\n", mean(y), ci,
                mean(y) - ci, mean(y) + ci))

    cat("\n\n\n")
    cat("Goodness of fit statistics for averaged fit\n")
    obsey <- with(data11$data, aggregate(rt, list(soa, cond), mean))$x
    predy <- with(data11$data, aggregate(rt.pred, list(soa, cond), mean))$x
    cat(sprintf("R-squared = %0.5f\n", cor(obsey, predy) ^ 2))

    cat("\n\n\n")
    cat("Goodness of fit statistics for fit from averaged parameters\n")
    m.rtime <- with(dt[rownames(dt) != "MEAN", ], tapply(rtime, list(cond), mean))
    m.baseRT <- with(dt[rownames(dt) != "MEAN", ], tapply(baseRT, list(cond), mean))
    obse <- with(data11$data, tapply(rt, list(soa, cond), mean))
    pred <- with(data11$data, tapply(rt.pred, list(soa, cond), mean))
    x <- as.numeric(dimnames(obse)[[1]])
    for (gd in dimnames(m.rtime)[[1]]) {
        pred[, gd] <- data11$model(x, m.rtime[gd], m.baseRT[gd])
    }
    cat(sprintf("R-squared = %0.5f\n", cor(as.vector(obse), as.vector(pred)) ^ 2))
}

do.tab1101()
rm(do.tab1101)
