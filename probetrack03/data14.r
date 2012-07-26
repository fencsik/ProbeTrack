### data14.r: fit reacquire-time model to RT by probe delay functions from
### data13

do.data14 <- function () {
   thisfile <- "data14.r"
   infile <- "data13.rda"
   outfile <- "data14.rda"

   exit.function <- function () {
      if (exists("old.opt")) options(old.opt)
   }
   on.exit(exit.function())
   old.opt <- options(warn=1)

   if (!file.exists(infile)) stop("cannot open input file ", infile)
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken")
      return(invisible(NULL))
   }
   load(infile)

   ## set-up data matrix
   data13$sub <- as.character(data13$sub)
   data13$soa <- as.numeric(as.character(data13$soa))
   data <- with(data13, aggregate(data.frame(rt=rt),
                                  list(soa=soa, sub=sub), mean))
   data$soa <- as.numeric(as.character(data$soa))
   data$rt.pred <- numeric(length(data$rt))
   Subjects <- as.character(sort(unique(data$sub)))

   ## set-up matrix for storing fit information
   fit <- matrix(NA, nrow=length(Subjects), ncol=8,
                 dimnames=list(Subjects,
                   c("rtime", "baseRT", "iter", "code",
                     "sse", "rmse", "r", "chisq")))
   fit.columns <- 1:ncol(fit)

   reacquire.model <- function (soa, rtime, baseRT) {
      baseRT + ifelse(rtime - soa > 0, rtime - soa, 0)
   }

   GoodnessOfFit <- function(p) {
      ## parameters: (1) rtime, (2) baseRT
###      if (p[1] > 0 && p[2] > 0 && abs(p[3] - min(y)) < min(y) / 2) {
      sum( (y - reacquire.model(x, rtime=p[1], baseRT=p[2])) ^ 2 )
###      } else {
###         10^12
###      }
   }

   p0 <- c(40, 500)
   names(p0) <- c("rtime", "baseRT")

   for (sub in Subjects) {
      data.index <- data$sub == sub
      fit.index <- rownames(fit) == sub
      x <- data[data.index, "soa"]
      y <- data[data.index, "rt"]
      p0["baseRT"] <- min(y)
      out <- nlm(GoodnessOfFit, p0, print.level=0)
      p <- out$estimate
      yhat <- reacquire.model(x, p[1], p[2])
      data[data.index, "rt.pred"] <- yhat
      fit[fit.index, fit.columns] <- c(p, out$iterations, out$code,
                                       GoodnessOfFit(p),
                                       sqrt(mean((y - yhat)^2)),
                                       cor(y, yhat),
                                       sum((y - yhat) ^ 2 / yhat))
   }
   fit <- data.frame(sub=Subjects, fit)
   row.names(fit) <- 1:(dim(fit)[1])

   data14 <- list(data=data, fit=fit, model=reacquire.model)
   save(data14, file=outfile)
}

print(system.time(do.data14(), TRUE))
rm(do.data14)
