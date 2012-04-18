### data11.r: fit reacquire-time model to RT by probe delay functions from
### data10, allowing different reacquisition times for each gap type

do.data11 <- function () {
   infile <- "data10.rda"
   outfile <- "data11.rda"
   load(infile)

   ## set-up data matrix
   ##data10$sub <- as.numeric(as.character(data10$sub))
   ##data10$cond <- as.character(data10$cond)
   ##data10$soa <- as.numeric(as.character(data10$soa))
   rownames(data10) <- seq_len(dim(data10)[1])
   data <- data10
   data$rt.pred <- numeric(length(data$rt))

   ## extract IVs
   Subjects <- as.character(levels(data$sub))
   Conditions <- as.character(levels(data$cond))

   ## generate table of IVs, collapsing over SOA
   ivs <- with(data, expand.grid(sub=levels(sub), cond=levels(cond)))

   ## set-up other matrix for storing fit information
   fit <- data.frame(ivs, matrix(NA, nrow=nrow(ivs), ncol=8,
                                 dimnames=list(1:nrow(ivs),
                                   c("rtime", "baseRT",
                                     "iter", "code", "sse", "rmse", "r", "chisq"))))
   fit.columns <- 3:ncol(fit)

   reacquire.model <- function (soa, rtime, baseRT) {
      baseRT + ifelse(rtime - soa > 0, rtime - soa, 0)
   }

   GoodnessOfFit <- function(p) {
      ## parameters: (1) rtime, (2) baseRT
      if (p[1] > 0 && p[2] > 0) {
         sum( (y - reacquire.model(x, rtime=p[1], baseRT=p[2])) ^ 2 )
      } else {
         10^12
      }
   }

   p0 <- c(40, 500)
   names(p0) <- c("rtime", "baseRT")

   for (sub in Subjects) {
       for (gt in Conditions) {
           data.index <- data$sub == sub & data$cond == gt
           fit.index <- fit$sub == sub & fit$cond == gt
           x <- as.numeric(as.character(data[data.index, "soa"]))
           y <- data[data.index, "rt"]
           p0["baseRT"] <- min(y)
           out <- nlm(GoodnessOfFit, p0, print.level=0, steptol=.1)
           p <- out$estimate
           yhat <- reacquire.model(x, p[1], p[2])
           data[data.index, "rt.pred"] <- yhat
           fit[fit.index, fit.columns] <- c(p, out$iterations, out$code,
                                            GoodnessOfFit(p),
                                            sqrt(mean((y - yhat)^2)),
                                            cor(y, yhat),
                                            sum((y - yhat) ^ 2 / yhat))
       }
   }

   data11 <- list(data=data, fit=data.frame(fit), model=reacquire.model)
   save(data11, file=outfile)
}

do.data11()
rm(do.data11)
