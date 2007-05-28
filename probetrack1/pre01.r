### pre01.r: plots predictions for reacquisition time model
###
### $LastChangedDate$

do.pre01 <- function () {
   outfile <- "data01.pdf";
   thisfile <- "data01.r";

   if (IsFileUpToDate(outfile, c(thisfile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }

### Define model function
   reacquire.model <- function (soa, rtime, baseRT) {
      if (length(rtime) > 1 && length(baseRT) > 1 &&
          length(rtime) != length(baseRT))
         stop("number of elements in rtime and baseRT must match or equal 1");
      n <- max(length(rtime), length(baseRT));
      if (n > 1) {
         if (length(rtime) == 1) rtime <- rep(rtime, n);
         if (length(baseRT) == 1) baseRT <- rep(baseRT, n);
         out <- matrix(0, nrow = length(soa), ncol = n,
                       dimnames = list(as.character(soa), paste(rtime, baseRT, sep=".")));
         i <- 1;
         for (i in 1:n) {
            out[,i] <- baseRT[i] + ifelse(rtime[i] - soa > 0, rtime[i] - soa, 0);
            i <- i + 1;
         }
      } else {
         out <- baseRT + ifelse(rtime - soa > 0, rtime - soa, 0);
      }
      out
   }

   rtime <- c(0, 40, 80, 120);
   baseRT <- c(500, 600, 700, 800);
   baseRT <- 500;
   soa <- seq(0, 600, by = 20);
   rt <- reacquire.model(soa, rtime, baseRT);
   print(rt);

   matplot(soa, rt, type = "l", ylim = c(500, 800),
           col = c("red", "blue", "green3", "cyan"), lty = 1, lwd = 3);
}

do.pre01();
rm(do.pre01);
