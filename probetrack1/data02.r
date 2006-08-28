### data02 compute d' from data01, and aggregate it by subject, gap duration,
### number of targets, and probe delay
###
### $LastChangedDate$

do.data02 <- function () {
   infile <- "data01.rda";
   outfile <- "data02.rda";
   thisfile <- "data02.r";

   if (!file.exists(infile)) stop("cannot open file ", infile);
   if (IsFileUpToDate(outfile, c(thisfile, infile))) {
      warning("Output file is up to date, no action taken");
      return(invisible(NULL));
   }
   dt <- load(infile);

   ## split data
   targ <- data01$target
   index <- names(data01) != "target";
   ta <- data01[targ == "0", index];
   tp <- data01[targ == "1", index];

   ## create new data table with appropriate IVs and check that everything is ordered correctly
   if (!all(tp$sub == ta$sub & tp$gapdur == ta$gapdur & tp$ntargets == ta$ntargets & tp$soa == ta$soa)) {
      stop("mismatch between target-present and target-absent independent variables");
   }
   data02 <- tp[,c("sub", "gapdur", "ntargets", "soa")];
   rownames(data02) <- as.character(1:dim(data02)[1]);

   ## place basic DVs into data table
   data02$nTA <- ta$nobs;
   data02$nFA <- ta$nobs - ta$ncor;
   data02$nTP <- tp$nobs;
   data02$nHit <- tp$ncor;
   data02$rateFA <- with(data02, nFA / nTA);
   data02$rateHit <- with(data02, nHit / nTP);

   ## correct 0s an 1s
   adjFA <- data02$nFA;
   adjHR <- data02$nHit;
   index <- adjFA == 0; if (any(index)) adjFA[index] <- .5;
   index <- adjFA == data02$nTA; if (any(index)) adjFA[index] <- data02$nTA[index] - .5;
   index <- adjHR == 0; if (any(index)) adjHR[index] <- .5;
   index <- adjHR == data02$nTP; if (any(index)) adjHR[index] <- data02$nTP[index] - .5;
   adjFA <- adjFA / data02$nTA;
   adjHR <- adjHR / data02$nTP;

   ## compute d', criterion, and 95% confidence intervals around d`
   data02$dprime <- qnorm(adjHR) - qnorm(adjFA);
   data02$crit <- -0.5 * (qnorm(adjHR) + qnorm(adjFA));
   phiFA <- 1 / sqrt(2 * pi) * exp(-.5 * qnorm(adjFA));
   phiHR <- 1 / sqrt(2 * pi) * exp(-.5 * qnorm(adjHR));
   data02$ci <- with(data02,
                     1.96 * sqrt(adjHR * (1 - adjHR) / nTP / (phiHR^2) +
                                 adjFA * (1 - adjFA) / nTA / (phiFA^2)));

   save(data02, file=outfile);
   invisible(data02);
}

do.data02();
rm(do.data02);
