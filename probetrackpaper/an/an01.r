### an01.r: lists which subjects were in which experiment
###
### $Id$

do.an01 <- function() {
   infiles <- c("../../probetrack1/data01.rda",
                "../../probetrack2/data01.rda",
                "../../probetrack3/data01.rda",
                "../../probetrack4/data01.rda");
   outfile <- "an01out.txt"

   ## extract experiment names from infile paths
   nexp <- length(infiles)
   splits <- strsplit(infiles, "\/")
   exp.names <- character(nexp)
   sublist <- NULL
   explist <- NULL
   for (i in 1:nexp) {
      exp.names[i] <- splits[[i]][3]
      if (!file.exists(infiles[i])) stop(infiles[i], " not found")
      load(infiles[i])
      subs <- unique(as.character(data01$sub))
      sublist <- c(sublist, subs)
      explist <- c(explist, rep(i, length(subs)))
   }

   sink(outfile)

   subs <- sort(unique(sublist))
   nsubs <- length(subs)

   output <- matrix(0, nrow=nsubs, ncol=nexp+1)
   rownames(output) <- subs
   colnames(output) <- c(exp.names, "count")

   for (i in 1:nsubs) {
      output[i, explist[subs[i] == sublist]] <- 1
      output[i, nexp+1] <- sum(output[i, 1:nexp])
      output[i, explist[subs[i] == sublist]] <- 11
   }

   print(output)

   cat("\nTotal number of subjects = ", nsubs, "\n", sep="")
   cat("Total number of experiments = ", nexp, "\n\n", sep="")

   numcounts <- 5
   out2 <- matrix(0, nrow=numcounts, ncol=1)
   rownames(out2) <- 1:numcounts
   colnames(out2) <- "count"
   for (i in 1:numcounts) {
      out2[i,1] <- sum(output[,nexp+1] == i)
   }

   print(out2)

   sink()
}

do.an01()
rm(do.an01)
