### an01.r: lists which subjects were in which experiment
###
### $Id$

do.an01 <- function() {
   infiles <- c("../../probetrack1/data01.rda",
                "../../probetrack2/data01.rda",
                "../../probetrack3/data01.rda",
                "../../probetrack4/data01.rda",
                "../../probetrack5/data01.rda");
   demofile <- "demographics.txt";
   outfile <- "an01out.txt"

   exit.function <- function () {
      while (sink.number() > 0) sink();
   }
   on.exit(exit.function());

   if (!file.exists(demofile)) stop(demofile, " not found");
   dg <- read.csv("demographics.txt");

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

   subs <- sort(unique(sublist));
   nsubs <- length(subs);

   if (nsubs != dim(dg)[1]) {
      stop(demofile, " has ", dim(dg)[1], " subjects, but there are ",
           nsubs,", subjects across all the experiments");
   }

   output <- matrix(0, nrow=nsubs, ncol=nexp+1, dimnames = list(subs, c(exp.names, "count")));

   for (i in 1:nsubs) {
      output[i, explist[subs[i] == sublist]] <- 1
      output[i, nexp+1] <- sum(output[i, 1:nexp])
      output[i, explist[subs[i] == sublist]] <- 11
   }

   print(output)

   cat("\n\nAge and gender distributions\n");
   dm <- matrix(0, nrow = nexp, ncol = 7,
                dimnames = list(exp.names, c("ageMean", "ageMin", "ageMax", "ageMissing",
                  "nFemale", "nMale", "nMissing")));
   for (i in 1:nexp) {
      index <- output[,i] > 0;
      ages <- dg[index, "Age"];
      dm[i, "ageMean"] <- round(mean(ages, na.rm = T), 1);
      dm[i, "ageMin"] <- min(ages, na.rm = T);
      dm[i, "ageMax"] <- max(ages, na.rm = T);
      dm[i, "ageMissing"] <- sum(is.na(ages));
      genders <- as.character(dg[index, "Gender"]);
      dm[i, "nFemale"] <- sum(genders == "F");
      dm[i, "nMale"] <- sum(genders == "M");
      dm[i, "nMissing"] <- sum(is.na(genders));
   }

   print(dm);

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
}

do.an01()
rm(do.an01)
