### an01.r: lists which subjects were in which experiment
###
### $Id$

f.an01 <- function() {
    infiles <- c("../../probetrack1/data01.rda",
                 "../../probetrack2/data01.rda",
                 "../../probetrack3/data01.rda",
                 "../../probetrack6b/data01.rda"
                 );
    demofile <- "demographics.txt";
    outfile <- "an01out.txt"

    exit.function <- function () {
        while (sink.number() > 0) sink()
    }
    on.exit(exit.function())

    if (!file.exists(demofile)) stop(demofile, " not found")
    dg <- read.csv("demographics.txt")

    ## Extract experiment names from infile paths.
    nexp <- length(infiles)
    splits <- strsplit(infiles, "/")

    ## Extract all subjects and the experiments they ran in.
    exp.names <- character(nexp)
    sublist <- NULL
    explist <- NULL
    for (i in 1:nexp) {
        exp.names[i] <- splits[[i]][3]
        if (!file.exists(infiles[i])) stop(infiles[i], " not found")
        load(infiles[i])
        subs <- unique(as.character(data01$sub))
        sublist <- c(sublist, subs)
        explist <- c(explist, rep(exp.names[i], length(subs)))
    }

    subs <- sort(unique(sublist))
    nsubs <- length(subs)
    names(explist) <- sublist

    ## Record demographics for all the subjects run in the various experiments,
    ## along with the experiments in which they participated.
    dem <- array(NA, dim = c(nsubs, 2 + nexp),
                 dimnames = list(subs,
                   c("sex", "age", exp.names)))
    for (s in subs) {
        if (s %in% dg$Subject) {
            dem[s, c("sex")] <- dg[match(s, dg$Subject), c("Gender")]
            dem[s, c("age")] <- dg[match(s, dg$Subject), c("Age")]
        }
        subexp <- explist[names(explist) == s]
        dem[s, exp.names] <- 0
        for (x in subexp) {
            dem[s, x] <- 1
        }
    }

    ## Generate report of the experiments each subject participated in, along
    ## with the total number of experiments per subject.
    expcount <- array(0, dim = c(nsubs, nexp+1),
                      dimnames = list(subs, c(exp.names, "count")))
    expcount[, exp.names] <- dem[, exp.names]
    expcount[, "count"] <- apply(expcount[, exp.names], 1, sum)

    ## Demographics summary for each experiment
    dm <- array(0, dim = c(nexp, 7),
                dimnames = list(exp.names,
                  c("ageMean", "ageMin", "ageMax", "ageMissing",
                    "nFemale", "nMale", "nMissing")))
    print(dm)
    for (x in exp.names) {
        index <- dem[, x] > 0
        ages <- dem[index, "age"]
        dm[x, "ageMean"] <- round(mean(ages, na.rm = TRUE), 1)
        dm[x, "ageMin"] <- min(ages, na.rm = TRUE)
        dm[x, "ageMax"] <- max(ages, na.rm = TRUE)
        dm[x, "ageMissing"] <- sum(is.na(ages))
        genders <- as.character(dem[index, "sex"])
        dm[x, "nFemale"] <- sum(genders == 1, na.rm = TRUE)
        dm[x, "nMale"] <- sum(genders == 2, na.rm = TRUE)
        dm[x, "nMissing"] <- sum(is.na(genders))
    }

    sink(outfile)

    print(expcount)

    cat("\n\nAge and gender distributions\n");
    print(dm);

    cat("\nTotal number of subjects = ", nsubs, "\n", sep="")
    cat("Total number of experiments = ", nexp, "\n\n", sep="")

    numcounts <- 5
    out2 <- matrix(0, nrow=numcounts, ncol=1)
    rownames(out2) <- 1:numcounts
    colnames(out2) <- "count"
    for (i in 1:numcounts) {
        out2[i,1] <- sum(expcount[,nexp+1] == i)
    }


    print(out2)
}

f.an01()
rm(f.an01)
