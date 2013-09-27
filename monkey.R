## hurricane or Shakesphere's monkeys
## written by CH Chan <chainsawtiney at gmail.com>
## release under GPL-2

hurricane <- function(origin=" ", target="METHINKS IT IS LIKE A WEASEL", maxGen=10000) {
    charset <- c(LETTERS, 0:9, " ")
    if (nchar(target) < nchar(origin)) {
        stop("The length of target cannot be shorter than origin.")
    } else {
        fillup <- paste0(rep(' ', nchar(target) - nchar(origin) ), collapse="")
        origin <- paste0(origin, fillup)
    }
    curGen <- 1
    offspring <- toupper(origin)
    target <- toupper(target)
    while(offspring != target & curGen <= maxGen) {
        offspring <- paste0(sample(charset, nchar(target), replace=TRUE), collapse="")
        cat("Gen ", curGen , ": ", offspring, "\n")
        curGen <- curGen + 1
    }
    cat("Final: ", offspring, "No. of generations: ", curGen - 1, "\n")
}
