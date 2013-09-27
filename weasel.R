## Weasel Program written by CH Chan (chainsawtiney at gmail.com)
## Try to make it as functional as possible. (No mutation)
## Released under GPL-2

errorProneReplicate <- function(parent, rateMutate, charset) {
  p <- runif(length(parent))
  mutants <- sum(p<rateMutate)
  if (mutants) {
    parent[p<rateMutate] <- sample(charset, mutants, replace=TRUE)
  }
  repparent <- parent
  return(repparent)
}
reproduce <- function(parent, numOffspring, rateMutate, charset) {
  offsprings <- replicate(numOffspring, errorProneReplicate(parent, rateMutate, charset), simplify=FALSE)
  return(offsprings)
}
fitnessScoring <- function(offspring, target) {
  fitnessScore <- sum(offspring==target)/length(target)
  return(fitnessScore)
}
naturalSelection <- function(offsprings, target) {
  mostFittedOffspring <- offsprings[[which.max(sapply(offsprings, fitnessScoring, target=target))]]
  return(list(mostFittedOffspring, fitnessScoring(mostFittedOffspring, target)))
}
evolve <- function(parent, numOffspring, rateMutate, charset, target, fitnessScore=0, Gen=1, maxGen=1000) {
  if (fitnessScore == 1 | Gen > maxGen) {
    cat(c("Final: ", parent, " No. of generations:  ", Gen-1, "\n"), sep="")
    return(list(finalFitness=fitnessScore, numGen=Gen-1))
  } else {
    offsprings <- reproduce(parent, numOffspring, rateMutate, charset)
    outcome <- naturalSelection(offsprings, target)
    cat(c("GEN ", Gen, ": ",outcome[[1]], "    ", "score: ", round(outcome[[2]], 3), "\n"), sep="")
    evolve(outcome[[1]], numOffspring, rateMutate, charset,target,fitnessScore=outcome[[2]], Gen=Gen+1, maxGen)
  }
}

evolution <- function(origin=" ", target="METHINKS IT IS LIKE A WEASEL", numOffSpring=100, rateMutate=0.1, maxGen=1000) {
    charset <- c(LETTERS, 0:9, " ")
    if (nchar(target) < nchar(origin)) {
        stop("The length of target cannot be shorter than origin.")
    } else {
        fillup <- paste0(rep(' ', nchar(target) - nchar(origin) ), collapse="")
        origin <- paste0(origin, fillup)
    }
    targetVector <- unlist(strsplit(toupper(target), ""))
    originVector <- unlist(strsplit(toupper(origin), ""))
    return(evolve(originVector, numOffSpring, rateMutate, charset, targetVector, maxGen=maxGen))
}

# producing less offspring
# evolution(origin, 10, 0.05, charset,target)
# Lower mutation rate
# evolution(origin, 100, 0.01, charset,target)
