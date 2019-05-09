getChromosome <- function () {
  chromosome <- c(runif(12))
  chromosome <- c(chromosome, as.integer(runif(1, 1, 4)), as.integer(runif(1, 1, 3)), 1, 0)
  return(chromosome)
}

getChromosomes <- function () {
    count <- 0
    chromosomes <- matrix(nrow = 0, ncol = 16)
    while (count < 10) {
        count <- count + 1
        chromosome <- getChromosome()
        chromosomes <- rbind(chromosomes, chromosome)
    }
    return(chromosomes)
}

prepareDataSet <- function(chromosome) {
  secondSection <- head(tail(chromosome, 4), 3)
  
  jobOne <- head(chromosome, 6)
  firstHalfOfJobOne <- head(jobOne, 3)
  secondHalfOfJobOne <- tail(jobOne, 3)
  
  jobTwo <- tail(head(chromosome, 10), 4)
  firstHalfOfJobTwo <- head(jobTwo, 2)
  secondHalfOfJobTwo <- tail(jobTwo, 2)
  
  jobThree <- tail(head(chromosome, 12), 2)
  firstHalfOfJobThree <- head(jobThree, 1)
  secondHalfOfJobThree <- tail(jobThree, 1)
  
  partialChromosome <-
    c(
      firstHalfOfJobOne,
      secondHalfOfJobOne,
      firstHalfOfJobTwo,
      secondHalfOfJobTwo,
      firstHalfOfJobThree,
      secondHalfOfJobThree,
      secondSection,
      0
    )
  machines <- c(1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 2, secondSection, 0)
  jobs <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, secondSection, 0)
  
  return(rbind(partialChromosome, machines, jobs))
}


orderDataSet <- function (chromosome) {
  dataSet <- prepareDataSet(chromosome)
  orderedChromosomes <- order(dataSet['partialChromosome', ])
  
  maximumJobCountForJobOne <- dataSet['partialChromosome', c(13)]
  maximumJobCountForJobOTwo <- dataSet['partialChromosome', c(14)]
  maximumJobCountForJobThree <- dataSet['partialChromosome', c(15)]
  
  selectedJobOneCount <- 0
  selectedJobTwoCount <- 0
  selectedJobThreeCount <- 0
  
  partialChromosomes <- c()
  machines <- c()
  jobs <- c()
  
  for (index in orderedChromosomes) {
    job <- dataSet['jobs',][index]
    
    switch (job,
            {
              if (selectedJobOneCount < maximumJobCountForJobOne) {
                selectedJobOneCount <- selectedJobOneCount + 1
                partialChromosomes <- c(partialChromosomes, dataSet[c('partialChromosome'), index])
                machines <- c(machines, dataSet[c('machines'), index])
                jobs <- c(jobs, dataSet[c('jobs'), index])
              }
            },
            {
              if (selectedJobTwoCount < maximumJobCountForJobOTwo) {
                selectedJobTwoCount <- selectedJobTwoCount + 1
                partialChromosomes <- c(partialChromosomes, dataSet[c('partialChromosome'), index])
                machines <- c(machines, dataSet[c('machines'), index])
                jobs <- c(jobs, dataSet[c('jobs'), index])
              }
            },
            {
              if (selectedJobThreeCount < 2) {
                selectedJobThreeCount <- selectedJobThreeCount + 1
                partialChromosomes <- c(partialChromosomes, dataSet[c('partialChromosome'), index])
                machines <- c(machines, dataSet[c('machines'), index])
                jobs <- c(jobs, dataSet[c('jobs'), index])
              }
            })
  
  }
  
  selectedJobs <- rbind(partialChromosomes, machines, jobs)
  return(selectedJobs)
}

readFromTable <- function(tablePath, rowName, columnName) {
  table <- read.csv(file = tablePath, stringsAsFactors = FALSE)
  return(table[[rowName, columnName]])
}

initialChromosomes <- getChromosomes()

startProcess <<- function(){
  for (r in 1:nrow(initialChromosomes)) {
    chromosome <- initialChromosomes[r,]
    initialChromosomes[r,] <- calculateProcessingTime(chromosome)
  }
  
  initialChromosomes <<- findMinimumsAndStartCrossover(initialChromosomes)
  
  print('chromosome after')
  print(initialChromosomes)
}

calculateProcessingTime <- function(chromosome) {
  orderedDataSet <- orderDataSet(chromosome)
  
  processingTimeForMachineOne <- 0
  processingTimeForMachineTwo <- 0
  lastProcessedJobInMachineOne <- 0
  lastProcessedJobInMachineTwo <- 0
  
  for (c in 1:ncol(orderedDataSet)) {
    machine <- orderedDataSet[2, c]
    job <- orderedDataSet[3, c]
    jobName <- paste('Job', sep = '', job)
    processingTimeForJob <- as.integer(readFromTable('table1.csv', machine, jobName))
    
    if (job != 3) {
      processingTimeForJob <- processingTimeForJob / 2
    }
    
    if(machine == 1){
      
      if(lastProcessedJobInMachineOne == 0){
        processingTimeForMachineOne <- processingTimeForMachineOne + processingTimeForJob
      } else {
        processingTimeValueFromTable <- readFromTable('table2.csv', lastProcessedJobInMachineOne, jobName)
        setupTimeForJobInMachineOne <- if(processingTimeValueFromTable == '-') 0 else as.integer(processingTimeValueFromTable)
        processingTimeForMachineOne <- processingTimeForMachineOne + processingTimeForJob + setupTimeForJobInMachineOne
      }
      
      lastProcessedJobInMachineOne <- job
      
    } else {
      
      if(lastProcessedJobInMachineTwo == 0){
        processingTimeForMachineTwo <- processingTimeForMachineTwo + processingTimeForJob
      }  else {
        processingTimeValueFromTable <- readFromTable('table3.csv', lastProcessedJobInMachineTwo, jobName)
        setupTimeForJobInMachineTwo <- if(processingTimeValueFromTable == '-') 0 else as.integer(processingTimeValueFromTable)
        processingTimeForMachineTwo <- processingTimeForMachineTwo + processingTimeForJob + setupTimeForJobInMachineTwo
      }
      
      lastProcessedJobInMachineTwo <- job
      
    }
    
  }
  
  maximumProcessingTime <- max(processingTimeForMachineOne, processingTimeForMachineTwo)
  chromosome[16] <- maximumProcessingTime
  return(chromosome)
}


findMinimumsAndStartCrossover <- function(chromosomes) {
    orderedChromosomes <- order(chromosomes[, 16])
    minimumChromosome1 <- chromosomes[orderedChromosomes[1],]
    minimumChromosome2 <- chromosomes[orderedChromosomes[2],]
    crossedOver <- doCrossover(minimumChromosome1, minimumChromosome2)
    
    crossedOver1 <- calculateProcessingTime(crossedOver[1,])
    crossedOver2 <- calculateProcessingTime(crossedOver[2,])
    
    chromosomes[orderedChromosomes[10],] <- crossedOver1
    chromosomes[orderedChromosomes[9],] <- crossedOver2
    
    return(chromosomes)
}


doCrossover <- function(chromosome1, chromosome2) {
    genesOfChromosome1 <- head(chromosome1, 12)    
    genesOfChromosome2 <- head(chromosome2, 12)
    
    secondSectionOfChromosome1 <- head(tail(chromosome1, 4), 3)
    secondSectionOfChromosome2 <- head(tail(chromosome2, 4), 3)
    
    newChromosome1 <- c()
    newChromosome2 <- c()
    
    newSecondSection1 <- c()
    newSecondSection2 <- c()
    
    newChromosome1 <- c(newChromosome1, head(genesOfChromosome1, 6))
    newChromosome1 <- c(newChromosome1, tail(genesOfChromosome2, 6))
    
    newSecondSection1 <- c(newSecondSection1, head(secondSectionOfChromosome1, 1))
    newSecondSection1 <- c(newSecondSection1, tail(secondSectionOfChromosome2, 2))
    
    newChromosome2 <- c(newChromosome2, head(genesOfChromosome2, 6))
    newChromosome2 <- c(newChromosome2, tail(genesOfChromosome1, 6))
    
    newSecondSection2 <- c(newSecondSection2, head(secondSectionOfChromosome2, 1))
    newSecondSection2 <- c(newSecondSection2, tail(secondSectionOfChromosome1, 2))
    
    newChromosome1 <- c(newChromosome1, newSecondSection1)
    newChromosome2 <- c(newChromosome2, newSecondSection2)
    
    randomIndexForChromosomeOne <- as.integer(runif(1, 1, 12))
    randomIndexForChromosomeTwo <- as.integer(runif(1, 1, 12))
    randomGeneForChromosomeOne <- runif(1)
    randomGeneForChromosomeTwo <- runif(1)
    
    newChromosome1[randomIndexForChromosomeOne] <- randomGeneForChromosomeOne
    newChromosome2[randomIndexForChromosomeTwo] <- randomGeneForChromosomeTwo
    
    return(rbind(newChromosome1, newChromosome2))
}

for (i in 1:50) {
  startProcess()
}