library(pracma)
library(curry)
library(dplyr)

totalEffort <- function(jobSequence, jobsMachinesMatrix){
  effortMatrix <- matrix(data = 0, nrow = nrow(jobsMachinesMatrix), ncol = ncol(jobsMachinesMatrix)) 
  # Compute the amount of time required for completing the first job by each machine
  effortMatrix[,1] <- cumsum(jobsMachinesMatrix[, jobSequence[1]]) 
  matrix_col_numb <- ncol(jobsMachinesMatrix)
  matrix_row_numb <- nrow(jobsMachinesMatrix)
  # Compute the amount of time needed by the first machine for completing all the jobs
  for (i in 2:matrix_col_numb) {
    effortMatrix[1, i] <- effortMatrix[1, i - 1] + jobsMachinesMatrix[1, jobSequence[i]] # the i-th job execution time is equal to the sum of previous jobs +  the execution time required by the i-th job
  }
  
  for (machine in 2:matrix_row_numb) {
    for (job in 2:matrix_col_numb) {
      # The current machine can execute the given job only when the previous machine machine has finished the current job AND the current machine has finished processing the previous job.
      effortMatrix[machine, job] <- jobsMachinesMatrix[machine, jobSequence[job]] +
        max(effortMatrix[machine -1, job],
            effortMatrix[machine, job - 1])
    }
  }
  
  return(effortMatrix[matrix_row_numb, matrix_col_numb])
}
#Fitness function necessary for using GA library
makespanFitness <- function(jobSequence, ...) {
  return(1/totalEffort(jobSequence, ...))
}


rouletteSelection <- function(scores, pop, number_of_pop){
  idx= sample_n(scores, size = number_of_pop, replace = F, weight = scores$fit)$index
  pop[idx, ]
}

generateOffspring <- function(p1, p2){
  swath = length(p1)/2
  cut1 = sample(length(p1) - swath, 1)
  cut2 = cut1+swath-1
  off1 = rep(0, length(p1))
  sel = p1[cut1:cut2]
  off1[cut1:cut2]= sel
  r = p2[!p2 %in% sel] # not the selected and copied ones
  r1= r[c(1:cut1-1)]
  r2= r[c(cut1:length(r))]
  off1[c(1:cut1-1)] = r1
  off1[c((cut2+1):length(off1))] = r2
  off1
}
generateRandomJobsSequence <- function(sequence_length){
  vet <- c(1:sequence_length)
  return(randperm(vet))
}

generateRandomPopulation <- function(number_of_individuals, sequence_length){
  t(replicate(number_of_individuals,
              generateRandomJobsSequence(sequence_length)))
}
evaluateJobSeq <- function(fitFun, jobSequence){
  fitFun(jobSequence, dataset_jobs_machines)
}

evaluatePopulation <- function(pop){
  data.frame(index =c(1:nrow(pop)), fit = apply(pop, 1, 
                                                curry(evaluateJobSeq, totalEffort)))
}
swapMutation <- function(p, sequenceLength){
  pos = sample(sequenceLength, 2)
  p[c(pos[1], pos[2])] = p[c(pos[2], pos[1])]
  p
}

ga_jobs <- function(number_of_iterations, popIndividuals, number_of_offsprings, crossOverProb = 0.8, wait_interval = 50, mutation_prob = 0.1) {
  ga_results <- data.frame(iteration = vector(),
                           best_score = vector(),
                           worst_score = vector(),
                           avg_score = vector(),
                           median_score = vector())
  nJobs <- ncol(dataset_jobs_machines)
  pop = generateRandomPopulation(number_of_individuals = popIndividuals,
                                 sequence_length = nJobs)
  
  for (i in 1:(number_of_iterations + 1)) {
    
    scores = evaluatePopulation(pop)
    bestScore <- min(scores$fit)
    avgScore <- mean(scores$fit)
    medianScore <- median(scores$fit)
    worstScore <- max(scores$fit)
    ga_results[i,] <- c(i,
                        bestScore,
                        worstScore,
                        avgScore,
                        medianScore)
    if (i > wait_interval && ga_results[i - wait_interval, 2] == ga_results[i, 2]) {
      break() # The algorithm is no longer improving
    }
    # Store the results of the last iteration.
    if (i == number_of_iterations + 1) {
      break()
    }
    worst_elements_indexes <- order(scores$fit, decreasing = T)[1:number_of_offsprings]
    
    best_pop <- rouletteSelection(scores, pop, number_of_pop = number_of_offsprings)
    #crossover
    for (j in 1:(number_of_offsprings/2)) {
      if (runif(1, min = 0, max = 1) < crossOverProb) {
        pop[worst_elements_indexes[j * 2 -1], ]  <- generateOffspring(best_pop[j * 2 -1,], best_pop[j * 2,])
        pop[worst_elements_indexes[j * 2], ]  <- generateOffspring(best_pop[j * 2,], best_pop[j * 2 -1,])
      } else {
        pop[worst_elements_indexes[j * 2 -1], ] <- best_pop[j * 2 -1,]
        pop[worst_elements_indexes[j * 2], ] <- best_pop[j * 2,]
      }
      
    }
    pop_without_best_elements <- setdiff(x = 1:dim(pop)[1],
                                         y = order(scores$fit)[1])
    # Mutation
    for (individual in 1:length(pop_without_best_elements)){
      if (runif(1, min = 0, max=1) < mutation_prob){
        pop[pop_without_best_elements[individual],] <- swapMutation(pop[pop_without_best_elements[individual],], sequenceLength = nJobs)
      }
    }
    
  }
  return(ga_results)
}
