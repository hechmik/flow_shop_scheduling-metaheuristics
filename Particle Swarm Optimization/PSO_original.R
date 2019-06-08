createParticle <- function(particleLength, x_min = 0, x_max = 4) {
  particle = x_min + (x_max - x_min) * runif(n = particleLength, min = 0, max = 1)
  return(particle)
}

createParticleVelocity <- function(particleLength, v_min = -4, v_max = 4) {
  velocity = v_min + (v_max - v_min) * runif(n = particleLength, min = 0, max = 1)
  return(velocity)
}

createParticlesPopulation <- function(number_of_particles, number_of_jobs) {
  t(replicate(n = number_of_particles,
              expr = createParticle(number_of_jobs)))
}

createParticlesVelocity <- function(number_of_particles, number_of_jobs) {
  t(replicate(n = number_of_particles,
              expr = createParticleVelocity(number_of_jobs)))
}

swapMutation <- function(p, sequenceLength){
  pos = sample(sequenceLength, 2)
  p[c(pos[1], pos[2])] = p[c(pos[2], pos[1])]
  p
}

#SVP
generateJobsFromParticlesPopulation <- function(pop) {
  jobs_seq <- apply(pop, 1, order) # apply order function to each row, so that we will ge
  jobs_seq <- t(jobs_seq)
}

particleSwarm <- function(n, numberOfIterations = 100, decrement_factor = 0.975, inertia_weight = 0.9,) {
  # Number of particles to create
  ro = 2 * n
  #iteration counter
  t = 0 
  #Particles
  pop <- createParticlesPopulation(number_of_particles = ro, number_of_jobs =  n)
  # Velocities
  vel <- createParticlesVelocity(number_of_particles = ro, number_of_jobs =  n)
  # Determine job sequences from particles
  job_sequences <- generateJobsFromParticlesPopulation(pop)
  # Compute makespan values for each job sequence
  makespan_particles_values <- evaluatePopulation(job_sequences)
  # store best results and the related iteration in a support matrix
  best_particles_makespan_iteration_matrix <- cbind(job_sequences,
                                                    makespan_particles_values$fit,
                                                    t)
  job_names <- paste0("Particle", 1:n)
  colnames(best_particles_makespan_iteration_matrix) <- c(job_names,
                                                          "best_makespan",
                                                          "best_iteration")
  # Find and store the best particle
  global_best_particle_index <- order(best_particles_makespan_iteration_matrix[, n + 1])[1]
  global_best_particle_params <- best_particles_makespan_iteration_matrix[global_best_particle_index,] # job seq + makespan + iteration
  c1 <- c2 <- 2 # cognitive and social params
  r1 <- runif(1,0,1)
  r2 <- runif(1,0,1)
  for (t in 1:numberOfIterations) {
    
    #Update intertia
    inertia_weight = inertia_weight * decrement_factor
    if (inertia_weight < 0.4) {
      inertia_weight = 0.4
    }
    #update velocity
    vel <- inertia_weight * vel +
      c1 * r1 * (best_particles_makespan_iteration_matrix[,1:n] - pop ) +
      c2 * r2 * (global_best_particle_params[1:n] - pop)
    # update population
    pop <- pop + vel
    #introduce mutation on 10% of pop
    elements_to_mutate <- sample(1:ro, size = ro/10)
    pop[elements_to_mutate, ] <- swapMutation(pop[elements_to_mutate,], n)
    # Obtain the current job sequence from the current particles
    current_jobs <- generateJobsFromParticlesPopulation(pop)
    # Compute makespan for each particle
    current_makespan <- evaluatePopulation(current_jobs)$fit
    # Find which solutions improved during this iteration
    current_sol_is_better_than_previous_one <- current_makespan < best_particles_makespan_iteration_matrix[, n+1]
    # Update the best results matrix
    for (particle in which(current_sol_is_better_than_previous_one)) {
      best_particles_makespan_iteration_matrix[particle,1:n] <- pop[particle, 1:n] #update the particle elements
      best_particles_makespan_iteration_matrix[particle, n+1] <- current_makespan[particle] # update the makespan value
      best_particles_makespan_iteration_matrix[particle, n+2] <- t # update iteration
    }
    # Update global best results
    current_best_particle_index <- order(best_particles_makespan_iteration_matrix[, n + 1])[1]
    current_best_particle_params <- best_particles_makespan_iteration_matrix[current_best_particle_index,]
    if (current_best_particle_params[n + 1] < global_best_particle_params[n + 1]) {
      global_best_particle_params <- current_best_particle_params # update global result
    }
    
  }
  res <- cbind(global_best_particle_params, "final_iteration" = t)
  return(res)
  
}