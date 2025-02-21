setwd("/Library/gurobi1102/macos_universal2/R")
install.packages('gurobi_11.0-2_R_4.4.0.tgz', repos=NULL)
install.packages('slam')
library(gurobi)

setwd("~/Desktop/IBA 3/Prescriptive Data Analytics")
## Question 1 ------------------------------------------------------------------
model1 <- list()

model1$A <- rbind(
  c(90, 2, 4, 8, 50, 320),    # Constraint 1
  c(90, 2, 4, 8, 50, 320),    # Constraint 1
  c(234, 15, 0, 23, -4, 20),  # Constraint 2
  c(60, 45, 12, 9, 35, 0)- c(30, 56, 7, 20, 34, 50), # Constraint 3
  c(0, 24, 5, 12, 34, 0)- (2/3)*c(90, 2, 4, 8, 50, 320),    # Constraint 4
  c(30, 32, 2, 8, 0, 50)- c(234, 15, 0, 23, -4, 20)) # Constraint 5
model1$rhs <- c(460, 500, 616, 0, 0, 30)
model1$sense <- c(">", "<", "<=", ">", "<=", ">=")
model1$obj <- c(30, 10, 5, 20, 74, 101)
model1$modelsense <- 'min'
model1$vtype <- 'C'
params = list(OutputFlag = 0)
result1 <- gurobi(model1, params)

# Print the results
print(result1$objval)
print(result1$x)


## Question 2 ------------------------------------------------------------------
#c) ____________________________________________________________________________

#Define model and matrix
model2 <- list()
  model2$A <- rbind(
    c(1,0,0,0,1,0,0,0,1,0,0,0),
    c(0,1,0,0,0,1,0,0,0,1,0,0),
    c(0,0,1,0,0,0,1,0,0,0,1,0),
    c(0,0,0,1,0,0,0,1,0,0,0,1),
    c(1,1,1,1,0,0,0,0,0,0,0,0),
    c(0,0,0,0,1,1,1,1,0,0,0,0),
    c(0,0,0,0,0,0,0,0,1,1,1,1),
    c(1,1,1,1,1,1,1,1,1,1,1,1))
model2$obj <- c(14, 13, 10, 19, 15, 19, 14, 19, 20, 18, 13, 14)
model2$modelsense <- 'min'
model2$rhs <- c(66,62,88,74,150,125,100,0)
model2$sense <- c('>=', '>=', '>=', '>=', '<=', '<=', '<=', '>=')
model2$vtype <- 'C'
params <- list(OutputFlag=0)
result2 <- gurobi(model2, params)

# Print the results
print(result2$objval)
print(result2$x)

## Question 3 ------------------------------------------------------------------
install.packages("tidyr")
install.packages("readr")
library(tidyr)
library(readr)

# read knapsack data in the correct format, function
read_knapsack <- function(readknapsack) {
  data <- readLines(readknapsack)
# clean and categorise each line of data 
 instances <- lapply(data, function(line) {
    parts <- unlist(strsplit(gsub("[()]", "", line), ","))
    W <- as.numeric(parts[1])
    V <- as.numeric(parts[2])
    n <- as.numeric(parts[3])
    items <- matrix(as.numeric(parts[4:length(parts)]), ncol = 3, byrow = TRUE)
    list(W = W, V = V, n = n, items = items)
  })
 return(instances)
}

#a) ____________________________________________________________________________

# Function to solve a single knapsack instance
solve_knapsack <- function(W, V, n, items) {
  profits <- items[, 1]
  weights <- items[, 2]
  volumes <- items[, 3]

  model3 <- list() 
  model3$A <- rbind(
    c(weights),
    c(volumes))
  
  model3$obj <- c(profits)
  model3$modelsense <- 'max'
  model3$rhs <- c(W, V)
  model3$sense <- c("<=","<=")
  model3$vtype <- 'B'
  params <- list(OutputFlag=0)
  result3 <- gurobi(model3, params)

  chosen_items <- which(result3$x > 0.5)
  total_profit <- result3$objval
  total_weight <- sum(weights[chosen_items])
  total_volume <- sum(volumes[chosen_items])
  
  return(list(
    total_profit = total_profit,
    total_weight = total_weight,
    total_volume = total_volume,
    W = W,
    V = V,
    chosen_items = chosen_items
    ))
}

# Function to solve all knapsack instances (applying to each instances) 
# and return the required information (weight, volume and profit)
solve_all_knapsacks <- function(instances) {
  results <- lapply(instances, function(instance) {
    W <- instance$W
    V <- instance$V
    n <- instance$n
    items <- instance$items
    result <- solve_knapsack(W, V, n, items)
    cat("Instance:\n")
    cat("  Objective Value (Total Profit):", result$total_profit, "\n")
    cat("  Total Weight of Chosen Items:", result$total_weight, "\n")
    cat("  Total Volume of Chosen Items:", result$total_volume, "\n")
    cat("  Weight Limit (W):", result$W, "\n")
    cat("  Volume Limit (V):", result$V, "\n")
    cat("  Chosen Items:", result$chosen_items, "\n")
    cat("\n")
    
    return(result)
  })
  return(results)
}

# Solve all instances 
instances <- read_knapsack("knapsack (1).txt")
results <- solve_all_knapsacks(instances)

#b) ____________________________________________________________________________

# Function to solve with the greedy algorithm 
greedy_knapsack <- function(p, w, v, W, V, sortMethod) {
  if (sortMethod == "p") {
    sortVector <- p
  } else if (sortMethod == "w") {
    sortVector <- -w
  } else if (sortMethod == "v") {
    sortVector <- -v
  } else if (sortMethod == "p/w") {
    sortVector <- p / w
  } else if (sortMethod == "p/v") {
    sortVector <- p / v
  } else {
    stop("Unknown sort method")
  }
  
  sortIndices <- sort(sortVector, decreasing = TRUE, index.return = TRUE)$ix
  currentWeight <- 0
  currentVolume <- 0
  currentProfit <- 0
  x <- numeric(length(p))
  
  for (i in 1:length(p)) {
    if (currentWeight + w[sortIndices[i]] <= W && currentVolume + v[sortIndices[i]] <= V) {
      x[sortIndices[i]] <- 1
      currentWeight <- currentWeight + w[sortIndices[i]]
      currentVolume <- currentVolume + v[sortIndices[i]]
      currentProfit <- currentProfit + p[sortIndices[i]]
    }
  }
  
  result <- list()
  result$x <- x
  result$p <- currentProfit
  result$w <- currentWeight
  result$v <- currentVolume
  return(result)
}

# Function to solve all knapsack instances, and chose the best sort-method (highest profit)
solve_greedy <- function(instances) {
results <- lapply(instances, function(instance) {
  sortMethods <- c("p", "v", "w", "p/w", "p/v")
  best_profit <- 0 
  best_result <- 0
  for(method in sortMethods) {
    W <- instance$W
    V <- instance$V
    n <- instance$n
    items <- instance$items
    p <- items[, 1]
    w <- items[, 2]
    v <- items[, 3]
    result <- greedy_knapsack(p, w, v, W, V, method)
    if (result$p > best_profit) {
      best_profit <- result$p
      best_result <- result
    }  
  }
  return(best_result)
})

# Print the highest profit and the selected items to get to this profit
lapply(results, function(result) {
  cat("Objective Value (Total Profit):", result$p, "\n")
  cat("Total Weight of Chosen Items:", result$w, "\n")
  cat("Total Volume of Chosen Items:", result$v, "\n")
  cat("Chosen Items:", which(result$x == 1), "\n")
  cat("\n")
})
}

# Read knapsack instances from file (assuming read_knapsack function is defined)
instances <- read_knapsack("knapsack (1).txt")
solve_greedy(instances)

## Question 4 ------------------------------------------------------------------
read_problem_instances <- function(file) {
  lines <- readLines(file)
  instances <- list()
  for (line in lines) {
    parts <- strsplit(line, ",")[[1]]
    n <- as.integer(parts[1])
    k <- as.integer(parts[2])
    parts <- strsplit(line, ",\\{")[[1]]
    sets <- lapply(parts[2:length(parts)], function(x) {
      as.integer(unlist(strsplit(gsub("\\}", "", x), ",")))
    })
    instances <- append(instances, list(list(n = n, k = k, sets = sets)))
  }
  return(instances)
}

instances <- read_problem_instances("setProblem (1).txt")

#b) ____________________________________________________________________________
# Greedy algorithm to solve the problem
greedy_algorithm <- function(n, k, sets) {
  chosen_sets <- list()
  unique_elements <- list()
  
  while (length(chosen_sets) < k) {
    best_set <- NULL
    best_new_elements <- 0
    
    for (set in sets) {
      new_elements <- length(union(set, unique_elements))
      if (new_elements > best_new_elements) {
        best_new_elements <- new_elements
        best_set <- set
      }
    }
    
    chosen_sets <- append(chosen_sets, list(best_set))
    unique_elements <- union(unique_elements, best_set)
    sets <- sets[!sapply(sets, function(x) all(x %in% best_set))]
  }
  
  return(list(chosen_sets = chosen_sets, unique_elements = unique_elements))
}

# Function to solve all problem instances using the greedy algorithm
solve_all_greedy <- function(file) {
  instances <- read_problem_instances(file)
  results <- list()
  
  for (instance in instances) {
    result <- greedy_algorithm(instance$n, instance$k, instance$sets)
    results <- append(results, list(result))
  }
  
  return(results)
}


# Apply the function and present the results
results <- solve_all_greedy("setProblem (1).txt")
for (i in 1:length(results)) {
  result <- results[[i]]
  cat("Instance", i, "\n")
  cat("Chosen Sets:", sapply(result$chosen_sets, function(x) paste0("{", paste(x, collapse = ", "), "}")), "\n")
  cat("Number of Unique Elements:", length(result$unique_elements), "\n")
  cat("\n")
}


#d) ____________________________________________________________________________
# Local search algorithm to improve the solution
local_search_algorithm <- function(n, k, sets, initial_solution) {
  chosen_sets <- initial_solution$chosen_sets
  unique_elements <- initial_solution$unique_elements
  improved <- TRUE
  
  while (improved) {
    improved <- FALSE
    for (i in 1:length(chosen_sets)) {
      for (set in sets) {
        new_chosen_sets <- chosen_sets
        new_chosen_sets[[i]] <- set
        new_unique_elements <- unique(unlist(new_chosen_sets))
        
        if (length(new_unique_elements) > length(unique_elements)) {
          chosen_sets <- new_chosen_sets
          unique_elements <- new_unique_elements
          improved <- TRUE
        }
      }
    }
  }
  
  return(list(chosen_sets = chosen_sets, unique_elements = unique_elements))
}

# Function to solve all problem instances using the local search algorithm
solve_all_local_search <- function(file, initial_results) {
  instances <- read_problem_instances(file)
  results <- list()
  
  for (i in 1:length(instances)) {
    instance <- instances[[i]]
    initial_solution <- initial_results[[i]]
    result <- local_search_algorithm(instance$n, instance$k, instance$sets, initial_solution)
    results <- append(results, list(result))
  }
  
  return(results)
}

# Apply the function and show the results by comparing them to the greedy output 
local_search_results <- solve_all_local_search("setProblem (1).txt", results)
for (i in 1:length(local_search_results)) {
  initial_result <- results[[i]]
  local_search_result <- local_search_results[[i]]
  
  cat("Instance", i, "\n")
  cat("Initial Number of Unique Elements:", length(initial_result$unique_elements), "\n")
  cat("Local Search Number of Unique Elements:", length(local_search_result$unique_elements), "\n")
  cat("Initial Chosen Sets:", sapply(initial_result$chosen_sets, function(x) paste0("{", paste(x, collapse = ", "), "}")), "\n")
  cat("Local Search Chosen Sets:", sapply(local_search_result$chosen_sets, function(x) paste0("{", paste(x, collapse = ", "), "}")), "\n")
  cat("Improvement:", length(local_search_result$unique_elements) > length(initial_result$unique_elements), "\n")
  cat("\n")
}

