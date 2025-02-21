## Monte Carlo Simulation------------------------------------------------------- 
# e)

set.seed(123) 

get_demand <- function () {
rand <- sample(0:99, 2)
if (rand[1] >= 0 & rand[1] <= 29) {
  type_of_demand <- "High"
} else if (rand[1] >= 30 & rand[1] <= 74) {
  type_of_demand <- "Average"
} else {
  type_of_demand <- "Low"
}

# Demand ranges 
demand_ranges <- list(
  list(0:4, 5:14, 15:39, 40:69, 70:89, 90:99),
  list(0:9, 10:29, 30:59, 60:84, 85:94, 95:99),
  list(0:14, 15:39, 40:74, 75:89, 90:94, 95:99)
)

# Function to select the range 
select_range <- function(ranges, value){
  for (i in seq_along(ranges)) {
    if (value %in% ranges[[i]]) {
      return(i)
    }
  }
}
    
if (type_of_demand == "High") {
  index <- select_range(demand_ranges[[1]],rand[2])
} else if (type_of_demand == "Average") {
  index <- select_range(demand_ranges[[2]],rand[2])
} else {
  index <- select_range(demand_ranges[[3]],rand[2])
}

demand_distr <- c(36, 48, 60, 72, 84, 96)
actual_demand <- demand_distr[index]
return(actual_demand)
}


# Different policies on different day samples
policies <- c(36, 48, 60, 72, 84, 96)
results <- data.frame(
  Policy = policies,
  Avg_Profit_10Days = numeric(length(policies)),
  Avg_Profit_50Days = numeric(length(policies)),
  Avg_Profit_10000Days = numeric(length(policies))
)

# Profit parameters
cost <- 0.25
rev <- 0.40
sv_price <- 0.10
lp_cost <- 0.15


# Function to calculate each profit
calculate_profit <- function(demand, baked) {
  revenue <- pmin(demand, baked) * rev
  production_cost <- baked * cost
  lost_profit <- pmax(demand - baked, 0) * lp_cost
  salvage_value <- pmax(baked - demand, 0) * sv_price
  profit <- revenue - production_cost - lost_profit + salvage_value
  return(profit)
}

# Function to run the Simulation 
simulation <- function(total_baked, days) {
  total_profit <- 0
  for (i in 1:days) {
    daily_demand <- get_demand()
    daily_profit <- calculate_profit(daily_demand, total_baked)
    total_profit <- total_profit + daily_profit
  }
  avg_profit <- total_profit / days
  return(avg_profit)
}


# Run the simulation for each policy and time period
for (i in 1:length(policies)) {
  results$Avg_Profit_10Days[i] <- simulation(policies[i], 10)
  results$Avg_Profit_50Days[i] <- simulation(policies[i], 50)
  results$Avg_Profit_10000Days[i] <- simulation(policies[i], 10000)
}

print(results)

# Print the best policy based on the different samples in a readable way
best_policy_10_days <- results$Policy[which.max(results$Avg_Profit_10Days)]
best_policy_50_days <- results$Policy[which.max(results$Avg_Profit_50Days)]
best_policy_10000_days <- results$Policy[which.max(results$Avg_Profit_10000Days)]

{cat("Best policy for 10 days:", best_policy_10_days, "with average profit:", max(results$Avg_Profit_10Days), "\n")
cat("Best policy for 50 days:", best_policy_50_days, "with average profit:", max(results$Avg_Profit_50Days), "\n")
cat("Best policy for 10000 days:", best_policy_10000_days, "with average profit:", max(results$Avg_Profit_10000Days), "\n")
}

## Dynamic Programming----------------------------------------------------------  
# d)
values <- c(40, 80, 20, 100, 65, 60, 70, 45, 60, 60)
weights <- c(4, 8, 4, 10, 8, 10, 5, 5, 6, 4)
volumes <- c(8, 12, 6, 14, 8, 5, 12, 7, 6, 8)
max_weight <- 25  
max_volume <- 40
num_parcels <- length(values)

# Value density per item & sorting
value_density <- values / (weights + volumes)
sorted_indices <- order(value_density, decreasing = TRUE)

# Initialize variables 
total_weight <- 0
total_volume <- 0
total_value <- 0
selected_items <- c()

# Greedy method
for (i in sorted_indices) {
  if (total_weight + weights[i] <= max_weight && total_volume + volumes[i] <= max_volume) {
    selected_items <- c(selected_items, i)
    total_weight <- total_weight + weights[i]
    total_volume <- total_volume + volumes[i]
    total_value <- total_value + values[i]
  }
}

# Print the results
{cat("Total value:", total_value, "\n")
cat("Selected parcels:", selected_items, "\n")
cat("Total weight:", total_weight, "\n")
cat("Total volume:", total_volume, "\n")
}



##########
# e) 
{values <- c(40, 80, 20, 100, 65, 60, 70, 45, 60, 60)
weights <- c(4, 8, 4, 10, 8, 10, 5, 5, 6, 4)
volumes <- c(8, 12, 6, 14, 8, 5, 12, 7, 6, 8)
max_weight <- 25  
max_volume <- 40}  

# Initialize the table
dp <- array(0, dim = c(max_weight + 1, max_volume + 1))

# Dynamic programming solving algorithm 
for (i in 1:length(values)) {
  for (w in max_weight:0) {
    for (v in max_volume:0) {
      if (weights[i] <= w && volumes[i] <= v) {
        dp[w + 1, v + 1] <- max(dp[w + 1, v + 1],
                                dp[w - weights[i] + 1, v - volumes[i] + 1] + values[i])
      }
    }
  }
}

# Optimal value
opt_value <- dp[max_weight + 1, max_volume + 1]

# Selected items
selected_items <- c()
w <- max_weight
v <- max_volume
for (i in length(values):1) {
  if (w >= weights[i] && v >= volumes[i] && 
      dp[w + 1, v + 1] == dp[w - weights[i] + 1, v - volumes[i] + 1] + values[i]) {
    selected_items <- c(selected_items, i)
    w <- w - weights[i]
    v <- v - volumes[i]
  }
}

# Show the results
opt_value
selected_items



#########
# f)
{values <- c(40, 80, 20, 100, 65, 60, 70, 45, 60, 60)
weights <- c(4, 8, 4, 10, 8, 10, 5, 5, 6, 4)
volumes <- c(8, 12, 6, 14, 8, 5, 12, 7, 6, 8)
max_weight <- 25  
max_volume <- 40  
num_parcels <- length(values)}

# Initialize the table
dp <- array(0, dim = c(max_weight + 1, max_volume + 1))

# Dynamic programming solving algorithm 
for (i in 1:num_parcels) {
  for (w in max_weight:0) {
    for (v in max_volume:0) {
      if (weights[i] <= w && volumes[i] <= v) {
        # Exclude parcel
        dp[w + 1, v + 1] <- dp[w + 1, v + 1]
        
        # Include parcel once
        if (w >= weights[i] && v >= volumes[i]) {
          dp[w + 1, v + 1] <- max(dp[w + 1, v + 1], dp[w - weights[i] + 1, v - volumes[i] + 1] + values[i])
        }
        
        # Include parcel twice
        if (w >= 2 * weights[i] && v >= 2 * volumes[i]) {
          dp[w + 1, v + 1] <- max(dp[w + 1, v + 1], dp[w - 2 * weights[i] + 1, v - 2 * volumes[i] + 1] + 2 * values[i])
        }
      }
    }
  }
}

# Optimal value
opt_value <- dp[max_weight + 1, max_volume + 1]

# Selected items
selected_items <- c()
item_counts <- rep(0, num_parcels)
w <- max_weight
v <- max_volume
for (i in num_parcels:1) {
  if (w >= weights[i] && v >= volumes[i] && dp[w + 1, v + 1] == dp[w - weights[i] + 1, v - volumes[i] + 1] + values[i]) {
    item_counts[i] <- 1
    selected_items <- c(selected_items, i)
  } else if (w >= 2 * weights[i] && v >= 2 * volumes[i] && dp[w + 1, v + 1] == dp[w - 2 * weights[i] + 1, v - 2 * volumes[i] + 1] + 2 * values[i]) {
    item_counts[i] <- 2
    selected_items <- c(selected_items, i)
  }
}

# Show the results
{cat("Optimal value:", opt_value, "\n")
cat("Selected parcels:", selected_items, "\n")}

########
# g)
{values <- c(40, 80, 20, 100, 65, 60, 70, 45, 60, 60)
weights <- c(4, 8, 4, 10, 8, 10, 5, 5, 6, 4)
volumes <- c(8, 12, 6, 14, 8, 5, 12, 7, 6, 8)
max_weight <- 25
max_volume <- 40
num_parcels <- length(values)
selected_items <- c()
item_counts <- rep(0, num_parcels)
w <- max_weight
v <- max_volume}

# DP Function
solveKnapsack <- function(values, weights, volumes, max_weight, max_volume, max_number) {
  dp <- array(0, dim = c(max_weight + 1, max_volume + 1))
  
  # Dynamic programming algorithm
  for (i in 1:num_parcels) {
    for (w in max_weight:0) {
      for (v in max_volume:0) {
        # Option 1: Exclude the item
        dp[w + 1, v + 1] <- dp[w + 1, v + 1]
        
        # Option 2: Include the item k times (k ranges from 1 to max_number)
        for (k in 1:max_number) {
          if (weights[i] * k <= w && volumes[i] * k <= v) {
            dp[w + 1, v + 1] <- max(dp[w + 1, v + 1], dp[w - k * weights[i] + 1, v - k * volumes[i] + 1] + k * values[i])
          }
        }
      }
    }
  }
  
  # Backtracking to find the selected items
  selected_items <- c()
  item_counts <- rep(0, num_parcels)
  w <- max_weight
  v <- max_volume
  for (i in num_parcels:1) {
    for (k in max_number:1) {
      if (w >= weights[i] * k && v >= volumes[i] * k && 
          dp[w + 1, v + 1] == dp[w - k * weights[i] + 1, v - k * volumes[i] + 1] + k * values[i]) {
        item_counts[i] <- k
        selected_items <- c(selected_items, i)
        w <- w - k * weights[i]
        v <- v - k * volumes[i]
        break
      }
    }
  }
  
  # Return results
  return(list(optimal_value = dp[max_weight + 1, max_volume + 1], selected_items = selected_items, item_counts = item_counts))
}

# Range of times items are available 
max_numbers <- 1:7 

# Solve for each max_number
results <- lapply(max_numbers, function(max_number) {
  solveKnapsack(values, weights, volumes, max_weight, max_volume, max_number)
})

# Print results for each max_number
for (i in seq_along(max_numbers)) {
  cat("Max number:", max_numbers[i], "\n")
  cat("Optimal value:", results[[i]]$optimal_value, "\n")
  cat("Selected parcels:", results[[i]]$selected_items, "\n")
  cat("Counts:", results[[i]]$item_counts[results[[i]]$selected_items], "\n")
  cat("Total weight:", sum(results[[i]]$item_counts[results[[i]]$selected_items] * weights[results[[i]]$selected_items]), "\n")
  cat("Total volume:", sum(results[[i]]$item_counts[results[[i]]$selected_items] * volumes[results[[i]]$selected_items]), "\n")
  cat("\n")
}

  
