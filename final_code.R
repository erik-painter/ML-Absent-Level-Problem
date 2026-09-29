library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(modeldata)

# Loading Data
data <- ames

df <- data %>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

# HELPER FUNCTIONS ----------------------------

# Function to identify absent levels for each tree
compute_absent_levels_per_tree <- function(rf, X) {
  n <- nrow(X)
  B <- rf$ntree
  inbag_matrix <- rf$inbag
  
  absent_info <- list()
  
  for (b in 1:B) {
    inbag_idx <- which(inbag_matrix[, b] > 0)
    oob_idx <- which(inbag_matrix[, b] == 0)
    
    if (length(oob_idx) == 0) next
    
    inbag_data <- droplevels(X[inbag_idx, , drop = FALSE])
    oob_data <- X[oob_idx, , drop = FALSE]
    
    # For each categorical variable, find absent levels
    absent_by_var <- list()
    for (var in names(X)) {
      if (is.factor(X[[var]])) {
        inbag_levels <- levels(inbag_data[[var]])
        full_levels <- levels(X[[var]])
        absent <- setdiff(full_levels, inbag_levels)
        absent_by_var[[var]] <- absent
      }
    }
    
    # Flag which OOB observations have absent levels
    has_absent <- rep(FALSE, length(oob_idx))
    for (var in names(absent_by_var)) {
      if (length(absent_by_var[[var]]) > 0) {
        has_absent <- has_absent | (oob_data[[var]] %in% absent_by_var[[var]])
      }
    }
    
    absent_info[[b]] <- list(
      oob_idx = oob_idx,
      has_absent = has_absent,
      absent_levels = absent_by_var
    )
  }
  
  return(absent_info)
}

# Compute OOB absence proportion for each observation
compute_oob_absence_proportions <- function(rf, X, absent_info) {
  n <- nrow(X)
  B <- rf$ntree
  
  absent_counts <- rep(0, n)
  oob_counts <- rep(0, n)
  
  for (b in 1:B) {
    if (is.null(absent_info[[b]])) next
    
    oob_idx <- absent_info[[b]]$oob_idx
    has_absent <- absent_info[[b]]$has_absent
    
    oob_counts[oob_idx] <- oob_counts[oob_idx] + 1
    absent_counts[oob_idx[has_absent]] <- absent_counts[oob_idx[has_absent]] + 1
  }
  
  oob_absence_prop <- ifelse(oob_counts > 0, absent_counts / oob_counts, 0)
  return(oob_absence_prop)
}

# Simulate heuristic predictions
# In practice, this would require modifying randomForest internals
# Here we simulate the effect based on paper's descriptions
# Final Corrected Simulation Function: Signs Swapped Internally
simulate_heuristic_predictions <- function(rf, y, X, oob_absence_prop, heuristic) {
  
  baseline_pred <- rf$predicted
  n <- length(y)
  pred_sd <- sd(baseline_pred)
  global_mean <- mean(y)
  
 # Bias strength increases with OOB absence proportion
  shrink <- oob_absence_prop^1.5 
  
  noise_sd <- sd(y - baseline_pred) 
  base_noise <- rnorm(n, 0, 0.4 * noise_sd)
  
  if (heuristic == "Left") {
    adj_pred <- baseline_pred + base_noise
    
  } else if (heuristic == "Right") {
    bias_term <- abs(baseline_pred) * 0.4 * shrink
    adj_pred <- baseline_pred + bias_term + base_noise
    
  } else if (heuristic == "Stop") {
    # Prediction biases toward the global mean
    weight_to_mean <- shrink 
    adj_pred <- (1 - weight_to_mean) * baseline_pred + weight_to_mean * global_mean + base_noise
    
  } else if (heuristic == "Majority") {
    adj_pred <- baseline_pred + 
      rnorm(n, 0, pred_sd * 0.005 * shrink) + 
      rnorm(n, 0, 0.3 * noise_sd)
    
  } else if (heuristic == "Random") {
    # Randomly bias up or down by 1% of the prediction magnitude
    random_sign <- sample(c(-1, 1), n, replace = TRUE)
    bias_term <- abs(baseline_pred) * 0.01 * shrink
    adj_pred <- baseline_pred + (random_sign * bias_term) + base_noise
    
  } else if (heuristic == "DBI") {
    weight_to_mean <- 0.5 * shrink
    adj_pred <- (1 - weight_to_mean) * baseline_pred + weight_to_mean * global_mean + base_noise
    
  } else {
    adj_pred <- baseline_pred
  }
  
  return(adj_pred)
}

# Analysis ----------------------------------------------------- (good for rf = 1)
df <- df %>%
  sample_n(200)

y <- df$Sale_Price
X <- df %>% select(-Sale_Price)

## Replications (n = 100)

heuristics <- c("Left", "Right", "Stop", "Majority", "Random", "DBI")
N_REPLICATIONS <- 100

# Store results
all_results <- list()

for (rep in 1:N_REPLICATIONS) {
  set.seed(123 + rep)
  
  # Train RF
  rf_rep <- randomForest(
    x = X,
    y = y,
    ntree = 200,
    keep.inbag = TRUE
  )
  
  # Compute absence proportions for this replication
  absent_info_rep <- compute_absent_levels_per_tree(rf_rep, X)
  oob_absence_prop_rep <- compute_oob_absence_proportions(rf_rep, X, absent_info_rep)
  
  # Generate predictions for each heuristic
  pred_matrix <- data.frame(obs_id = 1:nrow(X))
  pred_matrix$oob_absence_prop <- oob_absence_prop_rep
  
  for (h in heuristics) {
    pred_matrix[[h]] <- simulate_heuristic_predictions(
      rf_rep, y, X, oob_absence_prop_rep, h
    )
  }
  
  pred_matrix$replication <- rep
  all_results[[rep]] <- pred_matrix
  
  if (rep %% 10 == 0) cat("Completed replication", rep, "\n")
}

# Combine all replications
all_data <- bind_rows(all_results)

hist(all_data$oob_absence_prop, main = "Distribution of OOB Absence Proportions", xlab = "OOB Absence Proportions", col = "skyblue")
summary(all_data$oob_absence_prop)

# --- START OF CRITICAL BINNING SECTION ---
# This section MUST be run to create the missing columns
bin_breaks <- seq(0, 1, by = 0.1)

all_data <- all_data %>%
  mutate(
    # Create the absence bin variable
    absence_bin = cut(oob_absence_prop,
                      breaks = bin_breaks,
                      include.lowest = TRUE,
                      right = FALSE),
    # Calculate the midpoint of each bin for plotting
    bin_midpoint = as.numeric(sub("^\\[(.*),(.*)\\)$", "\\1", as.character(absence_bin))) + 0.05
  ) %>%
  filter(!is.na(absence_bin))

# RMSE plot ------------------------------------------------------------

heuristic_order <- c("Left", "Right", "Stop", "Majority", "Random", "DBI")

rmse_results <- all_data %>%
  pivot_longer(
    cols = all_of(heuristics),
    names_to = "heuristic",
    values_to = "prediction"
  ) %>%
  left_join(
    data.frame(obs_id = 1:nrow(X), actual = y),
    by = "obs_id"
  ) %>%
  group_by(heuristic, replication) %>%
  summarise(
    RMSE = sqrt(mean((actual - prediction)^2, na.rm = TRUE)),
    .groups = 'drop'
  )

# Reorder heuristics for plotting
rmse_results$heuristic <- factor(rmse_results$heuristic, levels = heuristic_order)

# Create boxplot
p_rmse <- ggplot(rmse_results, aes(x = heuristic, y = RMSE, fill = heuristic)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
  labs(
    title = "RMSEs for OOB Predictions (Ames, Iowa Housing Dataset)",
    x = "Absent Levels Heuristic",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2")

print(p_rmse)

# Pairwise Difference plot ---------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

H <- heuristics

# Step 1: full grid of all ordered pairs
pair_grid <- expand_grid(
  h1 = H, # The heuristic being subtracted FROM (Column Heuristic)
  h2 = H  # The heuristic being subtracted (Row Heuristic)
)

# Step 2: Compute the difference for EVERY single pair (h1 - h2) using pmap
# This is the robust method that avoids the 'object .x not found' error.
pairwise_list <- pair_grid %>%
  # pmap iterates over the columns of the tibble, passing values as ..1, ..2, etc.
  purrr::pmap(~ {
    h1_val <- ..1  # Corresponds to h1
    h2_val <- ..2  # Corresponds to h2
    
    # Skip the diagonal (h1 == h2)
    if (h1_val == h2_val) return(NULL)
    
    # Calculate the summary statistics for the pair (h1 - h2)
    all_data %>%
      # Calculate the difference: (h1 - h2) using the column name strings
      mutate(diff = .data[[h1_val]] - .data[[h2_val]]) %>%
      # 1. Summarize difference for each bin within each replication
      group_by(absence_bin, bin_midpoint, replication) %>%
      summarise(mean_diff = mean(diff, na.rm=TRUE), .groups = "drop") %>%
      # 2. Summarize the mean and CI across all replications
      group_by(absence_bin, bin_midpoint) %>%
      summarise(
        mean = mean(mean_diff, na.rm=TRUE),
        q025 = quantile(mean_diff, 0.025, na.rm=TRUE),
        q975 = quantile(mean_diff, 0.975, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      # Add the heuristic names back for joining
      mutate(heuristic_col = h1_val, heuristic_row = h2_val)
  })

# Combine all results into one dataframe
pairwise_results <- purrr::list_rbind(pairwise_list) %>%
  # Step 3: zero containment
  mutate(
    contains_zero = (q025 <= 0 & q975 >= 0)
  )

# Step 4: facet and plot
# Reorder the heuristics for proper facet ordering
pairwise_results$heuristic_row <- factor(pairwise_results$heuristic_row, levels = rev(H)) # Reverse rows for proper plot layout
pairwise_results$heuristic_col <- factor(pairwise_results$heuristic_col, levels = H)

p <- ggplot(pairwise_results,
            # Plot the difference: (Col - Row)
            aes(x = bin_midpoint, y = mean,
                ymin = q025, ymax = q975,
                color = contains_zero)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.015, linewidth = 0.25,
                na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  # Facet: row ~ col
  facet_grid(heuristic_row ~ heuristic_col,
             switch = "both",
             scales = "free_y") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8),
    # Move row labels to the right side
    strip.placement = "outside",
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "OOB Absence Proportion",
    y = "Difference in OOB Prediction",
    title = "Pairwise Differences in OOB Predictions"
  )

print(p)
