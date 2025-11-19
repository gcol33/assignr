# ==============================================================================
# Demonstration of auto_scale feature in match_couples()
# ==============================================================================

library(couplr)

# ==============================================================================
# Example 1: Basic auto_scale usage
# ==============================================================================

cat("Example 1: Basic auto_scale usage\n")
cat("===================================\n\n")

set.seed(123)

# Create datasets with variables on different scales
left <- data.frame(
  id = 1:50,
  age = rnorm(50, mean = 45, sd = 10),        # Range: ~15-75
  income = rnorm(50, mean = 50000, sd = 15000), # Range: ~5000-95000
  bp_systolic = rnorm(50, mean = 120, sd = 15) # Range: ~75-165
)

right <- data.frame(
  id = 51:120,
  age = rnorm(70, mean = 47, sd = 10),
  income = rnorm(70, mean = 52000, sd = 15000),
  bp_systolic = rnorm(70, mean = 122, sd = 15)
)

# Without auto_scale - income dominates the distance calculation
result_no_auto <- match_couples(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  auto_scale = FALSE
)

cat("Without auto_scale:\n")
cat(sprintf("  Mean distance: %.2f\n", mean(result_no_auto$pairs$distance)))
cat(sprintf("  Total distance: %.2f\n\n", result_no_auto$info$total_distance))

# With auto_scale - variables are automatically scaled
result_auto <- match_couples(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  auto_scale = TRUE
)

cat("With auto_scale:\n")
cat(sprintf("  Mean distance: %.2f\n", mean(result_auto$pairs$distance)))
cat(sprintf("  Total distance: %.2f\n\n", result_auto$info$total_distance))

# ==============================================================================
# Example 2: Handling problematic variables
# ==============================================================================

cat("\nExample 2: Automatic exclusion of problematic variables\n")
cat("========================================================\n\n")

# Create data with problematic variables
left_prob <- data.frame(
  id = 1:30,
  constant_var = rep(5, 30),           # Constant - should be excluded
  age = rnorm(30, 45, 10),
  all_na = rep(NA_real_, 30),          # All NA - should be excluded
  income = rnorm(30, 50000, 15000)
)

right_prob <- data.frame(
  id = 31:60,
  constant_var = rep(5, 30),
  age = rnorm(30, 47, 10),
  all_na = rep(NA_real_, 30),
  income = rnorm(30, 52000, 15000)
)

# Without auto_scale - will error or behave poorly
tryCatch({
  result_no_check <- match_couples(
    left_prob, right_prob,
    vars = c("constant_var", "age", "all_na", "income"),
    auto_scale = FALSE
  )
}, error = function(e) {
  cat("Error without auto_scale:\n")
  cat(sprintf("  %s\n\n", conditionMessage(e)))
})

# With auto_scale - problematic variables are automatically excluded
result_with_check <- suppressWarnings({
  match_couples(
    left_prob, right_prob,
    vars = c("constant_var", "age", "all_na", "income"),
    auto_scale = TRUE
  )
})

cat("With auto_scale (warnings suppressed for demo):\n")
cat("  Problematic variables automatically excluded\n")
cat(sprintf("  Matched %d pairs successfully\n\n", nrow(result_with_check$pairs)))

# ==============================================================================
# Example 3: Using preprocess_matching_vars() directly
# ==============================================================================

cat("\nExample 3: Direct use of preprocess_matching_vars()\n")
cat("====================================================\n\n")

# You can also use the preprocessing function directly for diagnostics
preproc <- preprocess_matching_vars(
  left_prob, right_prob,
  vars = c("constant_var", "age", "all_na", "income"),
  auto_scale = TRUE,
  scale_method = "auto",
  verbose = FALSE
)

cat("Preprocessing results:\n")
cat(sprintf("  Original variables: %d\n",
            length(c("constant_var", "age", "all_na", "income"))))
cat(sprintf("  Retained variables: %d (%s)\n",
            length(preproc$vars), paste(preproc$vars, collapse = ", ")))
cat(sprintf("  Excluded variables: %d (%s)\n",
            length(preproc$excluded_vars),
            paste(preproc$excluded_vars, collapse = ", ")))
cat(sprintf("  Suggested scaling: %s\n\n", preproc$scaling_method))

# ==============================================================================
# Example 4: Variable health diagnostics
# ==============================================================================

cat("\nExample 4: Variable health diagnostics\n")
cat("=======================================\n\n")

# Create data with various issues
set.seed(456)
left_diag <- data.frame(
  id = 1:100,
  good_var = rnorm(100, 50, 10),
  low_variance = rnorm(100, 100, 0.0001),
  high_missing = c(rep(NA_real_, 60), rnorm(40, 50, 10)),
  skewed_var = c(rnorm(90, 0, 1), rnorm(10, 50, 5))
)

right_diag <- data.frame(
  id = 101:200,
  good_var = rnorm(100, 50, 10),
  low_variance = rnorm(100, 100, 0.0001),
  high_missing = c(rep(NA_real_, 60), rnorm(40, 50, 10)),
  skewed_var = c(rnorm(90, 0, 1), rnorm(10, 50, 5))
)

# Check variable health
health <- check_variable_health(
  left_diag, right_diag,
  vars = c("good_var", "low_variance", "high_missing", "skewed_var")
)

cat("Variable health summary:\n")
print(health$summary[, c("variable", "sd", "prop_na", "skewness", "issue")])

cat("\n\nIssues detected:\n")
for (issue in health$issues) {
  cat(sprintf("  [%s] %s: %s\n",
              issue$severity, issue$variable, issue$message))
}

# ==============================================================================
# Example 5: Comparison of scaling methods
# ==============================================================================

cat("\n\nExample 5: Comparing scaling methods\n")
cat("=====================================\n\n")

set.seed(789)

# Data with outliers
left_outlier <- data.frame(
  id = 1:50,
  x = c(rnorm(45, 10, 2), c(100, 105, 98, 102, 99)), # 5 outliers
  y = rnorm(50, 20, 3)
)

right_outlier <- data.frame(
  id = 51:100,
  x = c(rnorm(45, 10, 2), c(101, 103, 97, 104, 100)),
  y = rnorm(50, 20, 3)
)

# Standard scaling
result_standard <- match_couples(
  left_outlier, right_outlier,
  vars = c("x", "y"),
  scale = "standardize"
)

# Robust scaling (uses median and MAD, resistant to outliers)
result_robust <- match_couples(
  left_outlier, right_outlier,
  vars = c("x", "y"),
  scale = "robust"
)

# Auto-scale (should detect outliers and choose robust)
result_auto_outlier <- match_couples(
  left_outlier, right_outlier,
  vars = c("x", "y"),
  auto_scale = TRUE
)

cat("Scaling method comparison (data with outliers):\n")
cat(sprintf("  Standardize: total distance = %.2f\n",
            result_standard$info$total_distance))
cat(sprintf("  Robust:      total distance = %.2f\n",
            result_robust$info$total_distance))
cat(sprintf("  Auto:        total distance = %.2f\n",
            result_auto_outlier$info$total_distance))

cat("\n\nDemo complete!\n")
