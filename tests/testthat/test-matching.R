# ==============================================================================
# Tests for matching layer
# ==============================================================================

test_that("matchmaker creates blocks correctly", {
  left <- data.frame(
    id     = 1:10,
    region = rep(c("A", "B"), each = 5),
    x      = rnorm(10)
  )
  right <- data.frame(
    id     = 11:20,
    region = rep(c("A", "B"), each = 5),
    x      = rnorm(10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "region")

  expect_s3_class(blocks, "matchmaker_result")
  expect_true("block_id" %in% names(blocks$left))
  expect_true("block_id" %in% names(blocks$right))
  expect_equal(nrow(blocks$block_summary), 2)
  expect_equal(blocks$block_summary$n_left,  c(5, 5))
  expect_equal(blocks$block_summary$n_right, c(5, 5))
})

test_that("matchmaker filters small blocks", {
  left <- data.frame(
    id     = 1:8,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x      = rnorm(8)
  )
  right <- data.frame(
    id     = 11:18,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x      = rnorm(8)
  )

  blocks <- matchmaker(
    left, right,
    block_type = "group",
    block_by   = "region",
    min_left   = 3,
    min_right  = 3
  )

  expect_equal(nrow(blocks$block_summary), 1)
  expect_equal(blocks$block_summary$block_id, "A")
  expect_equal(blocks$info$n_blocks_dropped, 2)
})

test_that("match_couples works with simple input", {
  set.seed(123)
  left  <- data.frame(id = 1:5,  x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
  expect_true(all(result$pairs$distance < 0.2))
})

test_that("match_couples respects max_distance", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 5, 3.1))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5)

  expect_equal(nrow(result$pairs), 2)
  expect_true(all(result$pairs$distance <= 0.5))
})

test_that("match_couples respects calipers", {
  left <- data.frame(
    id = 1:3,
    x  = c(1, 2, 3),
    y  = c(10, 20, 30)
  )
  right <- data.frame(
    id = 4:6,
    x  = c(1.1, 2.1, 3.1),
    y  = c(10.5, 25, 30.5)
  )

  result <- match_couples(
    left, right,
    vars     = c("x", "y"),
    calipers = list(y = 2)
  )

  # Should exclude middle pair where y differs by 5
  expect_equal(nrow(result$pairs), 2)
})

test_that("match_couples works with blocking", {
  left <- data.frame(
    id       = 1:6,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1, 2, 3, 10, 20, 30)
  )
  right <- data.frame(
    id       = 7:12,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1.1, 2.1, 3.1, 10.1, 20.1, 30.1)
  )

  result <- match_couples(left, right, vars = "x")

  expect_equal(nrow(result$pairs), 6)
  expect_true("block_id" %in% names(result$pairs))
  expect_equal(sum(result$pairs$block_id == "A"), 3)
  expect_equal(sum(result$pairs$block_id == "B"), 3)
})

test_that("match_couples handles rectangular inputs", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:8, x = c(1.1, 2.1, 3.1, 4, 5))

  result <- match_couples(left, right, vars = "x")

  expect_equal(nrow(result$pairs), 3)
  expect_equal(length(result$unmatched$left),  0)
  expect_equal(length(result$unmatched$right), 2)
})

test_that("validate_matching_inputs catches errors", {
  expect_error(
    validate_matching_inputs(list(), data.frame(x = 1)),
    "left must be a data frame"
  )

  expect_error(
    validate_matching_inputs(data.frame(), data.frame(x = 1)),
    "left must have at least one row"
  )

  expect_error(
    validate_matching_inputs(
      data.frame(x = 1),
      data.frame(y = 1),
      vars = "x"
    ),
    "right.*missing.*x"
  )
})

test_that("distance metrics work correctly", {
  left  <- matrix(c(0, 0), ncol = 2)
  right <- matrix(c(3, 4), ncol = 2)

  # Euclidean: sqrt(3^2 + 4^2) = 5
  d_euclidean <- compute_distance_matrix(left, right, "euclidean")
  expect_equal(d_euclidean[1, 1], 5)

  # Manhattan: |3| + |4| = 7
  d_manhattan <- compute_distance_matrix(left, right, "manhattan")
  expect_equal(d_manhattan[1, 1], 7)

  # Squared Euclidean: 3^2 + 4^2 = 25
  d_sq <- compute_distance_matrix(left, right, "squared_euclidean")
  expect_equal(d_sq[1, 1], 25)
})

test_that("scaling works correctly", {
  left  <- matrix(c(1, 2, 3, 4), ncol = 2)
  right <- matrix(c(5, 6, 7, 8), ncol = 2)

  scaled <- apply_scaling(left, right, "standardize")

  # Check that combined data has mean ~ 0, sd ~ 1
  combined_scaled <- rbind(scaled$left, scaled$right)
  expect_equal(colMeans(combined_scaled), c(0, 0), tolerance = 1e-10)
  expect_equal(apply(combined_scaled, 2, sd), c(1, 1), tolerance = 1e-10)
})

# ==============================================================================
# Tests for greedy_couples()
# ==============================================================================

test_that("greedy_couples works with simple input", {
  set.seed(123)
  left  <- data.frame(id = 1:5,  x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- greedy_couples(left, right, vars = "x")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
  expect_equal(nrow(result$pairs), 5)
  expect_true(all(result$pairs$distance < 0.2))
})

test_that("greedy_couples respects constraints", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 5, 3.1))

  result <- greedy_couples(left, right, vars = "x", max_distance = 0.5)

  expect_equal(nrow(result$pairs), 2)
  expect_true(all(result$pairs$distance <= 0.5))
})

test_that("greedy_couples works with blocking", {
  left <- data.frame(
    id       = 1:6,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1, 2, 3, 10, 20, 30)
  )
  right <- data.frame(
    id       = 7:12,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1.1, 2.1, 3.1, 10.1, 20.1, 30.1)
  )

  result <- greedy_couples(left, right, vars = "x")

  expect_equal(nrow(result$pairs), 6)
  expect_true("block_id" %in% names(result$pairs))
  expect_equal(result$info$n_blocks, 2)
})

test_that("greedy strategies produce valid matchings", {
  set.seed(456)
  left  <- data.frame(id = 1:20,  x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result_row    <- greedy_couples(left, right, vars = "x", strategy = "row_best")
  result_sorted <- greedy_couples(left, right, vars = "x", strategy = "sorted")
  result_pq     <- greedy_couples(left, right, vars = "x", strategy = "pq")

  # All should produce valid matchings
  expect_equal(nrow(result_row$pairs),    20)
  expect_equal(nrow(result_sorted$pairs), 20)
  expect_equal(nrow(result_pq$pairs),     20)

  # No duplicate assignments
  expect_equal(length(unique(result_row$pairs$left_id)),  20)
  expect_equal(length(unique(result_row$pairs$right_id)), 20)
})

test_that("greedy is faster than optimal (smoke test)", {
  skip_if_not(requireNamespace("microbenchmark", quietly = TRUE))

  set.seed(789)
  left  <- data.frame(id = 1:50,  x = rnorm(50), y = rnorm(50))
  right <- data.frame(id = 51:100, x = rnorm(50), y = rnorm(50))

  # Just test that both complete successfully
  expect_no_error({
    result_opt    <- match_couples(left, right, vars = c("x", "y"))
    result_greedy <- greedy_couples(left, right, vars = c("x", "y"))
  })

  # Greedy should produce valid but possibly suboptimal matching
  expect_gte(result_greedy$info$total_distance, result_opt$info$total_distance)
})

# ==============================================================================
# Tests for preprocessing functions
# ==============================================================================

test_that("check_variable_health detects constant variables", {
  left <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )
  right <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )

  health <- check_variable_health(left, right, c("x", "y"))

  expect_s3_class(health, "variable_health")
  expect_true("x" %in% health$exclude_vars)
  expect_false("y" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(i) i$type == "constant")))
})

test_that("check_variable_health detects all-NA variables", {
  left <- data.frame(
    x = rep(NA_real_, 10),
    y = rnorm(10)
  )
  right <- data.frame(
    x = rep(NA_real_, 10),
    y = rnorm(10)
  )

  health <- check_variable_health(left, right, c("x", "y"))

  expect_true("x" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(i) i$type == "all_na")))
})

test_that("check_variable_health detects high missingness", {
  left <- data.frame(
    x = c(rep(NA_real_, 7), rnorm(3))
  )
  right <- data.frame(
    x = c(rep(NA_real_, 7), rnorm(3))
  )

  health <- check_variable_health(left, right, "x")

  expect_true(any(sapply(health$issues, function(i) i$type == "high_missingness")))
})

test_that("check_variable_health detects skewness", {
  set.seed(42)
  # Create highly skewed data
  skewed_data <- c(rnorm(45, mean = 0, sd = 1), rnorm(5, mean = 20, sd = 1))

  left <- data.frame(x = skewed_data[1:25])
  right <- data.frame(x = skewed_data[26:50])

  health <- check_variable_health(left, right, "x")

  # Should detect skewness
  expect_true(any(sapply(health$issues, function(i) i$type == "skewed")))
})

test_that("suggest_scaling returns appropriate methods", {
  # Similar scales - should suggest standardize or none
  left <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 20, 3))
  right <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 20, 3))

  method <- suggest_scaling(left, right, c("x", "y"))
  expect_true(method %in% c("standardize", "robust", "none"))

  # Very different scales - should suggest standardize
  left2 <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 1000, 200))
  right2 <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 1000, 200))

  method2 <- suggest_scaling(left2, right2, c("x", "y"))
  expect_equal(method2, "standardize")
})

test_that("preprocess_matching_vars excludes problematic variables", {
  left <- data.frame(
    const = rep(5, 10),
    good = rnorm(10),
    all_na = rep(NA_real_, 10)
  )
  right <- data.frame(
    const = rep(5, 10),
    good = rnorm(10),
    all_na = rep(NA_real_, 10)
  )

  preproc <- preprocess_matching_vars(
    left, right,
    vars = c("const", "good", "all_na"),
    auto_scale = TRUE,
    remove_problematic = TRUE,
    verbose = FALSE
  )

  expect_s3_class(preproc, "preprocessing_result")
  expect_equal(preproc$vars, "good")
  expect_setequal(preproc$excluded_vars, c("const", "all_na"))
})

test_that("preprocess_matching_vars suggests scaling method", {
  left <- data.frame(x = rnorm(50, 100, 10), y = rnorm(50, 0.5, 0.1))
  right <- data.frame(x = rnorm(50, 100, 10), y = rnorm(50, 0.5, 0.1))

  preproc <- preprocess_matching_vars(
    left, right,
    vars = c("x", "y"),
    auto_scale = TRUE,
    scale_method = "auto",
    verbose = FALSE
  )

  expect_true(preproc$scaling_method %in% c("standardize", "robust", "range"))
})

test_that("match_couples with auto_scale works correctly", {
  set.seed(123)
  left <- data.frame(
    id = 1:10,
    const = rep(1, 10),  # Should be excluded
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )
  right <- data.frame(
    id = 11:20,
    const = rep(1, 10),  # Should be excluded
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )

  # Suppress warnings about excluded variables
  result <- suppressWarnings({
    match_couples(
      left, right,
      vars = c("const", "x", "y"),
      auto_scale = TRUE
    )
  })

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 10)
})

test_that("greedy_couples with auto_scale works correctly", {
  set.seed(456)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )

  result <- greedy_couples(
    left, right,
    vars = c("x", "y"),
    auto_scale = TRUE
  )

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
})

# ==============================================================================
# Tests for balance diagnostics
# ==============================================================================

test_that("standardized_difference calculates correctly", {
  x1 <- c(10, 12, 14, 16, 18)
  x2 <- c(11, 13, 15, 17, 19)

  std_diff <- standardized_difference(x1, x2)

  # Should be close to -1 / sqrt(var) for shift of 1
  expect_true(is.numeric(std_diff))
  expect_false(is.na(std_diff))
  expect_true(abs(std_diff) < 1)  # Should be small for similar distributions
})

test_that("standardized_difference handles edge cases", {
  # Empty vectors
  expect_true(is.na(standardized_difference(numeric(0), c(1, 2, 3))))

  # Constant values (SD = 0)
  expect_equal(standardized_difference(c(5, 5, 5), c(5, 5, 5)), 0)

  # With NAs
  x1 <- c(1, 2, 3, NA, 5)
  x2 <- c(2, 3, 4, 5, NA)
  std_diff <- standardized_difference(x1, x2)
  expect_false(is.na(std_diff))
})

test_that("balance_diagnostics works with simple matching", {
  set.seed(123)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 50, 10),
    income = rnorm(20, 50000, 10000)
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 10),
    income = rnorm(30, 50000, 10000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))

  expect_s3_class(balance, "balance_diagnostics")
  expect_equal(nrow(balance$var_stats), 2)
  expect_true("age" %in% balance$var_stats$variable)
  expect_true("income" %in% balance$var_stats$variable)
  expect_equal(balance$n_matched, 20)
  expect_true(is.numeric(balance$overall$mean_abs_std_diff))
})

test_that("balance_diagnostics computes correct statistics", {
  set.seed(456)
  # Create data with known properties
  left <- data.frame(
    id = 1:30,
    x = rep(10, 30)  # Constant value
  )
  right <- data.frame(
    id = 31:60,
    x = rep(10, 30)  # Same constant value
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  # With identical values, standardized difference should be 0
  expect_equal(balance$var_stats$std_diff[1], 0)
  expect_equal(balance$var_stats$mean_diff[1], 0)
})

test_that("balance_diagnostics detects imbalance", {
  set.seed(789)
  # Create data with deliberate imbalance
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 30, 5)  # Younger group
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 5)  # Older group
  )

  result <- match_couples(left, right, vars = "age")
  balance <- balance_diagnostics(result, left, right, vars = "age")

  # Should detect large standardized difference
  expect_true(abs(balance$var_stats$std_diff[1]) > 0.1)
  expect_true(balance$overall$mean_abs_std_diff > 0.1)
})

test_that("balance_diagnostics works with blocking", {
  set.seed(321)
  left <- data.frame(
    id = 1:40,
    group = rep(c("A", "B"), each = 20),
    age = rnorm(40, 50, 10)
  )
  right <- data.frame(
    id = 41:100,
    group = rep(c("A", "B"), each = 30),
    age = rnorm(60, 50, 10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "group")
  result <- match_couples(blocks$left, blocks$right, vars = "age")

  balance <- balance_diagnostics(result, blocks$left, blocks$right, vars = "age")

  expect_true(balance$has_blocks)
  expect_false(is.null(balance$block_stats))
  expect_equal(nrow(balance$block_stats), 2)  # Two blocks
  expect_true("quality" %in% names(balance$block_stats))
})

test_that("balance_diagnostics handles unmatched units", {
  set.seed(654)
  # Create unbalanced data (more right than left)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1)
  )
  right <- data.frame(
    id = 11:40,
    x = rnorm(30, 0, 1)
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  expect_equal(balance$n_matched, 10)
  expect_equal(balance$n_unmatched_left, 0)
  expect_equal(balance$n_unmatched_right, 20)
})

test_that("balance_table formats output correctly", {
  set.seed(987)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 50, 10),
    income = rnorm(20, 50000, 10000)
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 10),
    income = rnorm(30, 50000, 10000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))

  tbl <- balance_table(balance)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2)
  expect_true("Variable" %in% names(tbl))
  expect_true("Std Diff" %in% names(tbl))
  expect_true("Mean Diff" %in% names(tbl))
})

test_that("balance_diagnostics print method works", {
  set.seed(111)
  left <- data.frame(
    id = 1:15,
    x = rnorm(15, 0, 1)
  )
  right <- data.frame(
    id = 16:40,
    x = rnorm(25, 0, 1)
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  # Should print without error
  expect_output(print(balance), "Balance Diagnostics")
  expect_output(print(balance), "Matched pairs")
  expect_output(print(balance), "Overall Balance")
})

test_that("balance_diagnostics validates inputs", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  # Wrong result type
  expect_error(
    balance_diagnostics("not a result", left, right, vars = "x"),
    "matching_result"
  )

  # Missing ID column
  expect_error(
    balance_diagnostics(result, left, right, vars = "x", left_id = "missing"),
    "not found"
  )

  # Missing variable
  expect_error(
    balance_diagnostics(result, left, right, vars = "missing_var"),
    "not found"
  )
})
