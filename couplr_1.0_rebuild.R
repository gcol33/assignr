# Rebuild script for couplr 1.0.0
# Run this after renaming from lapr to couplr

cat("========================================================================\n")
cat("  couplr 1.0.0 - Complete Rebuild\n")
cat("========================================================================\n\n")

cat("Step 1: Regenerating Rcpp exports...\n")
Rcpp::compileAttributes()

cat("\nStep 2: Documenting package (regenerates NAMESPACE)...\n")
devtools::document()

cat("\nStep 3: Compiling and loading package...\n")
devtools::load_all()

cat("\n========================================================================\n")
cat("✓ Package rebuilt successfully!\n")
cat("========================================================================\n\n")

# Test core functionality
cat("Running quick tests...\n\n")

# Test 1: assignment()
cat("Test 1: assignment() function... ")
cost <- matrix(c(1, 2, 3, 4), nrow = 2)
result <- assignment(cost, method = "jv")
if (length(result$match) == 2) {
  cat("✓ PASS\n")
} else {
  cat("✗ FAIL\n")
}

# Test 2: lap_solve()
cat("Test 2: lap_solve() tidy interface... ")
result2 <- lap_solve(cost)
if (nrow(result2) == 2 && "source" %in% names(result2)) {
  cat("✓ PASS\n")
} else {
  cat("✗ FAIL\n")
}

# Test 3: Package name
cat("Test 3: Package metadata... ")
desc <- packageDescription("couplr")
if (!is.null(desc) && desc$Package == "couplr" && desc$Version == "1.0.0") {
  cat("✓ PASS (couplr 1.0.0)\n")
} else {
  cat("✗ FAIL\n")
}

cat("\n========================================================================\n")
cat("  Welcome to couplr 1.0.0!\n")
cat("========================================================================\n")
cat("\nMain functions:\n")
cat("  • lap_solve()        - Solve assignment problems (tidy interface)\n")
cat("  • lap_solve_batch()  - Batch solving\n")
cat("  • lap_solve_kbest()  - K-best solutions\n")
cat("  • assignment()       - Low-level matrix solver\n")
cat("\nRun devtools::test() to run full test suite.\n")
cat("========================================================================\n")
