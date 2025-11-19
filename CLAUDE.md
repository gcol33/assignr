# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**lapr** is an R package that provides fast solvers for the Linear Assignment Problem (LAP). It implements multiple algorithms (Hungarian, Jonker-Volgenant, Auction, etc.) with a modern tidy interface, supporting rectangular matrices, forbidden assignments (NA/Inf), k-best solutions, and pixel-level image morphing.

## Development Commands

### Building and Testing

```r
# Install dependencies
install.packages(c("Rcpp", "RcppEigen", "tibble", "dplyr", "testthat"))

# Build package with native code
devtools::load_all()  # or Ctrl+Shift+L in RStudio

# Run all tests
devtools::test()  # or Ctrl+Shift+T in RStudio

# Run specific test file
testthat::test_file("tests/testthat/test-assignment.R")

# Check package
devtools::check()

# Build documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()
```

### C++ Development

The package uses Rcpp and requires C++17. After modifying C++ code:

```r
# Recompile C++ code
Rcpp::compileAttributes()
devtools::load_all()
```

## Code Architecture

### Two-Layer API Design

**Layer 1 - Core solvers** ([R/assignment.R](R/assignment.R)):
- `assignment()` - Returns list with `match` vector, `total_cost`, `status`, `method_used`
- Low-level interface matching C++ semantics
- Used internally by all higher-level functions

**Layer 2 - Tidy interface** ([R/lap_solve.R](R/lap_solve.R)):
- `lap_solve()` - Returns tibble with tidy output
- Supports matrix, data frame, and grouped data frame inputs
- Handles column mapping for data frames via tidy evaluation
- User-facing API

### LAP Solver Implementations

All algorithm implementations follow the same pattern:

1. **R wrapper** (e.g., `assignment()` in [R/assignment.R](R/assignment.R)):
   - Validates inputs
   - Handles maximize/minimize conversion
   - Transposes if rows > cols
   - Calls C++ implementation
   - Formats result

2. **C++ interface** ([src/rcpp_interface.cpp](src/rcpp_interface.cpp)):
   - Exported via `[[Rcpp::export]]`
   - Thin wrapper around implementation
   - Handles R <-> C++ type conversion

3. **C++ implementation** (e.g., [src/solve_jv.cpp](src/solve_jv.cpp)):
   - Pure C++ algorithm
   - Returns `Rcpp::List` with `match` and `total_cost`

Available algorithms (see [R/assignment.R](R/assignment.R:10-12)):
- `jv` - Jonker-Volgenant (general purpose, fast)
- `hungarian` - Classic Hungarian algorithm
- `auction` / `auction_gs` / `auction_scaled` - Auction variants
- `sap` / `ssp` - Shortest augmenting path
- `csflow` - Cost-scaling flow
- `hk01` - Hopcroft-Karp for binary/uniform costs
- `bruteforce` - Exhaustive search for tiny problems
- `auto` - Automatic selection based on problem characteristics

### K-Best Solutions

Two algorithms for finding k-best assignments (see [R/kbest_assignment.R](R/kbest_assignment.R)):
- **Murty's algorithm** ([src/solve_murty.cpp](src/solve_murty.cpp)) - Systematic partitioning
- **Lawler's algorithm** ([src/solve_kbest_lawler.cpp](src/solve_kbest_lawler.cpp)) - Alternative approach

### Pixel Morphing Feature

Complex multi-mode system for image morphing via LAP (see [R/pixel_morph.R](R/pixel_morph.R)):

**Three assignment modes:**
1. `exact` - Full pixel-level LAP (small images only, < 4096 pixels recommended)
2. `color_walk` - Color quantization + spatial mini-LAPs (scales well)
3. `recursive` - Multi-scale 2×2 recursive tiling

**Key functions:**
- `pixel_morph_animate()` - Creates animated morph (GIF/WebP/MP4)
- `pixel_morph()` - Returns final transported frame only

**Cost model:** `cost(i,j) = alpha * color_distance + beta * spatial_distance`

**Rendering semantics (CRITICAL):**
- Assignment computed using both images A and B
- Renderer uses ONLY image A's colors (transport-only)
- B influences WHERE pixels go, NOT WHAT colors appear
- Final frame is sharp (no motion blur)

**C++ support functions** ([src/morph_pixel_level.cpp](src/morph_pixel_level.cpp)):
- `compute_pixel_cost_cpp()` - Build cost matrix
- `morph_pixel_level_cpp()` - Render animation frames
- `downscale_image_cpp()` / `upscale_assignment_cpp()` - Multi-resolution
- `color_palette_info_cpp()` - Color quantization for color_walk mode

### Special Purpose Solvers

**Line metric solver** ([R/lap_solve.R](R/lap_solve.R:298)):
- `lap_solve_line_metric()` - Specialized for 1D point matching
- O(n log n) for square, O(n*m) DP for rectangular
- Supports L1 (Manhattan) and L2 (squared Euclidean) costs

**Bucketed SAP** ([R/lap_solve.R](R/lap_solve.R:420)):
- `assignment_ssap_bucket()` - Dial's algorithm for integer costs
- O(n²×m×C) where C is max cost
- Auto-scales decimal costs to integers

**Cycle canceling** ([R/lap_solve.R](R/lap_solve.R:487)):
- `assignment_cycle_cancel()` - Min-cost flow with Karp's algorithm

**Gabow-Tarjan** ([R/lap_solve.R](R/lap_solve.R:578)):
- `assignment_gabow_tarjan()` - Cost scaling with complementary slackness
- Maintains exact optimality (no epsilon)

### C++ Code Organization

**Header files:**
- [src/lap_internal.h](src/lap_internal.h) - Function declarations for all solvers
- [src/lap_utils.h](src/lap_utils.h) - Shared utilities
- [src/utils_gabow_tarjan.h](src/utils_gabow_tarjan.h) - Gabow-Tarjan specific types/functions

**Implementation pattern:**
- Each solver in separate `.cpp` file (e.g., `solve_hungarian.cpp`)
- Shared code in `lap_utils.cpp`
- All exports centralized in `rcpp_interface.cpp`

**Key utilities:**
- `prepare_cost_matrix_impl()` - Handle maximize, Inf/NA masking
- `validate_cost_data()` - Input validation

### Testing Strategy

**Test organization:**
- One test file per solver (e.g., [tests/testthat/test-assignment-jv.R](tests/testthat/test-assignment-jv.R))
- Shared test cases for correctness across algorithms
- Performance benchmarks for large problems
- Edge cases: rectangular, forbidden edges, maximize

**Gabow-Tarjan has modular tests:**
- Modules A-H test individual components ([tests/testthat/test_gabow_tarjan_moduleA.R](tests/testthat/test_gabow_tarjan_moduleA.R), etc.)
- Each module exports test functions via `rcpp_interface.cpp`
- Integration test in [tests/testthat/test-gabow_tarjan_solver.R](tests/testthat/test-gabow_tarjan_solver.R)

## Important Conventions

### Indexing
- **R code:** 1-based indexing (R convention)
- **C++ code:** 0-based indexing internally, convert at boundaries
- **Special value:** -1 (C++) / 0 (R) for "unmatched"

### Matrix Orientation
- Rows = sources/tasks
- Columns = targets/agents
- If rows > cols, automatically transpose and reverse at end

### Cost Matrix Semantics
- `NA` or `Inf` = forbidden assignment
- `maximize = TRUE` internally negates costs
- Always returns costs on original scale

### Return Values
All solvers return consistent structure:
```r
list(
  match = integer vector (1-based, length = nrow),
  total_cost = numeric scalar,
  status = "optimal",
  method_used = "algorithm_name"
)
```

## Cost Computation Semantics (CRITICAL)

**The Single Source of Truth**: All solvers MUST use `compute_total_cost()` from [src/lap_utils.cpp](src/lap_utils.cpp) to compute the `cost` or `total_cost` field.

### The Cost Guarantee

The `cost` field in solver results has a **precise, unambiguous definition**:

```
cost = sum of original_cost[i, assignment[i]] over all matched rows
```

**What this means:**
1. **Always uses ORIGINAL cost matrix** - No transformations, reductions, scaling, or negation
2. **Works for both minimize and maximize** - Same formula, no sign flips
3. **Ignores dummy columns** - If `assignment[i] > ncol(original_cost)`, skip it
4. **Ignores unmatched rows** - If `assignment[i] == 0` or `NA`, skip it
5. **Only sums finite costs** - Inf/NA in matched edges are skipped

### Why This Matters

Algorithms use many internal tricks:
- **Hungarian**: Row/column reduction leaves costs near zero
- **Gabow-Tarjan**: Bit-scaling, integer conversion, negation for maximize
- **Auction**: Prices added to costs, epsilon-optimal trades

**These must NOT leak into the reported cost!**

### Implementation Pattern

Every solver must follow this pattern at the end:

```cpp
// Convert to 1-based R assignment vector
Rcpp::IntegerVector assignment_R(n);
for (int i = 0; i < n; ++i) {
    assignment_R[i] = (internal_match[i] != UNMATCHED)
                      ? (internal_match[i] + 1)
                      : NA_INTEGER;
}

// Use centralized helper (THE SINGLE SOURCE OF TRUTH)
double total_cost = compute_total_cost(original_cost_matrix, assignment_R);

return Rcpp::List::create(
    Rcpp::Named("match") = assignment_R,
    Rcpp::Named("total_cost") = total_cost
);
```

**NEVER compute cost from internal transformed matrices!**

### Testing Cost Semantics

Use R helper functions from [R/diagnostic_tools.R](R/diagnostic_tools.R):

```r
# Verify single solver
result <- assignment(cost, method = "gabow_tarjan")
verify_cost_semantics(cost, result)  # Checks cost matches formula

# Compare multiple solvers
compare_solver_costs(cost, methods = c("jv", "hungarian", "gabow_tarjan"))

# Test both minimize and maximize
test_cost_minimize_maximize(cost)
```

These functions enforce the cost guarantee and catch violations early.

## Adding New Algorithms

To add a new LAP solver `foo`:

1. Create [src/solve_foo.cpp](src/solve_foo.cpp) with `solve_foo_impl()`
2. Add declaration to [src/lap_internal.h](src/lap_internal.h)
3. Export in [src/rcpp_interface.cpp](src/rcpp_interface.cpp): `[[Rcpp::export]] lap_solve_foo()`
4. Add R wrapper in [R/assignment.R](R/assignment.R): call `lap_solve_foo()` in switch statement
5. Optionally add standalone function in [R/lap_solve.R](R/lap_solve.R) like `assignment_foo()`
6. Create test file [tests/testthat/test-assignment-foo.R](tests/testthat/test-assignment-foo.R)
7. Update documentation and NAMESPACE via `devtools::document()`

## Common Pitfalls

1. **Forgetting to call `Rcpp::compileAttributes()`** after adding/modifying `[[Rcpp::export]]`
2. **Mixing 0-based and 1-based indexing** at R/C++ boundary
3. **Not handling rectangular matrices** - always test with n ≠ m
4. **Ignoring NA/Inf costs** - test forbidden edges
5. **Auto method selection** - verify `method="auto"` chooses correct algorithm
6. **Pixel morphing semantics** - remember renderer is transport-only (A's colors)

## Package Structure Notes

- Entry point: [R/assignment.R](R/assignment.R) (`assignment()` function)
- Tidy interface: [R/lap_solve.R](R/lap_solve.R) (`lap_solve()` function)
- K-best: [R/kbest_assignment.R](R/kbest_assignment.R)
- Batch solving: [R/lap_solve_batch.R](R/lap_solve_batch.R)
- Image morphing: [R/pixel_morph.R](R/pixel_morph.R)
- All C++ exports: [src/rcpp_interface.cpp](src/rcpp_interface.cpp)
