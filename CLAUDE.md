# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**couplr** (formerly lapr) is an R package for solving Linear Assignment Problems (LAP) with production-ready matching workflows. The package combines:

- **14+ LAP algorithms** - Hungarian, Jonker-Volgenant, Auction, Gabow-Tarjan, and more
- **Greedy matching** - Fast approximate algorithms for large-scale problems
- **Automatic preprocessing** - Smart scaling, variable health checks, categorical encoding
- **Balance diagnostics** - Standardized differences, variance ratios, KS tests
- **Distance caching** - Reusable precomputed distances for faster experimentation
- **Parallel processing** - Distributed block matching via the future framework
- **Joined datasets** - Analysis-ready merged output with broom-style interface
- **Fun error messages** - Couple-themed warnings with emoji support

**Current version:** 1.0.0 (released 2025-11-19)
**Tests:** 1382 passing
**License:** MIT

## Development Commands

### Core Package Development

```r
# Install dependencies
install.packages(c("Rcpp", "RcppEigen", "tibble", "dplyr", "testthat", "devtools"))

# Load package (recompiles C++ if needed)
devtools::load_all()  # or Ctrl+Shift+L in RStudio

# Run all tests (1382 tests)
devtools::test()  # or Ctrl+Shift+T

# Run specific test file
testthat::test_file("tests/testthat/test-matching.R")

# Run matching tests only
devtools::test(filter = "matching")

# Build documentation (after editing roxygen comments)
devtools::document()

# Full package check
devtools::check()

# Build tarball
devtools::build()

# Install from source (alternative to devtools)
# From command line: R CMD INSTALL --build .
# Or in R: install.packages(".", repos = NULL, type = "source")
```

### C++ Development Workflow

After modifying C++ code or adding `[[Rcpp::export]]` functions:

```r
# Regenerate Rcpp exports (REQUIRED after C++ changes)
Rcpp::compileAttributes()

# Update documentation
devtools::document()

# Reload package
devtools::load_all()

# Run tests
devtools::test()
```

**Critical:** C++17 required. Windows users need Rtools with g++ supporting C++17.

**Troubleshooting compilation issues:**
```powershell
# Windows: Clean stale object files if encountering binary mismatches
Remove-Item src\*.o, src\*.dll -Force

# Then rebuild
R CMD INSTALL --clean .
```

**Note:** The `src/` directory may contain committed object files (`*.o`, `*.dll`) which can become stale. Clean before building if behavior is unexpected.

### Git Workflow

```bash
# Check status
git status

# Add changes
git add .

# Commit (use descriptive messages)
git commit -m "Description of changes"

# Push to GitHub
git push origin main

# Create release tag
git tag -a v1.0.0 -m "Release message"
git push origin v1.0.0
```

## Code Architecture

### Three-Layer API Design

**Layer 1 - Low-level LAP solvers** ([R/assignment.R](R/assignment.R)):
- `assignment()` - Core solver wrapper returning `list(match, total_cost, status, method_used)`
- Direct mapping to C++ implementations
- Method auto-selection via `method = "auto"`

**Layer 2 - Tidy LAP interface** ([R/lap_solve.R](R/lap_solve.R)):
- `lap_solve()` - Returns tibble with tidy output
- Supports matrix, data frame, and grouped inputs
- Batch solving via `lap_solve_batch()`
- K-best solutions via `lap_solve_kbest()`

**Layer 3 - Matching workflows** (NEW in v1.0.0):
- `match_couples()` - Optimal one-to-one matching with preprocessing ([R/matching_core.R](R/matching_core.R:1-387))
- `greedy_couples()` - Fast greedy matching with 3 strategies ([R/matching_core.R](R/matching_core.R:388-625))
- `matchmaker()` - Blocking/stratification support ([R/matching_blocks.R](R/matching_blocks.R))
- `balance_diagnostics()` - Comprehensive balance assessment ([R/matching_diagnostics.R](R/matching_diagnostics.R))
- `preprocess_matching_vars()` - Automatic variable health checks ([R/matching_preprocessing.R](R/matching_preprocessing.R))
- `compute_distances()` - Precompute and cache distance matrices ([R/matching_distance_cache.R](R/matching_distance_cache.R))
- `join_matched()` - Create analysis-ready merged datasets ([R/matching_join.R](R/matching_join.R))

### Matching Layer Architecture (v1.0.0)

The matching layer provides production-ready workflows for observational studies, treatment effect estimation, and sample matching.

**Core Workflow:**
```r
# 1. Optional: Create blocks/strata
blocks <- matchmaker(left, right, block_type = "group", block_by = "site")

# 2. Match with automatic preprocessing
result <- match_couples(
  left, right,
  vars = c("age", "income", "education"),
  auto_scale = TRUE,           # Smart preprocessing
  scale = "robust",             # MAD-based scaling
  max_distance = 0.5,           # Caliper
  return_diagnostics = TRUE
)

# 3. Assess balance quality
balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))
print(balance)  # Standardized differences, variance ratios, KS tests
balance_table(balance)  # Publication-ready table
```

**Complete R File Structure (20 files):**

```
R/
├── couplr-package.R              # Package documentation and imports
├── data.R                        # Dataset documentation
├── zzz.R                         # Package startup/attach messages
├── RcppExports.R                 # Auto-generated Rcpp bindings
├── utils.R                       # General utility functions
│
├── LAP Solvers (Layer 1 & 2):
│   ├── lap_solve.R               # Tidy LAP interface (lap_solve)
│   ├── lap_solve_batch.R         # Batch solving for multiple problems
│   └── lap_solve_kbest.R         # K-best solutions (Murty, Lawler)
│
├── Matching Core (Layer 3):
│   ├── matching_core.R           # match_couples(), greedy_couples() (625 lines)
│   ├── matching_distance.R       # Distance computation, 3 scaling methods
│   ├── matching_distance_cache.R # compute_distances(), update_constraints()
│   ├── matching_constraints.R    # Calipers, weights, max_distance
│   ├── matching_blocks.R         # matchmaker(), blocking/stratification
│   ├── matching_parallel.R       # Parallel processing via future
│   ├── matching_utils.R          # Shared matching utilities
│   └── matching_messages.R       # Fun error/warning messages (270 lines)
│
├── Matching Analysis:
│   ├── matching_preprocessing.R  # Auto-scaling, health checks (510 lines)
│   ├── matching_diagnostics.R    # Balance diagnostics (461 lines)
│   └── matching_join.R           # join_matched(), augment() (220 lines)
│
└── Pixel Morphing:
    ├── morph_pixel.R             # Pixel-level LAP morphing (modes)
    ├── morph_tiling.R            # Recursive tiling for large images
    └── morph_utils.R             # Morphing utilities
```

**Key file responsibilities:**
- **[matching_core.R](R/matching_core.R)** - Main matching entry points
- **[matching_preprocessing.R](R/matching_preprocessing.R)** - Variable health, scaling suggestions
- **[matching_diagnostics.R](R/matching_diagnostics.R)** - Balance metrics, quality ratings
- **[matching_join.R](R/matching_join.R)** - Analysis-ready merged datasets
- **[matching_distance_cache.R](R/matching_distance_cache.R)** - Distance object management
- **[matching_parallel.R](R/matching_parallel.R)** - Parallel block matching
- **[matching_messages.R](R/matching_messages.R)** - Couple-themed error messages

**Key Features:**

1. **Automatic Preprocessing** ([R/matching_preprocessing.R](R/matching_preprocessing.R)):
   - Detects constant variables (SD = 0) → excludes with warning
   - Detects high missingness (>50%) → warns
   - Detects extreme skewness (|skew| > 2) → info
   - Smart scaling: "robust" (median/MAD), "standardize" (mean/SD), "range" (min-max)
   - Categorical encoding: binary (0/1), ordered factors (numeric)

2. **Greedy Matching Algorithms** ([src/solvers/greedy_matching.cpp](src/solvers/greedy_matching.cpp)):
   - `sorted` - Sort all pairs by cost, assign greedily
   - `row_best` - For each row, pick best available column
   - `pq` - Priority queue for large problems
   - 10-100x faster than optimal for large datasets

3. **Balance Diagnostics** ([R/matching_diagnostics.R](R/matching_diagnostics.R)):
   - Standardized differences: (mean_left - mean_right) / pooled_sd
   - Variance ratios: SD_left / SD_right
   - KS tests for distribution comparison
   - Per-block statistics with quality ratings
   - Quality thresholds: <0.1 excellent, 0.1-0.25 good, 0.25-0.5 acceptable, >0.5 poor

4. **Blocking Support** ([R/matching_blocks.R](R/matching_blocks.R)):
   - Exact blocking: `block_type = "group"` + `block_by = "site"`
   - K-means clustering: `block_type = "cluster"` + `n_clusters = 5`
   - Preserves block IDs through matching pipeline

### C++ Code Organization

**Complete File Structure (22 .cpp files, 3 .h files):**

```
src/
├── core/                      # Shared utilities
│   ├── lap_internal.h            # Function declarations for all solvers
│   ├── lap_utils.cpp             # Common helpers (cost computation, validation)
│   └── lap_utils.h               # Utility function headers
├── interface/                 # Cost matrix preparation
│   └── prepare_cost_matrix.cpp  # Rcpp interface for cost matrix creation
├── solvers/                   # LAP algorithm implementations (15 files)
│   ├── greedy_matching.cpp       # Greedy strategies (sorted, row_best, pq)
│   ├── solve_auction.cpp         # Auction algorithm variants
│   ├── solve_bruteforce.cpp      # Exhaustive search for tiny problems
│   ├── solve_cost_scaling.cpp    # Gabow's cost-scaling algorithm
│   ├── solve_csflow.cpp          # Cost-scaling flow algorithm
│   ├── solve_cycle_cancel.cpp    # Cycle canceling with Karp's trick
│   ├── solve_hk01.cpp            # Hopcroft-Karp for binary costs
│   ├── solve_hungarian.cpp       # Classic Hungarian algorithm
│   ├── solve_jv.cpp              # Jonker-Volgenant (general purpose)
│   ├── solve_kbest_lawler.cpp    # Lawler's k-best algorithm
│   ├── solve_line_metric.cpp     # Specialized for 1D matching
│   ├── solve_murty.cpp           # Murty's algorithm for k-best solutions
│   ├── solve_ssap_bucket.cpp     # Dial's algorithm for integer costs
│   ├── solve_ssp.cpp             # Shortest augmenting path
│   └── solve_gabow_tarjan.cpp    # (note: also in gabow_tarjan/)
├── gabow_tarjan/              # Gabow-Tarjan specific code
│   ├── solve_gabow_tarjan.cpp    # Main Gabow-Tarjan implementation
│   ├── utils_gabow_tarjan.cpp    # Helper functions
│   └── utils_gabow_tarjan.h      # Header for Gabow-Tarjan utilities
├── morph/                     # Pixel morphing for visualization
│   ├── morph_pixel_level.cpp     # Pixel-level LAP morphing
│   └── region_means.cpp          # Region-based color averaging
├── rcpp_interface.cpp         # Central export point (ALL [[Rcpp::export]] here)
└── RcppExports.cpp            # Auto-generated by Rcpp::compileAttributes()
```

**Key files:**
- **[rcpp_interface.cpp](src/rcpp_interface.cpp)** - All `[[Rcpp::export]]` declarations
- **[lap_internal.h](src/core/lap_internal.h)** - Forward declarations for all solver implementations
- **[RcppExports.cpp](src/RcppExports.cpp)** - Auto-generated, DO NOT EDIT manually

**Export Pattern (CRITICAL):**

Subdirectory files (e.g., `src/solvers/greedy_matching.cpp`) do NOT use `[[Rcpp::export]]` directly. Instead:

1. Implementation in subdirectory with `_impl` suffix:
```cpp
// src/solvers/greedy_matching.cpp
List greedy_matching_impl(NumericMatrix cost_matrix, bool maximize, std::string strategy) {
    // Implementation
}
```

2. Forward declaration in [src/rcpp_interface.cpp](src/rcpp_interface.cpp):
```cpp
extern Rcpp::List greedy_matching_impl(Rcpp::NumericMatrix, bool, std::string);
```

3. Exported wrapper in [src/rcpp_interface.cpp](src/rcpp_interface.cpp):
```cpp
// [[Rcpp::export]]
Rcpp::List greedy_matching(Rcpp::NumericMatrix cost_matrix,
                           bool maximize = false,
                           std::string strategy = "row_best") {
    return greedy_matching_impl(cost_matrix, maximize, strategy);
}
```

**Why:** Rcpp only scans the main source directory, not subdirectories. All `[[Rcpp::export]]` tags MUST be in `src/*.cpp` files at the root level.

### Makefile Structure

**Makevars** (Unix/Linux/Mac):
```make
CXX_STD = CXX17
SOURCES = RcppExports.cpp rcpp_interface.cpp \
          $(wildcard interface/*.cpp) \
          $(wildcard core/*.cpp) \
          $(wildcard solvers/*.cpp) \
          $(wildcard gabow_tarjan/*.cpp) \
          $(wildcard morph/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
```

**Makevars.win** (Windows):
- Same structure but uses `$(shell ...)` instead of backticks
- Collects all .cpp files from subdirectories using wildcards
- Automatically links all object files

### LAP Solver Implementations

Available algorithms (see [R/assignment.R](R/assignment.R)):
- `jv` - Jonker-Volgenant (general purpose, fast)
- `hungarian` - Classic Hungarian
- `auction` / `auction_gs` / `auction_scaled` - Auction variants
- `ssp` / `sap` - Shortest augmenting path
- `csflow` - Cost-scaling flow
- `cost_scaling` - Gabow's cost-scaling
- `cycle_cancel` - Cycle canceling with Karp
- `gabow_tarjan` - Gabow-Tarjan with complementary slackness
- `ssap_bucket` - Dial's algorithm for integer costs
- `line_metric` - Specialized for 1D matching
- `hk01` - Hopcroft-Karp for binary costs
- `bruteforce` - Exhaustive for tiny problems
- `auto` - Automatic selection

**Greedy Algorithms** (NEW):
- `greedy_sorted` - Sort pairs, assign greedily
- `greedy_row_best` - Row-by-row best match
- `greedy_pq` - Priority queue based

### Testing Strategy

**Test Organization (1382 total tests - all passing):**

```
tests/testthat/
├── LAP Solver Tests:
│   ├── test-assignment-hungarian.R       # Hungarian algorithm tests
│   ├── test-assignment-jv.R              # Jonker-Volgenant tests
│   ├── test-assignment-auction.R         # Auction variants
│   ├── test-assignment-*.R               # 14+ individual solver test files
│   └── test-lap-solve.R                  # Tidy interface tests
│
├── Matching Workflow Tests (98 tests):
│   └── test-matching.R                   # Comprehensive matching tests
│       ├── Preprocessing: 10 tests
│       ├── Balance diagnostics: 11 tests
│       ├── Joined datasets: 13 tests
│       ├── Distance caching: tests
│       ├── Parallel processing: tests
│       ├── Core matching: 9 tests
│       ├── Greedy strategies: 4 tests
│       └── Integration tests: remainder
│
├── Gabow-Tarjan Modular Tests:
│   ├── test_gabow_tarjan_moduleA.R       # Component testing
│   ├── test_gabow_tarjan_moduleB.R
│   ├── ...
│   └── test_gabow_tarjan_moduleH.R       # 8 module test files
│
├── Pixel Morphing Tests:
│   └── test-pixel-morph.R                # Morphing algorithm tests
│
└── Utility Tests:
    ├── test-utils.R                      # General utilities
    └── test-*.R                          # Other specialized tests
```

**Running Tests:**
```r
# Run all tests
devtools::test()  # or Ctrl+Shift+T in RStudio

# Run matching tests only
devtools::test(filter = "matching")

# Run specific test file
testthat::test_file("tests/testthat/test-matching.R")

# Run single test
testthat::test_that("balance_diagnostics works with blocking", { ... })

# Run with parallel execution
devtools::test(parallel = TRUE)
```

**Test Coverage Highlights:**
- All 14+ LAP algorithms have dedicated test files
- Edge cases: empty matches, NA handling, constant variables
- Backward compatibility: v0.1.0 tests still pass
- Performance: No regressions from v0.1.0
- Integration: Cross-feature testing (preprocessing + matching + diagnostics)

## Important Conventions

### Indexing
- **R code:** 1-based (R standard)
- **C++ code:** 0-based internally, convert at boundaries
- **Unmatched:** -1 (C++) / 0 (R, displayed as NA in output)

### Matrix Orientation
- Rows = left/treatment units
- Cols = right/control units
- Auto-transpose if rows > cols

### Cost Matrix Semantics
- `NA` or `Inf` = forbidden assignment
- `maximize = TRUE` negates costs internally
- Always returns costs on original scale

### Return Values

**LAP Solvers:**
```r
list(
  match = integer vector (1-based, length = nrow),
  total_cost = numeric,
  status = "optimal",
  method_used = "algorithm_name"
)
```

**Matching Functions:**
```r
list(
  pairs = tibble(left_id, right_id, distance, block_id),
  info = list(
    method = "optimal" or "greedy",
    strategy = "row_best" (greedy only),
    n_matched = integer,
    total_distance = numeric,
    n_blocks = integer (if blocking)
  ),
  # If return_diagnostics = TRUE:
  left_unmatched = tibble(...),
  right_unmatched = tibble(...),
  cost_matrix = matrix,
  distance_info = list(...)
)
```

**Balance Diagnostics:**
```r
balance_diagnostics object with:
  var_stats = tibble(variable, mean_left, mean_right, std_diff, ...),
  overall = list(mean_abs_std_diff, max_abs_std_diff, pct_large_imbalance),
  n_matched, n_unmatched_left, n_unmatched_right,
  has_blocks, block_stats
```

## Adding New Functionality

### Adding a New LAP Solver

1. Create `src/solvers/solve_foo.cpp` with `solve_foo_impl()`
2. Add forward declaration in `src/rcpp_interface.cpp`
3. Add `[[Rcpp::export]]` wrapper in `src/rcpp_interface.cpp`
4. Run `Rcpp::compileAttributes()` to update `R/RcppExports.R`
5. Add case to switch in `assignment()` ([R/assignment.R](R/assignment.R))
6. Create `tests/testthat/test-assignment-foo.R`
7. Update documentation via `devtools::document()`

### Adding Matching Features

**For preprocessing:**
- Edit [R/matching_preprocessing.R](R/matching_preprocessing.R)
- Add tests to [tests/testthat/test-matching.R](tests/testthat/test-matching.R)
- Update print methods if adding new diagnostics

**For balance diagnostics:**
- Edit [R/matching_diagnostics.R](R/matching_diagnostics.R)
- Extend `balance_diagnostics()` or add new functions
- Update print methods for new output

**For new matching algorithms:**
- Add C++ implementation in `src/solvers/`
- Export via `src/rcpp_interface.cpp`
- Add R wrapper in [R/matching_core.R](R/matching_core.R)

## Common Pitfalls

1. **Forgetting `Rcpp::compileAttributes()`** after C++ changes
   - Symptom: "could not find function" errors
   - Fix: Always run after adding/modifying `[[Rcpp::export]]`

2. **Exporting from subdirectories**
   - DON'T: Put `[[Rcpp::export]]` in `src/solvers/*.cpp`
   - DO: Forward declare and wrap in `src/rcpp_interface.cpp`

3. **0-based vs 1-based indexing**
   - Always convert at R/C++ boundary
   - C++ match vectors use -1 for unmatched, R uses 0/NA

4. **Not handling rectangular matrices**
   - Always test with nrow != ncol
   - Check auto-transpose logic

5. **Ignoring NA/Inf costs**
   - Test forbidden edges (calipers, blocking)
   - Verify cost computation skips invalid pairs

6. **Package name references**
   - OLD: lapr (deprecated)
   - NEW: couplr (current)
   - Check tests, examples, vignettes

7. **Object files in commits**
   - Clean before committing: `git clean -fdX src/` (Unix/Mac) or `Remove-Item src\*.o, src\*.dll -Force` (Windows)
   - Note: Some object files may already be in the repo - clean them if they cause issues
   - Consider adding `src/*.o` and `src/*.dll` to .gitignore if not already present

## Documentation Files

**Package Documentation:**
- [CLAUDE.md](CLAUDE.md) - This file - comprehensive development guide
- [CHANGELOG.md](CHANGELOG.md) - Detailed release notes (Keep a Changelog format)
- [NEWS.md](NEWS.md) - User-facing changes (R package convention)
- [DESCRIPTION](DESCRIPTION) - Package metadata, dependencies, authors
- [LICENSE](LICENSE) - MIT License

**Example Files (8 demos):**
```
examples/
├── auto_scale_demo.R              # 5 preprocessing demonstrations
├── balance_diagnostics_demo.R     # 6 balance diagnostic examples
├── join_matched_demo.R            # 8 joined dataset examples
├── parallel_matching_demo.R       # 7 parallel processing examples
├── error_messages_demo.R          # 10 fun error message demonstrations
└── (3 more demo files for distance caching, etc.)
```

**Vignettes (3 comprehensive guides):**
```
vignettes/
├── getting-started.Rmd            # Entry-point tutorial (394 lines)
├── algorithms.Rmd                 # Mathematical foundations (667 lines)
└── pixel-morphing.Rmd             # Advanced applications (884 lines)
```

**Implementation Notes (historical):**
- IMPLEMENTATION_STEP1.md - Automatic preprocessing implementation details
- IMPLEMENTATION_STEP2.md - Balance diagnostics implementation details
- MATCHING_ENHANCEMENTS.md - Feature roadmap and enhancement tracking
- SETUP_REQUIRED.md - Initial setup documentation for greedy matching

## Special Notes

### Pixel Morphing Semantics

Complex multi-mode system ([R/morph_pixel.R](R/morph_pixel.R)):

**Modes:**
- `exact` - Full pixel LAP (< 4096 pixels)
- `color_walk` - Color quantization + spatial LAPs
- `recursive` - Multi-scale 2×2 tiling

**CRITICAL rendering semantics:**
- Assignment uses BOTH images A and B
- Renderer uses ONLY A's colors (transport-only)
- B influences WHERE pixels go, not WHAT colors
- Result is sharp (no motion blur)

### Cost Computation Guarantee

All solvers MUST use `compute_total_cost()` from [src/core/lap_utils.cpp](src/core/lap_utils.cpp):

```cpp
double total_cost = compute_total_cost(original_cost_matrix, assignment_R);
```

**Never** compute cost from transformed/reduced/scaled matrices. The cost field has precise semantics: sum of original_cost[i, match[i]] over matched rows.

### Backward Compatibility

v1.0.0 is 100% backward compatible:
- All new parameters default to previous behavior
- `auto_scale = FALSE` by default
- No breaking changes to return structures
- All 1365 pre-existing tests still pass

## Vignette Writing Style Guide

### Existing Vignettes

The package currently has three vignettes that demonstrate a consistent, high-quality writing style:

1. **[getting-started.Rmd](vignettes/getting-started.Rmd)** - Entry-point tutorial (394 lines)
2. **[algorithms.Rmd](vignettes/algorithms.Rmd)** - Mathematical foundations (667 lines)
3. **[pixel-morphing.Rmd](vignettes/pixel-morphing.Rmd)** - Advanced applications (884 lines)

### Core Style Principles

**1. Progressive Complexity**
- Start with simplest use case (basic matrix input)
- Add complexity incrementally (data frames → rectangular → forbidden → maximization)
- Each section builds on previous concepts
- Clear "When to Use" sections for each method

**2. Mathematical Rigor with Accessibility**
- Formal definitions in LaTeX for precision
- Plain language explanations before/after equations
- Concrete examples immediately following theory
- Balance between mathematical correctness and readability

**3. Code-First Demonstration**
- Every concept illustrated with working code
- Self-contained examples using small, reproducible data
- Real-world scenarios (employee scheduling, particle tracking, molecular alignment)
- Comments explain the "why" not just the "what"

**4. Structured Narrative Flow**
```
Overview → Problem Definition → Basic Usage → Advanced Features →
Performance Considerations → Real-World Examples → Summary → Further Reading
```

**5. Visual Communication**
- Use tables for algorithm comparisons
- Include complexity notation (O(n³), O(n²))
- Show decision trees (when to use which algorithm)
- Pixel morphing vignette uses embedded images/GIFs for visual learning

**6. Cross-Referencing**
- Link between vignettes at appropriate moments
- Point to function documentation (`?lap_solve`)
- Reference GitHub repository for contributions
- Suggest "Learn more" pathways at end of each major section

**7. Practical Guidance**
- Performance benchmarks with `system.time()`
- Memory/scalability considerations
- "When NOT to use" sections
- Common pitfalls and solutions

**8. Code Formatting Standards**
```r
# Use descriptive variable names
cost <- matrix(c(...), nrow = 3, byrow = TRUE)

# Inline comments for complex logic
result <- lap_solve(cost)  # Defaults to method = "auto"

# Show actual output
print(result)
```

**9. Pseudocode for Algorithms**
- Indented, readable structure
- Clear variable naming conventions
- Step-by-step annotations
- Mix of mathematical notation and plain English

**10. Domain-Specific Applications**
- Ecology: vegetation plot matching with Bray-Curtis dissimilarity
- Physics: particle tracking with velocity prediction
- Chemistry: molecular alignment with RMSD
- Computer Vision: pixel morphing for intuitive visualization

### Recommended Future Vignettes

**Priority 1 - Matching Workflows (NEW in v1.0.0)**

1. **"matching-workflows.Rmd"** - Production matching guide
   - **Why:** v1.0.0 added major matching functionality (`match_couples()`, `greedy_couples()`, `balance_diagnostics()`) with NO dedicated vignette
   - **Content:**
     - Treatment effect estimation workflow
     - Preprocessing: auto-scaling, health checks, categorical encoding
     - Optimal vs greedy trade-offs
     - Blocking/stratification with `matchmaker()`
     - Balance assessment and interpretation
     - Publication-ready tables
   - **Target audience:** Researchers, epidemiologists, economists
   - **Estimated length:** 600-800 lines

2. **"balance-diagnostics.Rmd"** - Understanding match quality
   - **Why:** Balance assessment is complex with multiple metrics (std diff, variance ratios, KS tests)
   - **Content:**
     - What is balance and why it matters
     - Interpreting standardized differences (<0.1 excellent, 0.1-0.25 good, etc.)
     - Variance ratios for distributional similarity
     - KS tests for non-normal variables
     - Handling imbalanced covariates
     - Sensitivity analysis
   - **Target audience:** Applied researchers needing causal inference
   - **Estimated length:** 400-500 lines

**Priority 2 - Algorithm Selection**

3. **"algorithm-selection.Rmd"** - Choosing the right solver
   - **Why:** 14+ algorithms can overwhelm users; `method = "auto"` hides important details
   - **Content:**
     - Decision flowchart for algorithm selection
     - Benchmarks across problem types (dense, sparse, rectangular, binary)
     - Memory vs speed trade-offs
     - When to override `auto` selection
     - Auction variants comparison (standard, scaled, Gauss-Seidel)
     - Greedy vs optimal: speed vs quality
   - **Target audience:** Power users, performance-critical applications
   - **Estimated length:** 500-600 lines

**Priority 3 - Domain Applications**

4. **"ecological-applications.Rmd"** - Matching in ecology
   - **Why:** Pixel-morphing vignette mentions ecology but doesn't show real examples
   - **Content:**
     - Vegetation plot matching across time periods
     - Species composition similarity (Bray-Curtis, Jaccard)
     - Spatial autocorrelation handling
     - Hierarchical matching for large surveys (>1000 plots)
     - Climate matching for transplant experiments
     - Animal pairing for behavioral studies
   - **Target audience:** Ecologists, conservation biologists
   - **Estimated length:** 600-700 lines

5. **"observational-studies.Rmd"** - Causal inference with matching
   - **Why:** Major use case for matching but not explicitly covered
   - **Content:**
     - Treatment effect estimation framework
     - Propensity score matching
     - Nearest neighbor with calipers
     - Exact matching on key variables
     - Sensitivity analysis (Rosenbaum bounds)
     - Reporting standards (STROBE guidelines)
   - **Target audience:** Epidemiologists, health services researchers
   - **Estimated length:** 700-800 lines

**Priority 4 - Advanced Topics**

6. **"large-scale-matching.Rmd"** - Handling massive datasets
   - **Why:** Pixel-morphing shows approximation strategies but needs dedicated guide
   - **Content:**
     - When exact LAP becomes impractical (n > 5,000)
     - Feature quantization strategy (reduce problem size)
     - Hierarchical decomposition (divide-and-conquer)
     - Resolution reduction (coarse-to-fine)
     - Greedy algorithms: sorted, row_best, priority queue
     - Parallel batch solving
     - Memory management
   - **Target audience:** Data scientists, engineers with large-n problems
   - **Estimated length:** 500-600 lines

7. **"k-best-solutions.Rmd"** - Exploring alternatives
   - **Why:** `lap_solve_kbest()` exists but lacks usage guidance
   - **Content:**
     - Murty's algorithm explanation
     - When you need multiple solutions
     - Robustness analysis
     - Cost landscape exploration
     - Sensitivity to perturbations
     - Alternative planning scenarios
   - **Target audience:** Decision scientists, operations researchers
   - **Estimated length:** 400-500 lines

8. **"custom-distance-metrics.Rmd"** - Beyond Euclidean
   - **Why:** Many domains need specialized similarity measures
   - **Content:**
     - Mahalanobis distance for correlated variables
     - Gower distance for mixed-type data
     - String edit distance for text matching
     - Network-based distances
     - Spatial autocorrelation adjustments
     - Creating custom cost matrices
   - **Target audience:** Methodologists, statisticians
   - **Estimated length:** 450-550 lines

**Priority 5 - Specialized Use Cases**

9. **"time-series-matching.Rmd"** - Temporal alignment
   - **Why:** Natural extension to particle tracking example
   - **Content:**
     - Panel data matching
     - Trajectory alignment
     - Dynamic time warping connections
     - Temporal constraints (max displacement)
     - Velocity-predicted matching
     - Event sequence alignment
   - **Target audience:** Physicists, video analysts, signal processors
   - **Estimated length:** 500-600 lines

10. **"image-registration.Rmd"** - Computer vision applications
   - **Why:** Pixel-morphing shows potential but doesn't cover full CV workflow
   - **Content:**
     - Feature point matching (SIFT, SURF)
     - Image alignment and warping
     - Multi-resolution strategies
     - Color space considerations
     - Real-time performance
     - Integration with image processing packages
   - **Target audience:** Computer vision researchers, photographers
   - **Estimated length:** 550-650 lines

### Vignette Template Structure

```rmd
---
title: "Title in Title Case"
author: "Gilles Colling"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Title in Title Case}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(couplr)


## Overview

[2-3 paragraphs explaining: what problem does this solve, who is it for, what will they learn]

**Key Features:** [3-5 bullet points]

**Related vignettes:** [Cross-references]

## Problem Definition

[Mathematical formulation if applicable, with LaTeX]

[Plain language explanation]

## Basic Usage

[Simplest possible example with minimal code]

{r basic-example}
# Code here


## Advanced Features

[Progressive complexity, each section follows:]
### Feature Name
[Explanation]
{r feature-demo}
[When to use / when NOT to use]

## Real-World Example

[Complete workflow with domain-specific data]

## Performance Considerations

[Benchmarks, scalability, memory usage]

## Summary

[Recap key points in 3-5 bullets]

**Learn more:**
- [Links to related vignettes]
- [Function documentation]
- [External resources]
```

### Writing Checklist

Before submitting a new vignette:

- [ ] Follows progressive complexity principle
- [ ] Every concept has working code example
- [ ] Cross-references to related vignettes
- [ ] Includes "When to use" guidance
- [ ] Shows real-world application
- [ ] Performance/scalability notes
- [ ] Summary with "Learn more" section
- [ ] Mathematical notation is correct (if used)
- [ ] Code is self-contained and reproducible
- [ ] Proper attribution for algorithms/methods
- [ ] Builds successfully with `devtools::build_vignettes()`

## Package Dependencies

**System Requirements:**
- **R** ≥ 3.5.0
- **C++17** compiler (Rtools for Windows with g++ supporting C++17)
- **RcppEigen** for matrix operations

**Core Dependencies (Imports):**
```r
Rcpp (>= 1.0.0)           # C++ integration
RcppEigen                 # Matrix operations
tibble (>= 3.0.0)         # Modern data frames
dplyr (>= 1.0.0)          # Data manipulation
rlang (>= 0.4.0)          # Tidy evaluation
purrr (>= 0.3.0)          # Functional programming
magrittr (>= 2.0.0)       # Pipe operator
```

**Suggested Dependencies (Optional):**
```r
# Testing
testthat (>= 3.0.0)       # Unit testing framework
withr                     # Temporary state management

# Documentation
knitr                     # Vignette generation
rmarkdown                 # Documentation rendering

# Performance
bench                     # Benchmarking
parallel                  # Parallel computation
future (>= 1.20.0)        # Async parallel framework
future.apply (>= 1.8.0)   # Future-compatible apply functions

# Image processing (for pixel morphing)
magick                    # ImageMagick R bindings
OpenImageR                # Image processing utilities
farver                    # Fast color space manipulation
av                        # Video encoding
reticulate                # Python integration
png                       # PNG file handling
```

**LinkingTo:**
- Rcpp - R/C++ interface
- RcppEigen - High-performance matrix library

## Development History

**v1.0.0 (2025-11-19) - Major Release**

Complete rewrite with 6 major enhancement steps:

1. **Automatic Preprocessing** - Variable health checks, smart scaling, categorical encoding
2. **Balance Diagnostics** - Standardized differences, variance ratios, KS tests, quality ratings
3. **Joined Dataset Output** - `join_matched()`, `augment()` for analysis-ready data
4. **Distance Caching** - `compute_distances()` for reusable precomputed distances
5. **Parallel Processing** - Block matching parallelization via future framework
6. **Fun Error Messages** - Couple-themed messages with optional emoji support

**Key Metrics:**
- 1382 tests passing (up from 1365)
- 100% backward compatible
- 20 R files, 22 C++ files
- 3 vignettes, 8+ example files
- Renamed from lapr to couplr

**v0.1.0 - Initial Release**
- 14+ LAP solver algorithms
- Basic matching workflows
- Pixel morphing for visualization
- Tidy interface with tibble output

---

## Quick Reference: Common Workflows

### 1. Basic Optimal Matching
```r
library(couplr)

# Match with automatic preprocessing
result <- match_couples(
  left = treatment_df,
  right = control_df,
  vars = c("age", "income", "education"),
  auto_scale = TRUE,
  max_distance = 0.5,
  return_diagnostics = TRUE
)

# Check balance
balance <- balance_diagnostics(result, treatment_df, control_df, vars)
print(balance)

# Get analysis-ready dataset
matched_data <- join_matched(result, treatment_df, control_df)
```

### 2. Fast Greedy Matching for Large Data
```r
# Use greedy algorithm for speed
result <- greedy_couples(
  left = large_treatment_df,
  right = large_control_df,
  vars = c("age", "income"),
  strategy = "row_best",  # or "sorted" or "pq"
  auto_scale = TRUE
)
```

### 3. Matching with Blocking
```r
# Create blocks
blocks <- matchmaker(
  left_df, right_df,
  block_type = "group",
  block_by = "site"
)

# Match within blocks (optionally in parallel)
result <- match_couples(
  left_df, right_df,
  vars = c("age", "income"),
  block_id = blocks$block_id,
  parallel = TRUE
)
```

### 4. Distance Caching for Experimentation
```r
# Compute distances once
dist_obj <- compute_distances(
  left_df, right_df,
  vars = c("age", "income", "education"),
  scale = "robust"
)

# Try different constraints quickly
result1 <- match_couples(dist_obj, max_distance = 0.3)
result2 <- match_couples(dist_obj, max_distance = 0.5)
result3 <- greedy_couples(dist_obj, strategy = "sorted")

# Update constraints without recomputing
dist_obj2 <- update_constraints(dist_obj, max_distance = 0.4)
```

### 5. LAP Solving (Low-Level)
```r
# Create cost matrix
cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)

# Solve with specific algorithm
result <- lap_solve(cost, method = "jv")  # Jonker-Volgenant

# Or let the package choose
result <- lap_solve(cost, method = "auto")
```

### 6. Publication Workflow
```r
# 1. Match
result <- match_couples(left, right, vars, auto_scale = TRUE)

# 2. Assess balance
balance <- balance_diagnostics(result, left, right, vars)

# 3. Create publication table
balance_table(balance, digits = 3)

# 4. Get matched dataset for analysis
data <- join_matched(result, left, right,
                     left_vars = c("outcome", "covariate1"),
                     right_vars = c("covariate1"))

# 5. Run analysis on matched data
lm(outcome_left ~ covariate1_left + covariate1_right, data = data)
```

---

**For more information:**
- See vignettes: `browseVignettes("couplr")`
- View examples: `?match_couples`, `?balance_diagnostics`
- Check issues: https://github.com/gcol33/couplr/issues
