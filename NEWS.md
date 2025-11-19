# couplr 1.0.0

## Major New Features (2025-11-19 Update)

### Automatic Preprocessing and Scaling

The package now includes intelligent preprocessing to improve matching quality:

* **New `auto_scale` parameter** in `match_couples()` and `greedy_couples()` enables automatic preprocessing
* **Variable health checks** detect and handle problematic variables:
  - Constant columns (SD = 0) are automatically excluded with warnings
  - High missingness (>50%) triggers warnings
  - Extreme skewness (|skewness| > 2) is flagged
* **Smart scaling method selection** analyzes data and recommends:
  - "robust" scaling using median and MAD (resistant to outliers)
  - "standardize" for traditional mean-centering and SD scaling
  - "range" for min-max normalization
* New `preprocess_matching_vars()` function for manual preprocessing control
* Categorical variable encoding for binary and ordered factors

### Balance Diagnostics

Comprehensive tools to assess matching quality:

* **New `balance_diagnostics()` function** computes multiple balance metrics:
  - Standardized differences: (mean_left - mean_right) / pooled_sd
  - Variance ratios: SD_left / SD_right
  - Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
* **Quality thresholds** with interpretation:
  - |Std Diff| < 0.10: Excellent balance
  - |Std Diff| 0.10-0.25: Good balance
  - |Std Diff| 0.25-0.50: Acceptable balance
  - |Std Diff| > 0.50: Poor balance
* Per-block statistics with quality ratings when blocking is used
* `balance_table()` creates publication-ready formatted tables
* Informative print methods with interpretation guides

### New Functions

* `preprocess_matching_vars()` - Main preprocessing orchestrator
* `balance_diagnostics()` - Comprehensive balance assessment
* `balance_table()` - Formatted balance tables for reporting
* Added robust scaling method using median and MAD

### Documentation & Examples

* `examples/auto_scale_demo.R` - 5 preprocessing demonstrations
* `examples/balance_diagnostics_demo.R` - 6 balance diagnostic examples
* Complete implementation documentation (IMPLEMENTATION_STEP1.md, IMPLEMENTATION_STEP2.md)
* All functions have full Roxygen documentation

### Tests

* Added 21 new tests (10 for preprocessing, 11 for balance diagnostics)
* All 1369 tests passing with full backward compatibility

## Major Changes (Initial 1.0.0 Release)

### Package Renamed: lapr → couplr

The package has been renamed from **lapr** to **couplr** to better reflect its purpose as a general pairing and matching toolkit.

**couplr** = Optimal pairing and matching via linear assignment

### Clean 1.0.0 Release

First official stable release with clean, well-organized codebase.

## New Organization

### R Code
- Eliminated 3 redundant files
- Consistent `morph_*` naming prefix
- Two-layer API: `assignment()` (low-level) + `lap_solve()` (tidy)
- 10 well-organized files (down from 13)

### C++ Code  
- Modular subdirectory structure:
  - `src/core/` - Utilities and headers
  - `src/interface/` - Rcpp exports
  - `src/solvers/` - 14 LAP algorithms
  - `src/gabow_tarjan/` - Gabow-Tarjan solver
  - `src/morph/` - Image morphing

## Features

### Solvers
Hungarian, Jonker-Volgenant, Auction (3 variants), SAP/SSP, SSAP-Bucket, Cost-scaling, Cycle-cancel, Gabow-Tarjan, Hopcroft-Karp, Line-metric, Brute-force, Auto-select

### High-Level
✅ Tidy tibble interface
✅ Matrix & data frame inputs  
✅ Grouped data frames
✅ Batch solving + parallelization
✅ K-best solutions (Murty, Lawler)
✅ Rectangular matrices
✅ Forbidden assignments (NA/Inf)
✅ Maximize/minimize
✅ Pixel morphing visualization

## API

- `lap_solve()` - Main tidy interface
- `lap_solve_batch()` - Batch solving
- `lap_solve_kbest()` - K-best solutions
- `assignment()` - Low-level solver
- Utilities: `get_total_cost()`, `as_assignment_matrix()`, etc.
- Visualization: `pixel_morph()`, `pixel_morph_animate()`

---

*Development history under "lapr" available in git log before v1.0.0.*
