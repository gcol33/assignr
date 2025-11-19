# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-11-19

### Added

#### Matching Enhancements - Step 1: Automatic Scaling and Preprocessing
- **Automatic preprocessing** with `auto_scale` parameter in `match_couples()` and `greedy_couples()`
- **Variable health checks** via `check_variable_health()`:
  - Detects constant columns (SD = 0) and excludes with warning
  - Detects nearly-constant columns (SD < threshold) and warns
  - Detects all-NA columns and excludes with warning
  - Detects high missingness (>50%) and warns
  - Detects extreme skewness (|skewness| > 2) and provides info messages
  - Returns detailed diagnostics including per-variable statistics
- **Smart scaling method selection** via `suggest_scaling()`:
  - Analyzes variable distributions and detects outliers using IQR method
  - Checks for different scales across variables
  - Recommends "robust", "standardize", "range", or "none"
- **Robust scaling method** added to `apply_scaling()`:
  - Uses median and MAD (median absolute deviation)
  - Resistant to outliers and skewed distributions
  - Formula: `(x - median) / MAD`
- **Categorical variable encoding** via `auto_encode_categorical()`:
  - Binary variables → 0/1 encoding
  - Ordered factors → numeric codes
  - Error for unordered categorical (requires Gower distance)
- **Main preprocessing orchestrator** `preprocess_matching_vars()`:
  - Runs variable health checks
  - Automatically excludes problematic variables
  - Suggests and applies scaling method
  - Returns preprocessing result with metadata
  - Exported for direct user access
- **Print methods** for preprocessing results:
  - `print.variable_health()` - Pretty printing for health diagnostics
  - `print.preprocessing_result()` - Summary of preprocessing results
- **10 comprehensive tests** for preprocessing functionality
- **Example file** `examples/auto_scale_demo.R` with 5 demonstrations

#### Matching Enhancements - Step 2: Balance Diagnostics
- **Balance diagnostics** via `balance_diagnostics()`:
  - Computes standardized differences (mean diff / pooled SD)
  - Calculates variance ratios (SD_left / SD_right)
  - Performs Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
  - Per-block statistics with quality ratings when blocking used
  - Counts matched and unmatched units
  - Works with both `match_couples()` and `greedy_couples()` results
- **Balance table formatting** via `balance_table()`:
  - Clean tabular output suitable for reports and publications
  - Configurable decimal precision
- **Standardized difference calculation** via `standardized_difference()`:
  - Internal function with robust edge case handling
  - Supports pooled or group-specific SD
  - Handles NA values, empty vectors, constant data
- **Helper function** `calculate_var_balance()`:
  - Per-variable balance statistics
  - Mean, SD, mean difference for both groups
  - Standardized difference, variance ratio, KS statistic
- **Print method** for balance diagnostics:
  - `print.balance_diagnostics()` - Comprehensive output with:
    - Matching summary (method, matched/unmatched counts)
    - Variable-level balance table
    - Overall balance assessment with quality ratings
    - Block-level statistics (if blocking used)
    - Interpretation guide for standardized differences
- **Block-level quality ratings**:
  - Excellent: |Std Diff| < 0.10
  - Good: |Std Diff| 0.10-0.25
  - Fair: |Std Diff| 0.25-0.50
  - Poor: |Std Diff| > 0.50
  - Unknown: For edge cases (empty blocks, all NA)
- **11 comprehensive tests** for balance diagnostics
- **Example file** `examples/balance_diagnostics_demo.R` with 6 demonstrations:
  - Basic balance diagnostics
  - Comparing optimal vs greedy matching
  - Balance with blocking
  - Detecting poor balance
  - Extracting specific metrics
  - Using balance tables for publication

### Changed
- Updated `DESCRIPTION` to include new matching features
- Enhanced `NAMESPACE` with new exports:
  - `preprocess_matching_vars()`
  - `balance_diagnostics()`
  - `balance_table()`
  - S3 print methods for new classes

### Fixed
- Greedy matching functions now properly exported via Rcpp interface
- Block statistics preserved correctly in `greedy_couples()` when `return_diagnostics = FALSE`
- Variable health checks now include all required fields for all edge cases
- Robust edge case handling in balance diagnostics:
  - Variance ratio calculation handles NA values
  - Block quality determination handles NA/NaN from empty blocks
  - Overall metrics handle all-NA standardized differences

### Documentation
- Added `IMPLEMENTATION_STEP1.md` documenting preprocessing implementation
- Added `IMPLEMENTATION_STEP2.md` documenting balance diagnostics implementation
- Updated Roxygen documentation for all new functions
- Created comprehensive examples demonstrating new features
- All functions have complete parameter descriptions and return value documentation

### Tests
- **Total: 1369 tests passing** (up from 1365)
- Added 10 tests for preprocessing (Step 1)
- Added 11 tests for balance diagnostics (Step 2)
- All existing tests continue to pass
- No warnings or errors
- Test coverage includes:
  - Variable health detection (constant, all-NA, high missingness, skewness)
  - Scaling suggestion
  - Preprocessing integration
  - Standardized difference calculation
  - Balance diagnostics with simple and blocked matching
  - Balance table formatting
  - Print methods
  - Input validation
  - Edge case handling

### Backward Compatibility
- **100% backward compatible**
- All new parameters default to `FALSE` or previous behavior
- No breaking changes to existing APIs
- All existing code continues to work unchanged
- Return structures extended, not replaced

---

## [0.1.0] - Previous Release

Initial release with core LAP solving functionality, basic matching, and pixel morphing.

[1.0.0]: https://github.com/gcol33/couplr/releases/tag/v1.0.0
[0.1.0]: https://github.com/gcol33/couplr/releases/tag/v0.1.0
