## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: "Days since last update: 0"
This is a hotfix for UBSAN and vignette failures in v1.0.6/1.0.7.

## Test environments

* local Windows 11 x64, R 4.5.2
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-devel

## Downstream dependencies

None.

## Resubmission

This is a **hotfix** addressing failures detected during CRAN's additional checks:

### Issue 1: gcc-UBSAN failure (C++ undefined behavior)
**Source:** https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/couplr

**Error:** Left shift of negative value -1 in `utils_gabow_tarjan.cpp:1384`

**Fix:** Replaced `c_current[i][j] << 1` with `c_current[i][j] * 2` to avoid
undefined behavior when intermediate cost values are negative (which can occur
during the bit-scaling phase of the Gabow-Tarjan algorithm).

### Issue 2: M1mac vignette failure
**Source:** https://www.stats.ox.ac.uk/pub/bdr/M1mac/couplr.out

**Error:** `Error in select(): unused arguments (variable, std_diff)` in
comparison.Rmd during vignette rebuild.

**Root cause:** The `do.call(rbind, lapply(list, as.data.frame))` pattern in
`balance_diagnostics()` was converting named lists to a matrix with generic
column names instead of preserving the original names like `variable` and
`std_diff`.

**Fix:** Replaced with `dplyr::bind_rows(lapply(list, tibble::as_tibble))`
which properly preserves column names. Applied same fix to:
- `R/matching_diagnostics.R` (2 locations)
- `R/matching_preprocessing.R` (1 location)
- `R/matching_parallel.R` (2 locations)
