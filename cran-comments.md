## R CMD check results

0 errors | 1 warning | 1 note

### WARNING: Found 'abort' in compiled code
The 'abort' symbol appears in the compiled code due to the C++ exception
handling mechanism used by Rcpp::stop() for error reporting. This is a
standard Rcpp pattern used by many CRAN packages. The code never calls
abort() directly - the symbol is linked as part of the C++ runtime ABI
for exception handling. All errors are properly signaled to R via
Rcpp::stop() which throws a catchable exception.

### NOTE: "unable to verify current time"
This is a network-related check that does not affect package functionality.

## Test environments

* local Windows 11 x64, R 4.5.2
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* R-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel

## Downstream dependencies

None (new package).

## Notes

This is a new submission.

couplr provides optimal pairing and matching via linear assignment algorithms,
with 20 LAP solvers and production-ready matching workflows for causal inference
and observational studies.
