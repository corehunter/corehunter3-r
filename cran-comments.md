## Submission

This version fixes the test failures on r-devel reported by CRAN, caused by
the change to `str()` output in R-devel r90805 (no longer prepending a space).
The print methods of the package's data objects now format this output
consistently across R versions.

## Test environments

* GitHub actions:
  * Windows Server 2022: R 4.6.2
  * macOS Tahoe 26.7: R 4.6.2
  * Ubuntu 26.04: R 4.6.2, R 4.5.3 & R devel (2026-11-08 r90859)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked 1 reverse dependency, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
