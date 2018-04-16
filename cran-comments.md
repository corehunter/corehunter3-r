This is a resubmission that addresses the NOTEs raised by CRAN's automatic checks.
The following changes have been made:
 * updated Date field

## Test environments

* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.2 & R devel
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE on Travis CI:

* checking R code for possible problems ... NOTE
  Picked up _JAVA_OPTIONS: -Xmx2048m -Xms512m

  This note is due to the Travis build server's Java options.

## Downstream dependencies

There are currently no downstream dependencies for this package.
