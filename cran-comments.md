## Test environments

* local OS X install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.5, R 3.3.1 & R devel
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE because this is the first submission of the package:

* checking CRAN incoming feasibility ... NOTE

	Maintainer: ‘Herman De Beukelaer <herman.debeukelaer@gmail.com>’
	New submission
	
	License components with restrictions and base license permitting such:
	  MIT + file LICENSE
	File 'LICENSE':
	  YEAR: 2016
	  COPYRIGHT: Herman De Beukelaer, Guy Davenport
	  
Only on win-builder the same note also says

 * Possibly mis-spelled words in DESCRIPTION:
	  Gower's (12:25)
	  Shannon's (11:56)
	  genotypes (12:10)
	  phenotypes (12:47)
	  phenotypic (9:60)
	  precomputed (10:7)

Each of these words is however correctly spelled.

## Downstream dependencies

There are currently no downstream dependencies for this package.
