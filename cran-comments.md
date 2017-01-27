## Test environments

* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.2.5, R 3.3.2 & R devel
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE on win-builder:

* checking CRAN incoming feasibility ... NOTE

	Maintainer: 'Herman De Beukelaer <herman.debeukelaer@gmail.com>'

	License components with restrictions and base license permitting such:
	  MIT + file LICENSE
	File 'LICENSE':
	  YEAR: 2016, 2017
	  COPYRIGHT: Herman De Beukelaer, Guy Davenport
	
	Possibly mis-spelled words in DESCRIPTION:
	  Multi (2:8)
	  allelic (13:28)
	  germplasm (8:89)
	  phenotypic (11:26)
	  precomputed (11:47)
	  representativeness (13:5)

This note was mainly due to the MIT license referring to a separate LICENSE file. The words flagged by the spell checker are correctly spelled. 

## Downstream dependencies

There are currently no downstream dependencies for this package.
