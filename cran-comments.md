## Test environments

* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.2.5, R 3.3.1 & R devel
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE on win-builder:

* checking CRAN incoming feasibility ... NOTE

	Maintainer: 'Herman De Beukelaer <herman.debeukelaer@gmail.com>'
	
	Days since last update: 1
	
	License components with restrictions and base license permitting such:
	  MIT + file LICENSE
	File 'LICENSE':
	  YEAR: 2016
	  COPYRIGHT: Herman De Beukelaer, Guy Davenport
	
	Possibly mis-spelled words in DESCRIPTION:
	  Multi (2:8)
	  allelic (13:28)
	  germplasm (8:89)
	  phenotypic (11:26)
	  precomputed (11:47)
	  representativeness (13:5)

It has only been one day since version 3.0.0 was deployed because this version 3.0.1 is a hotfix as requested by Prof. Brian Ripley that fixes the issues described above. The words flagged by the spellchecker are correctly spelled. 

## Downstream dependencies

There are currently no downstream dependencies for this package.
