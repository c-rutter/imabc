## Resubmission
This is a resubmission. In this version I have:
* Made sure not to start the description with "This package", package name, title or similar.
* Added reference to the methods paper behind the algorithm in the Description field.
* Added \value to read_previous_results.Rd
* Adjusted code to restore .GlobalEnv to previous state when worked with.
  * This includes stringsAsFactors in read_previous_results.R for backwards compatibility with R < 4.0.0
  * This includes working with .Random.seed in function created by define_target_function.R
  * This includes working with RNGkind in imabc.R

## Test environments
* local OS X install, R 4.0.5
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: '"Christopher, E. Maerzluft" <cmaerzlu@rand.org>'

## Downstream dependencies
There are currently no downstream dependencies for this package
