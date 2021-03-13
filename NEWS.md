# rcorex 0.2.1
 * Fixed bug in `check_converged` function. The TCs for eah hidden node should have been summed at each iteration before checking convergence and they were not. This is fixed now which improves detection of convergence.
 
# rcorex 0.2.0

* Removed `sig_ml` minimal value in `estimate_parameters_gaussian()` function
* Added `minmarg` to replace `sig_ml` minimal value. `minmarg` imposes a minimal value (should be a negative number) on individual log_marginal values
* Added `epicorex()` function which allows the use of different marginal descriptions for different variables of a dataset. Added `bernoulli` marginal description for binary data (this is an optimised version of the `discrete` marginal description)

# rcorex 0.1.0

* Added a `NEWS.md` file to track changes to the package
* Added `biocorex()`, `plot.rcorex()` and `make_corex_tidygraph()` functions
