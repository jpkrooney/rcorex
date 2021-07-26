# rcorex 0.2.3
* Removed `minmarg` as argument from `biocorex()` and `epicorex()`
* Restored `sig_ml` minimal value to the `estimate_parameters_gaussian()` function to prevent divide by zero errors in the `marginal_p_gaussian()` function. However, now the minimal value is instead set to the machine floating point minimum value. As a result, `biocorex()` may sometimes produce negative `tcs`. This is an indication that the data is non-gaussian and such results should not be trusted. This might occur, for example, when binary data is fit with a gaussian marginal description.
* Minor code optimization to speed up calculating the discrete and bernoulli
* Summary methods added
* Experimental option logpx_method added. This allows use of an alternative method of calculating log_p_xi.

# rcorex 0.2.2
* Minor code optimization to speed up calculating the gaussian marginals

# rcorex 0.2.1
* Fixed bug in `check_converged` function. The TCs for each hidden node should have been summed at each iteration before checking convergence and they were not. This is fixed now which improves detection of convergence.
 
# rcorex 0.2.0

* Removed `sig_ml` minimal value in `estimate_parameters_gaussian()` function
* Added `minmarg` to replace `sig_ml` minimal value. `minmarg` imposes a minimal value (should be a negative number) on individual log_marginal values. Default is -10.
* Added `epicorex()` function which allows the use of different marginal descriptions for different variables of a dataset. Added `bernoulli` marginal description for binary data (this is an optimised version of the `discrete` marginal description)

# rcorex 0.1.0

* Added a `NEWS.md` file to track changes to the package
* Added `biocorex()`, `plot.rcorex()` and `make_corex_tidygraph()` functions
