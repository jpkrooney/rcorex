
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcorex

<!-- badges: start -->

[![R build
status](https://github.com/jpkrooney/rcorex/workflows/R-CMD-check/badge.svg)](https://github.com/jpkrooney/rcorex/actions)
<!-- badges: end -->

Total correlation explanation is an unsupervised learning algorithm for
discovering structure in high dimensional data. Total correlation
explanation has been implemented in Python as CorEx and related modules
(<https://github.com/gregversteeg/CorEx>). The initial aim of rcorex is
to implement Total Correlation Explanation in the R statistical
software, specifically to replicate the functionality of the Bio CorEx
Python module ( <https://github.com/gregversteeg/bio_corex> ).

The theoretical framework behind the CorEx and Bio CorEx Python modules
are laid out in the following academic papers:  
1\. [Discovering Structure in High-Dimensional Data Through Correlation
Explanation](http://arxiv.org/abs/1406.1222)  
2\. [Maximally Informative Hierarchical Representions of
High-Dimensional Data](https://arxiv.org/abs/1410.7404)  
3\. [Comprehensive discovery of subsample gene expression components by
information explanation: therapeutic implications in
cancer](https://bmcmedgenomics.biomedcentral.com/articles/10.1186/s12920-017-0245-6)

## Installation

rcorex can be installed from Github:

``` r
devtools::install_github("jpkrooney/rcorex")
```

## Minimal example

To fit a CorEx model in rcorex we can use the `biocorex()` command.
However, we first need to pay attention to the data types. Biocorex
accepts a data.frame as input, however as with the Python implementation
of Bio CorEx, all variables must have the same data-type and currently
only discrete or continuous data are allowed.

As a brief example we will use the iris dataset. The iris data set has 4
continuous variables and one factor variable. Therefore in order to fit
the iris dataset using `biocorex()` we must either omit the `Species`
variable of convert it to dummy variables. Since this is an important
variable in this dataset and we do not wish to omit it we will convert
if to dummy variables using `model.matrix()`.

``` r
data(iris)
# Use model matrix to make dummy vars - note we exclude the Intercept column produced
dummies <- model.matrix(~Species, iris)[,-1]
# join dummy vars to iris
iris <- data.frame(iris, dummies)
# make a copy of the original variable for later comparison
species <- iris$Species
# remove original as we don't want to put this into biocorex
iris$Species <- NULL
```

With the data prepared, we are now ready to fit `biocorex()`

``` r
library(rcorex)
# fit biocorex
set.seed(1234)
fit1 <- biocorex(iris, n_hidden = 1, dim_hidden = 3, marginal_description = "gaussian")
#> Calculating single iteration of corex

# Plot the model convergence
plot(fit1)
```

<img src="man/figures/README-layer1-1.png" width="100%" />

``` r

# extract variable clusters
fit1$clusters
#>      Sepal.Length       Sepal.Width      Petal.Length       Petal.Width 
#>                 0                 0                 0                 0 
#> Speciesversicolor  Speciesvirginica 
#>                 0                 0
# all variables assigned to cluster 0 so this is not interesting in this case

# look at row labels (i.e. dimension of each latent variable assigned to each row)
head(fit1$labels)
#>      [,1]
#> [1,]    1
#> [2,]    1
#> [3,]    1
#> [4,]    1
#> [5,]    1
#> [6,]    1

# Lets compare the labels to the species variable saved earlier
table(fit1$labels, species)
#>    species
#>     setosa versicolor virginica
#>   0      0         50         1
#>   1     50          0         0
#>   2      0          0        49
```

`biocorex()` has correctly labeled the species for all but 1 row.
