% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcorex-package.R
\name{rcorex-package}
\alias{rcorex-package}
\title{An Implementation of Total Correlation Explanation}
\description{
rcorex implements Total Correlation Explanation (CorEx) in R.
CorEx is a computationally efficient algorithm which uses
multivariate mutual information to discover latent factors which
best explain correlations in a dataset. CorEx is computationally
efficient and is easily extended to discover hierarchical structure.

The main function in the \code{rcorex} package is \code{\link{biocorex}}, model convergence can be visualised via \code{\link{plot.rcorex}}, and a tidygraph object of discovered structure can be created using \code{\link{make_corex_tidygraph}}.
}
