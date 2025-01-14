% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdquant.R
\name{rdquant}
\alias{rdquant}
\title{Quantile regression in regression discontinuity framework 
see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"}
\usage{
rdquant(Y, x, fuzzy = NULL, c = 0, grid = 0.01, qstep = 0.05, ...)
}
\arguments{
\item{Y}{a numeric vector}

\item{x}{a numeric vector}

\item{fuzzy}{a binary vector indicating dummy for treatment}

\item{c}{a double for the cutoff point}

\item{grid}{a double between 0 and 1 controlling how finely to estimate the pdfs}

\item{qstep}{a double between 0 and 1 controlling which quantile treatment effects to return}
}
\value{
a dataframe of coefficients, standard errors and quantile treatment effect standard errors, and rearranged estimates, and a vector of quantile treatment effect estimates
}
\description{
Quantile regression in regression discontinuity framework 
see Frandsen, Frolich and Melly (2010). "Quantile Treatment Effects in the Regression Discontinuity Design"
}
