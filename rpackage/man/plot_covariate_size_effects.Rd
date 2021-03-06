% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_covariate_size_effects.R
\name{plot_covariate_size_effects}
\alias{plot_covariate_size_effects}
\alias{plot_covariate_size_effects_country}
\alias{plot_covariate_size_effects_from_file}
\title{Plot covariate size effects}
\usage{
plot_covariate_size_effects(stan_fit, stan_data)

plot_covariate_size_effects_country(country, stan_fit, stan_data)

plot_covariate_size_effects_from_file(file_path)
}
\arguments{
\item{stan_fit}{a stan_fit object with [alpha] parameter draws}

\item{stan_data}{a stan_data object with named design matrix array}

\item{file}{a [rda] file containing a [stan_data] and [stan_fit] object}
}
\description{
Plot covariate size effects
}
