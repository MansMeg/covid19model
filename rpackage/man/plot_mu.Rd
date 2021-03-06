% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mu.R
\name{plot_mu}
\alias{plot_mu}
\alias{plot_mu_from_file}
\alias{plot_Rt}
\alias{plot_Rt_from_file}
\title{Plot covariate size effects}
\usage{
plot_mu(stan_fit, stan_data, countries = NULL)

plot_mu_from_file(file_path)

plot_Rt(country, stan_fit, stan_data, daily_data, plot_dates = NULL)

plot_Rt_from_file(file_path)
}
\arguments{
\item{stan_fit}{a stan_fit object with [alpha] parameter draws}

\item{stan_data}{a stan_data object with named design matrix array}
}
\description{
Plot covariate size effects
}
