% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_three_panel.R
\name{plot_infections}
\alias{plot_infections}
\alias{plot_deaths}
\alias{plot_Rt}
\title{Plot Daily number of infections}
\usage{
plot_infections(country, stan_fit, stan_data, daily_data)

plot_deaths(country, stan_fit, stan_data, daily_data)

plot_Rt(country, stan_fit, stan_data, daily_data, plot_dates = NULL)
}
\arguments{
\item{stan_fit}{a stan_fit object with [alpha] parameter draws}

\item{stan_data}{a stan_data object with named design matrix array}
}
\description{
Plot Daily number of infections
}
