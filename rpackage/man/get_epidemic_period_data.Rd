% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_epidemic_period_data.R
\name{get_epidemic_period_data}
\alias{get_epidemic_period_data}
\alias{get_EpidemicStart}
\title{Extract the epidemic period from data}
\usage{
get_epidemic_period_data(x, number_of_deaths, days_before_to_include_in_period)

get_EpidemicStart(x, number_of_deaths, days_before_to_include_in_period)
}
\arguments{
\item{x}{a data.frame with the number of cases and deaths per date and country}

\item{number_of_deaths}{The number of deaths to define starting of epidemic}

\item{days_before_to_include_in_period}{The number of days before the cumulative deaths to use as starting date}
}
\description{
The epidemic is said to start [days_before_to_include_in_period] days before
the country has reached [number_of_deaths] number of deaths.
}
