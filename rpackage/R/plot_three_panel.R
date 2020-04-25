#' Plot Daily number of infections
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_infections <- function(country, stan_fit, stan_data, daily_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(fit)
  prediction <- out$prediction
  estimated.deaths <- out$E_deaths
  estimated.deaths.cf <- out$E_deaths0
  
  dates <- get_dates(daily_data)[[country]]
  reported_cases <- get_reported_cases(daily_data)[[country]]
  deaths_by_country <- get_deaths(daily_data)[[country]]
  
  N <- stan_data$N[country]
  country_idx <- which(names(stan_data$N) == country)
  
  predicted_cases <- colMeans(prediction[,1:N,country_idx])
  predicted_cases_li <- colQuantiles(prediction[,1:N,country_idx], probs=.025)
  predicted_cases_ui <- colQuantiles(prediction[,1:N,country_idx], probs=.975)
  predicted_cases_li2 <- colQuantiles(prediction[,1:N,country_idx], probs=.25)
  predicted_cases_ui2 <- colQuantiles(prediction[,1:N,country_idx], probs=.75)
  
  estimated_deaths <- colMeans(estimated.deaths[,1:N,country_idx])
  estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,country_idx])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.75)
  
  data_country <- data.frame("time" = as_date(as.character(dates)),
                             "country" = rep(country, length(dates)),
                             "reported_cases" = reported_cases, 
                             "reported_cases_c" = cumsum(reported_cases), 
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = deaths_by_country,
                             "deaths_c" = cumsum(deaths_by_country),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui,
                             "death_min2" = estimated_deaths_li2,
                             "death_max2"= estimated_deaths_ui2,
                             "rt" = rt,
                             "rt_min" = rt_li,
                             "rt_max" = rt_ui,
                             "rt_min2" = rt_li2,
                             "rt_max2" = rt_ui2)
  
  data_cases_95 <- data.frame(data_country$time, data_country$predicted_min, 
                              data_country$predicted_max)
  names(data_cases_95) <- c("time", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))
  data_cases_50 <- data.frame(data_country$time, data_country$predicted_min2, 
                              data_country$predicted_max2)
  names(data_cases_50) <- c("time", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$time))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_ribbon(data = data_cases, 
                aes(x = time, ymin = cases_min, ymax = cases_max, fill = key)) +
    xlab("") +
    ylab("Daily number of infections\n") +
    scale_x_date(date_breaks = "weeks", labels = scales::date_format("%e %b")) + 
    scale_y_continuous(expand = c(0, 0)) + #, labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    ggpubr::theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  p1
}



#' Plot Daily Number of Deaths
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_deaths <- function(country, stan_fit, stan_data, daily_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(fit)
  prediction <- out$prediction
  estimated.deaths <- out$E_deaths
  estimated.deaths.cf <- out$E_deaths0
  
  dates <- get_dates(daily_data)[[country]]
  reported_cases <- get_reported_cases(daily_data)[[country]]
  deaths_by_country <- get_deaths(daily_data)[[country]]
  
  N <- stan_data$N[country]
  country_idx <- which(names(stan_data$N) == country)
  
  predicted_cases <- colMeans(prediction[,1:N,country_idx])
  predicted_cases_li <- colQuantiles(prediction[,1:N,country_idx], probs=.025)
  predicted_cases_ui <- colQuantiles(prediction[,1:N,country_idx], probs=.975)
  predicted_cases_li2 <- colQuantiles(prediction[,1:N,country_idx], probs=.25)
  predicted_cases_ui2 <- colQuantiles(prediction[,1:N,country_idx], probs=.75)
  
  estimated_deaths <- colMeans(estimated.deaths[,1:N,country_idx])
  estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,country_idx])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.75)
  
  data_country <- data.frame("time" = as_date(as.character(dates)),
                             "country" = rep(country, length(dates)),
                             "reported_cases" = reported_cases, 
                             "reported_cases_c" = cumsum(reported_cases), 
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = deaths_by_country,
                             "deaths_c" = cumsum(deaths_by_country),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui,
                             "death_min2" = estimated_deaths_li2,
                             "death_max2"= estimated_deaths_ui2,
                             "rt" = rt,
                             "rt_min" = rt_li,
                             "rt_max" = rt_ui,
                             "rt_min2" = rt_li2,
                             "rt_max2" = rt_ui2)
  
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = scales::date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0)) + #, labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    ylab("Daily number of deaths\n") + 
    xlab("") +
    ggpubr::theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  p2
}



#' Plot Daily Number of Deaths
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_deaths <- function(country, stan_fit, stan_data, daily_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(fit)
  prediction <- out$prediction
  estimated.deaths <- out$E_deaths
  estimated.deaths.cf <- out$E_deaths0
  
  dates <- get_dates(daily_data)[[country]]
  reported_cases <- get_reported_cases(daily_data)[[country]]
  deaths_by_country <- get_deaths(daily_data)[[country]]
  
  N <- stan_data$N[country]
  country_idx <- which(names(stan_data$N) == country)
  
  predicted_cases <- colMeans(prediction[,1:N,country_idx])
  predicted_cases_li <- colQuantiles(prediction[,1:N,country_idx], probs=.025)
  predicted_cases_ui <- colQuantiles(prediction[,1:N,country_idx], probs=.975)
  predicted_cases_li2 <- colQuantiles(prediction[,1:N,country_idx], probs=.25)
  predicted_cases_ui2 <- colQuantiles(prediction[,1:N,country_idx], probs=.75)
  
  estimated_deaths <- colMeans(estimated.deaths[,1:N,country_idx])
  estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,country_idx])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.75)
  
  data_country <- data.frame("time" = as_date(as.character(dates)),
                             "country" = rep(country, length(dates)),
                             "reported_cases" = reported_cases, 
                             "reported_cases_c" = cumsum(reported_cases), 
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = deaths_by_country,
                             "deaths_c" = cumsum(deaths_by_country),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui,
                             "death_min2" = estimated_deaths_li2,
                             "death_max2"= estimated_deaths_ui2,
                             "rt" = rt,
                             "rt_min" = rt_li,
                             "rt_max" = rt_ui,
                             "rt_min2" = rt_li2,
                             "rt_max2" = rt_ui2)
  
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = scales::date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0)) + #, labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    ylab("Daily number of deaths\n") + 
    xlab("") +
    ggpubr::theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  p2
}



#' Plot Daily Number of Deaths
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_deaths <- function(country, stan_fit, stan_data, daily_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(fit)
  prediction <- out$prediction
  estimated.deaths <- out$E_deaths
  estimated.deaths.cf <- out$E_deaths0
  
  dates <- get_dates(daily_data)[[country]]
  reported_cases <- get_reported_cases(daily_data)[[country]]
  deaths_by_country <- get_deaths(daily_data)[[country]]
  
  N <- stan_data$N[country]
  country_idx <- which(names(stan_data$N) == country)
  
  predicted_cases <- colMeans(prediction[,1:N,country_idx])
  predicted_cases_li <- colQuantiles(prediction[,1:N,country_idx], probs=.025)
  predicted_cases_ui <- colQuantiles(prediction[,1:N,country_idx], probs=.975)
  predicted_cases_li2 <- colQuantiles(prediction[,1:N,country_idx], probs=.25)
  predicted_cases_ui2 <- colQuantiles(prediction[,1:N,country_idx], probs=.75)
  
  estimated_deaths <- colMeans(estimated.deaths[,1:N,country_idx])
  estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,country_idx])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.75)
  
  data_country <- data.frame("time" = as_date(as.character(dates)),
                             "country" = rep(country, length(dates)),
                             "reported_cases" = reported_cases, 
                             "reported_cases_c" = cumsum(reported_cases), 
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = deaths_by_country,
                             "deaths_c" = cumsum(deaths_by_country),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui,
                             "death_min2" = estimated_deaths_li2,
                             "death_max2"= estimated_deaths_ui2,
                             "rt" = rt,
                             "rt_min" = rt_li,
                             "rt_max" = rt_ui,
                             "rt_min2" = rt_li2,
                             "rt_max2" = rt_ui2)
  
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = scales::date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0)) + #, labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    ylab("Daily number of deaths\n") + 
    xlab("") +
    ggpubr::theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  p2
}


#' Plot Daily Number of Deaths
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_Rt <- function(country, stan_fit, stan_data, daily_data, plot_dates = NULL){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  checkmate::assert_date(plot_dates, null.ok = TRUE)
  checkmate::assert_named(plot_dates)
  
  
  out <- rstan::extract(fit)
  prediction <- out$prediction
  estimated.deaths <- out$E_deaths
  estimated.deaths.cf <- out$E_deaths0
  
  dates <- get_dates(daily_data)[[country]]
  reported_cases <- get_reported_cases(daily_data)[[country]]
  deaths_by_country <- get_deaths(daily_data)[[country]]
  
  N <- stan_data$N[country]
  country_idx <- which(names(stan_data$N) == country)
  
  predicted_cases <- colMeans(prediction[,1:N,country_idx])
  predicted_cases_li <- colQuantiles(prediction[,1:N,country_idx], probs=.025)
  predicted_cases_ui <- colQuantiles(prediction[,1:N,country_idx], probs=.975)
  predicted_cases_li2 <- colQuantiles(prediction[,1:N,country_idx], probs=.25)
  predicted_cases_ui2 <- colQuantiles(prediction[,1:N,country_idx], probs=.75)
  
  estimated_deaths <- colMeans(estimated.deaths[,1:N,country_idx])
  estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.025)
  estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.975)
  estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.25)
  estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,country_idx], probs=.75)
  
  rt <- colMeans(out$Rt_adj[,1:N,country_idx])
  rt_li <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.025)
  rt_ui <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.975)
  rt_li2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.25)
  rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,country_idx],probs=.75)
  
  data_country <- data.frame("time" = as_date(as.character(dates)),
                             "country" = rep(country, length(dates)),
                             "reported_cases" = reported_cases, 
                             "reported_cases_c" = cumsum(reported_cases), 
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = deaths_by_country,
                             "deaths_c" = cumsum(deaths_by_country),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
                             "estimated_deaths" = estimated_deaths,
                             "death_min" = estimated_deaths_li,
                             "death_max"= estimated_deaths_ui,
                             "death_min2" = estimated_deaths_li2,
                             "death_max2"= estimated_deaths_ui2,
                             "rt" = rt,
                             "rt_min" = rt_li,
                             "rt_max" = rt_ui,
                             "rt_min2" = rt_li2,
                             "rt_max2" = rt_ui2)
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$time, 
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
  data_rt_50 <- data.frame(data_country$time, data_country$rt_min2, 
                           data_country$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  
  if(!is.null(plot_dates)){
    pddf <- data.frame(date = plot_dates, type = names(plot_dates))
  }
  
  p3 <- ggplot(data_country) +
    geom_stepribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                        group = key,
                                        fill = key)) +
    geom_hline(yintercept = 1, color = 'black', size = 0.1) + 
    ylim(c(0, max(data_rt$rt_max))) +
#    geom_segment(data = covariates_country_long,
#                 aes(x = value, y = 0, xend = value, yend = max(x)), 
#                 linetype = "dashed", colour = "grey", alpha = 0.75) +
#    geom_point(data = covariates_country_long, aes(x = value, 
#                                                   y = x, 
#                                                   group = key, 
#                                                   shape = key, 
#                                                   col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
#    scale_shape_manual(name = "Interventions", labels = plot_labels,
#                       values = c(21, 22, 23, 24, 25, 12)) + 
#    scale_colour_discrete(name = "Interventions", labels = plot_labels) + 
    scale_x_date(date_breaks = "weeks", labels = scales::date_format("%e %b"), 
                 limits = c(data_country$time[1], 
                            data_country$time[length(data_country$time)])) + 
#    scale_y_continuous(expand = expansion(mult=c(0,0.1))) + 
    ggpubr::theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  
  if(!is.null(plot_dates)){
    p3 <- p3 + geom_vline(data = pddf, aes(xintercept = date, lty = type))
  }
  p3
}


#' Plot threeplot
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_three_panel <- function(country, stan_fit, stan_data, daily_data, plot_dates = NULL){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  checkmate::assert_date(plot_dates, null.ok = TRUE)
  checkmate::assert_named(plot_dates)
  
  p1 <- plot_infections(country, stan_fit, stan_data, daily_data)
  p2 <- plot_deaths(country, stan_fit, stan_data, daily_data)
  p3 <- plot_Rt(country, stan_fit, stan_data, daily_data, plot_dates = NULL)
  
  p <- cowplot::plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(1, 1, 2))
  p
}

