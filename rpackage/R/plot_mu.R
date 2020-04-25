#' Plot covariate size effects
#' 
#' @inheritParams plot_covariate_size_effects
#' 
#' @export
plot_mu <- function(stan_fit, stan_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(stan_fit)
  mu <- (as.matrix(out$mu))
  colnames(mu) <-  dimnames(stan_data$X)[[1]]
  g <- bayesplot::mcmc_intervals(mu,prob = .9)
  g
}


#' @rdname plot_mu
#' @export
plot_mu_from_file <- function(file_path){
  checkmate::assert_file_exists(file_path)
  e <- new.env()
  load(file = file_path, envir =   e)
  e <- as.list(e)
  plot_mu(e$fit, e$stan_data)
}


#' @rdname plot_mu
#' @export
plot_Rt <- function(stan_fit, stan_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  countries <- dimnames(stan_data$X)[[1]]
  tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
  Rt_adj = do.call(cbind,tmp)
  colnames(Rt_adj) = countries
  g <- mcmc_intervals(Rt_adj,prob = .9)
  g
}

#' @rdname plot_mu
#' @export
plot_Rt_from_file <- function(file_path){
  checkmate::assert_file_exists(file_path)
  e <- new.env()
  load(file = file_path, envir =   e)
  e <- as.list(e)
  plot_mu(e$fit, e$stan_data)
}