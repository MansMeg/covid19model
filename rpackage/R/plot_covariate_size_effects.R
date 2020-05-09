#' Plot covariate size effects
#' 
#' @param stan_fit a stan_fit object with [alpha] parameter draws
#' @param stan_data a stan_data object with named design matrix array
#' @param file a [rda] file containing a [stan_data] and [stan_fit] object
#' 
#' @importFrom bayesplot vline_0
#' @export
plot_covariate_size_effects <- function(stan_fit, stan_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  
  out <- rstan::extract(stan_fit)
  stopifnot(length(dim(out$alpha)) == 2)
  alpha <- data.frame(as.matrix(out$alpha))
  colnames(alpha) = dimnames(stan_data$X)[[3]]
  plot_covariate_size_effects_from_alpha(alpha)
}

#' @rdname plot_covariate_size_effects
#' @export
plot_covariate_size_effects_country <- function(country, stan_fit, stan_data){
  checkmate::assert_class(stan_fit, "stanfit")
  checkmate::assert_true(any(grepl(names(stan_fit), pattern = "^alpha")))
  assert_stan_data(stan_data)
  checkmate::assert_choice(country, get_countries(stan_data))
  
  out <- rstan::extract(stan_fit)
  stopifnot(length(dim(out$alpha)) == 3)
  
  cidx <- which(get_countries(stan_data) == country)
  alpha <- data.frame(as.matrix(out$alpha[,,cidx]))
  colnames(alpha) = dimnames(stan_data$X)[[3]]
  xx <- plot_covariate_size_effects_from_alpha(alpha)
}

plot_covariate_size_effects_from_alpha <- function(alpha){
  checkmate::assert_data_frame(alpha)
  
  data <- suppressWarnings(
    bayesplot::mcmc_intervals_data(alpha,  
                                   prob = .95,
                                   transformation = function(x) 1-exp(-x),
                                   point_est = "mean")
  )
  
  levels(data$parameter) = gsub("t(", "", levels(data$parameter), fixed=TRUE)
  levels(data$parameter) = gsub(")", "", levels(data$parameter), fixed=TRUE)
  data$parameter = (as.character(data$parameter))
  
  no_point_est <- all(data$point_est == "none")
  x_lim <- range(c(data$ll, data$hh))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    bayesplot::vline_0(color = "gray90", size = 0.5)
  } else {
    geom_blank(
      mapping = NULL, data = NULL,
      show.legend = FALSE, inherit.aes = FALSE)
  }
  args_outer <- list(mapping = aes_(x = ~ll, xend = ~hh, y = ~parameter, 
                                    yend = ~parameter)) #, color = bayesplot::get_color("mid"))
  args_inner <- list(mapping = aes_(x = ~l, xend = ~h, y = ~parameter, 
                                    yend = ~parameter), size = 2, show.legend = FALSE)
  args_point <- list(mapping = aes_(x = ~m, y = ~parameter), 
                     data = data, size = 4, shape = 21)
  
  args_point$color <- "blue" #get_color("dark_highlight")
  
  point_func <- geom_point
  layer_outer <- do.call(geom_segment, args_outer)
  layer_inner <- do.call(geom_segment, args_inner)
  layer_point <- do.call(point_func, args_point)
  
  data$parameter = factor(as.character(data$parameter))
  # data = data[order(-data$m),]
  p = ggplot(data) +ggpubr::theme_pubr() +  geom_point(aes(x=m,y=parameter),position = position_dodge(-.5)) + 
    geom_linerange(aes(xmin=ll,xmax=hh,y=parameter),
                   position = position_dodge(-.5)) + 
    scale_x_continuous(breaks=seq(0,1,.25),labels = c("0%\n(no effect on transmissibility)",
                                                      "25%","50%","75%","100%\n(ends transmissibility)"),
                       expand=c(0.005,0.005),expression(paste("Relative % reduction in  ",R[t])))  +
    scale_colour_manual(name = "", #labels = c("50%", "95%"),
                        values = c(("coral4"), ("seagreen"))) + 
    
    geom_vline(xintercept=1,colour="darkgray") +
    scale_y_discrete("Governmental intervention\n") +
    theme(plot.margin = margin(0, 2, 0, .5, "cm"))
  
  p
}



#' @rdname plot_covariate_size_effects
#' @export
plot_covariate_size_effects_from_file <- function(file_path){
  checkmate::assert_file_exists(file_path)
  e <- new.env()
  load(file = file_path, envir =   e)
  e <- as.list(e)
  plot_covariate_size_effects(e$fit, e$stan_data)
}
