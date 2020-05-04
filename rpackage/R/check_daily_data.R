#' Check Daily Data set
#' @param x a daily_data data.frame to check for inconsistencies
#' @export
check_daily_data <- function(x){
  assert_daily_data(x)
  if(!checkmate::test_data_frame(x, any.missing = FALSE)) {
    message("Missing values:\n")
    for(j in seq_along(colnames(x))){
      if(any(is.na(x[[j]]))){
        message("'", paste0(names(x)[j], "' ", tolower(checkmate::check_vector(x[[j]], any.missing = FALSE))))
      }
    }
  }

  check_sequential_jumps_in_daily_data(x)
  check_decrease_before_april(x)
}



check_sequential_jumps_in_daily_data <- function(x){
  countries <- unique(x$country)
  for(i in seq_along(countries)){
    tmp <- x[x$country == countries[i],]
    for(j in seq_along(colnames(x))){
      if(is.factor(tmp[[j]])) {
        int <- as.integer(tmp[[j]])
        diff <- int[-1] - int[-length(int)]
        idx <- which(abs(diff) > 0)
        for (k in seq_along(idx)){
          sum5days <- sum(diff[idx[k]:(idx[k] + 5)], na.rm = TRUE)
          if(abs(sum5days) == 0) {
            message(paste0(countries[i], " has a jump in variable '", colnames(x)[j], "' at ", tmp$date[idx[k]]), ".")
          }
        }
      }
    }
  }
}

check_decrease_before_april <- function(x){
  countries <- unique(x$country)

  for(i in seq_along(countries)){
    tmp <- x[x$country == countries[i],]
    before_april <- tmp$date < as.Date("2020-04-01")
    for(j in seq_along(colnames(x))){
      if(colnames(x)[j] == "H3") next
      if(is.factor(tmp[[j]])) {
        int <- as.integer(tmp[[j]])[before_april]
        diff <- int[-1] - int[-length(int)]
        idx <- which(diff < 0)
        if(length(idx) > 0) {
          message(paste0(countries[i], " has a decrease in variable '", colnames(x)[j], "' at ", tmp$date[idx]), ".")
        }
      }
    }
  }
}
