#' @export
triton_model_runs <- function(path = "triton/model_runs"){
  cat(paste0("sbatch ", dir(path, full.names = TRUE), "\n"))
}
