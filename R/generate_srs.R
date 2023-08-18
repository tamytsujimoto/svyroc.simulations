#' Generate simple random sample with missing
#'
#' @param pop
#' @param sample_size numeric value
#' @param coef_missing numeric vector
#'
#' @return

generate_srs <- function(pop,
                         sample_size){

  pop_size <- dim(pop)[1] # Population size
  weight <- ifelse(sample_size > pop_size, 1, pop_size/sample_size) # Computing sampling weight for SRS

  sample <-
    pop %>%
    dplyr::slice_sample(n = sample_size) %>% # sampling sample_size observations
    dplyr::mutate(weight = weight,
           pop_size = pop_size)

  return(sample)

}
