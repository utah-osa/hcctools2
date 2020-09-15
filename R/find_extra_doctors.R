#' find_extra_doctors
#'
#' @param doctor_str a string of cleaned doctor names
#'
#' @return the number of doctors in the string
#' @export
#'
#' @examples
find_extra_doctors <- function(doctor_str){
  doctor_str %>% stri_split_regex(",") %>% unlist() %>% length()
}
