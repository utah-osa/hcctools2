#' filter_strange_hosp_combo
#'
#' @param facility_str a string of facilities separatred by ', '
#'
#' @return the number of facilities which match the regex 'HOSPITAL'
#' @export
#'
#' @examples
filter_strange_hosp_combo <- function(facility_str){
  facility_str %>% stri_split_regex(",") %>% unlist() %>% stri_detect_regex("HOSPITAL") %>% sum()
}
