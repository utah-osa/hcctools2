#' filter_strange_hosp_combo
#'
#' @param facility_str
#'
#' @return
#' @export
#'
#' @examples
filter_strange_hosp_combo <- function(facility_str){
  facility_str %>% stri_split_regex(",") %>% unlist() %>% stri_detect_regex("HOSPITAL") %>% sum()
}
# count_doctors("CHRISTOPH WOERLEIN, JASON AHEE, JAYSON DAVID")
filter_strange_hosp_combo("IHC LAYTON HOSPITAL, IHC MKCAY DEE HOSPITAL")
