#' count_number_provider
#'
#' @param doctor_str
#'
#' @return
#' @export
#'
#' @examples
count_number_provider <- function(doctor_str){
  doctor_str %>% stri_split_regex(",") %>% unlist() %>% length()
}
# count_doctors("CHRISTOPH WOERLEIN, JASON AHEE, JAYSON DAVID")
# count_number_provider("CHRISTOPH WOERLEIN")
