#' get_npi_standard_name
#'
#' @param npi_bundle
#'
#' @return
#' @export
#'
#' @examples
get_npi_standard_name <- function(npi_bundle){

  npi_bundle %>%
    stringi::stri_split_regex(",") %>%
    modify(~npi_prov_pair$clean_name[match(.,npi_prov_pair$npi)]) %>%
    unlist() %>%
    # unique() %>%
    # sort() %>%
    paste0(collapse=", ")

}
