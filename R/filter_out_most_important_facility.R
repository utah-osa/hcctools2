#' filter_out_most_important_facility
#'
#' @param facility_str
#'
#' @return
#' @export
#'
#' @examples
filter_out_most_important_facility <- function(facility_str){
  facility_list <- facility_str %>% stri_split_regex(", ")%>%
    unlist() %>%
    stri_trim_both()

  facility_list %>% some( ~stri_detect_regex(.,"HOSPITAL")==T)

  hosp <- medi_center<- ""

  if (facility_list %>% some( ~stri_detect_regex(.,"HOSPITAL")==T)){
    hosp <- facility_list[facility_list %>% stri_detect_regex("HOSPITAL")] %>% .[1]
  }

  if (facility_list %>% some( ~stri_detect_regex(.,"CENTER")==T)){
    medi_center <- facility_list[facility_list %>% stri_detect_regex("CENTER")] %>% .[1]
  }


  if (hosp != ""){
    # print("hospital detected")
    single_fac <- hosp
  } else if(medi_center != ""){
    # print("center detected")
    single_fac <- medi_center
  } else{
    # print("no hospital or medical center detected")
    single_fac <- facility_list[1]
  }

  single_fac
}
