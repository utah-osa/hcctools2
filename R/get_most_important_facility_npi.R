get_most_important_facility_npi <- function(most_important_fac, facility_str, facility_npi_str){
  facility_str_list <- facility_str %>% stri_split_regex(", ") %>% unlist()
  facility_str_npi_list <- facility_npi_str %>% stri_split_regex(", ") %>% unlist()

  most_important_index  <- which(facility_str_list == most_important_fac)
  surg_sp_list_clean <- facility_str_npi_list[most_important_index]
}
