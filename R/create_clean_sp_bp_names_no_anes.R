#' create_clean_sp_bp_names_no_anes
#'
#' @param surg_pc_codes
#' @param surg_sp_name_clean
#' @param surg_bp_name_clean
#' @param faci_sp_name_clean
#' @param faci_bp_name_clean
#'
#' @return
#' @export
#'
#' @examples
create_clean_sp_bp_names_no_anes <- function(surg_pc_codes,
                                             surg_sp_name_clean,
                                             surg_bp_name_clean,
                                             faci_sp_name_clean,
                                             faci_bp_name_clean){
  # print(paste0("current surg_pc_codes:",surg_pc_codes))
  valid_doctor_index <-  surg_pc_codes %>%
    stri_split_regex(",") %>%
    unlist() %>%
    as.numeric() %>%
    map_lgl(not_in_nerve_blocks)


  # convert the surgery service provider npi names into a list.
  # this list will contain both the Facility and Doctor names because the data is messy.
  surg_sp_list <- surg_sp_name_clean %>%
    stri_split_regex("\\|") %>%
    unlist() %>%
    stri_trim_both()

  surg_sp_list_invalid <- surg_sp_list[!valid_doctor_index]
  surg_sp_list <- surg_sp_list[valid_doctor_index]


  #get the list index for surgeons
  doct_only_index  <- which(!(surg_sp_list %>%  stri_detect_regex(fac_ref_regex)))
  # now get the list of doctors only
  surg_sp_list_clean <- surg_sp_list[doct_only_index]

  #get the list index for the facilities
  fac_only_index <- which(surg_sp_list %>%  stri_detect_regex(fac_ref_regex))
  # now get the list of facilities in the surgery field if any.
  faci_in_surg_sp_list <- surg_sp_list[fac_only_index]



  # Now from the surgery bill provider field, we want to get the facilities.
  surg_bp_list <- surg_bp_name_clean %>%
    stri_split_regex("\\|") %>%
    unlist() %>%
    stri_trim_both()

  # Now from the facility fields we want to get the facilities too
  faci_sp_list <- faci_sp_name_clean %>%
    stri_split_regex("\\|") %>%
    unlist() %>%
    stri_trim_both()

  faci_bp_list <- faci_bp_name_clean %>%
    stri_split_regex("\\|") %>%
    unlist() %>%
    stri_trim_both()

  # combine the above three lists
  bp_master <-  append(surg_bp_list, faci_sp_list) %>% append(faci_bp_list)

  # now append the facilities found in the usrgery field into one list
  bp_master_with_faci_in_surg <- bp_master %>% append(faci_in_surg_sp_list) %>% unique() %>% sort()

  # now we have a large list of facilities, but they may have non facility names. So we need to send those back to the surgery list.

  bp_master_faci_only_index <- which(bp_master_with_faci_in_surg %>%  stri_detect_regex(fac_ref_regex))
  bp_master_clean <- bp_master_with_faci_in_surg[bp_master_faci_only_index]

  bp_master_surg_in_faci <-  which(!(bp_master_with_faci_in_surg %>%  stri_detect_regex(fac_ref_regex)))
  bp_doct_in_faci <- bp_master_with_faci_in_surg[bp_master_surg_in_faci]

  # check that the name is not an anesthesilologist.
  bp_doct_in_faci <- bp_doct_in_faci %>% discard(. %in% surg_sp_list_invalid)

  # now we want to remove any doctor's from this list so we will filter them out.
  doctor_final_list<- surg_sp_list_clean %>% append(bp_doct_in_faci) %>% unique() %>% sort()

  # faci_list_clean <-  setdiff(bp_master_with_faci_in_surg,surg_sp_list_clean)

  # now return the doctor list and the facility list cleaned, and sorted


  doctor_str <- doctor_final_list %>% paste0(collapse=", ")%>% stri_replace_all_regex("(NA)|(NA,)","") %>% stri_replace_last_regex("(, $)|(,$)|(^, )","")
  facility_str <- bp_master_clean %>% paste0(collapse=", ") %>% stri_replace_all_regex("(NA)|(NA,)","") %>% stri_replace_last_regex("(, $)|(,$)|(^, )","")

  if (doctor_str=="") doctor_str <- "NOT SPECIFIED"
  if (facility_str=="") facility_str <- "NOT SPECIFIED"

  return(list(
    doctor_str,
    facility_str
  ))

}
