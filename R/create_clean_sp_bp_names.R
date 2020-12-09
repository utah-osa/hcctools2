
#' create_clean_sp_bp_names
#'
#' @param surg_sp_name_clean  surgery service provider npi name
#' @param surg_bp_name_clean  surgery bill provider npi name
#' @param faci_sp_name_clean  facility service provider npi name
#' @param faci_bp_name_clean  facility bill provider npi name
#' @param surg_sp_npi
#' @param surg_bp_npi
#' @param faci_sp_npi
#' @param faci_bp_npi
#'
#' @return list( doctor_str,facility_str)
#' @export
#'
#' @examples
create_clean_sp_bp_names <- function(surg_sp_name_clean,
                                     surg_bp_name_clean,
                                     faci_sp_name_clean,
                                     faci_bp_name_clean,
                                     surg_sp_npi,
                                     surg_bp_npi,
                                     faci_sp_npi,
                                     faci_bp_npi){

  # create atomic lists --------------------------------------------------------

  surg_sp_list <- surg_sp_name_clean %>%
    stri_split_regex(",") %>%
    unlist() %>%
    stri_trim_both()

  surg_sp_npi_list <- surg_sp_npi %>%
    stri_split_regex(",") %>%
    unlist()


  surg_bp_list <- surg_bp_name_clean %>%
    stri_split_regex(",") %>%
    unlist() %>%
    stri_trim_both()

  surg_bp_npi_list <- surg_bp_npi %>%
    stri_split_regex(",") %>%
    unlist()

  faci_sp_list <- faci_sp_name_clean %>%
    stri_split_regex(",") %>%
    unlist() %>%
    stri_trim_both()

  faci_sp_npi_list <- faci_sp_npi %>%
    stri_split_regex(",") %>%
    unlist()

  faci_bp_list <- faci_bp_name_clean %>%
    stri_split_regex(",") %>%
    unlist() %>%
    stri_trim_both()

  faci_bp_npi_list <- faci_bp_npi %>%
    stri_split_regex(",") %>%
    unlist()


  # create provider name & npi hash -------------------------------------------

  temp1 <- surg_sp_list %>% append(surg_bp_list) %>% append(faci_sp_list) %>% append(faci_bp_list)
  temp2 <- surg_sp_npi_list %>% append(surg_bp_npi_list) %>% append(faci_sp_npi_list) %>% append(faci_bp_npi_list)

  if (length(temp1) == length(temp2)){
    npi_provider_dict <- tibble(
      p_name = temp1,
      npi = temp2
    ) %>%
      as.data.table() %>%
      unique() %>%
      .[p_name != "NA"]

    # filter out the doctor and facility names from the sp and bp string lists

    # 1. Get the doctors and facility from the surg_sp_list ----------------------

    #get the list index for surgeons
    doct_only_index  <- which(!(surg_sp_list %>%  stri_detect_regex(fac_ref_regex)))
    # now get the list of doctors only
    surg_sp_list_clean <- surg_sp_list[doct_only_index]

    #get the list index for the facilities
    fac_only_index <- which(surg_sp_list %>%  stri_detect_regex(fac_ref_regex))
    # now get the list of facilities in the surgery field if any.
    faci_in_surg_sp_list <- surg_sp_list[fac_only_index]

    #2. Get the Bill Providers and aka likely facility providers in one big list

    bp_master <-  surg_bp_list %>% append( faci_sp_list) %>% append(faci_bp_list)

    # now append the already found facilities in the surgery sp field
    bp_master <- bp_master %>%
      append(faci_in_surg_sp_list) %>%
      unique() %>%
      sort()

    # now we have a large list of facilities, but they may have non facility names. So we need to send those back to the surgery list.

    bp_master_faci_only_index <- which(bp_master %>%  stri_detect_regex(fac_ref_regex))
    bp_master_clean <- bp_master[bp_master_faci_only_index]

    bp_master_surg_in_faci <-  which(!(bp_master %>%  stri_detect_regex(fac_ref_regex)))
    bp_doct_in_faci <- bp_master[bp_master_surg_in_faci]


    # now we want to remove any doctor's from this list so we will filter them out.
    doctor_final_list<- surg_sp_list_clean %>% append(bp_doct_in_faci) %>% unique() %>% sort()

    # faci_list_clean <-  setdiff(bp_master_with_faci_in_surg,surg_sp_list_clean)

    # now return the doctor list and the facility list cleaned, and sorted


    doctor_str <- doctor_final_list %>% paste0(collapse=", ")%>%
      stri_replace_all_regex("(^NA[[:blank:]]+)|(NA,)","") %>%
      stri_replace_last_regex("(, $)|(,$)|(^, )","")%>%
      stri_trim_both() %>%
      stri_replace_all_regex("^NA$","")%>%
      stri_replace_all_regex(", NA$","")

    facility_str <- bp_master_clean %>% paste0(collapse=", ") %>%
      stri_replace_all_regex("(^NA)|(^NA[[:blank:]]+)|(NA,)","") %>%
      stri_replace_last_regex("(, $)|(,$)|(^, )","")%>%
      stri_trim_both() %>%
      stri_replace_all_regex("^NA$","")%>%
      stri_replace_all_regex(", NA$","")


    #get a npi list of the doctor and facility string

    doctor_npi_str <-npi_provider_dict$npi[match(doctor_final_list,npi_provider_dict$p_name) ] %>%
      unlist() %>%
      paste0(collapse=", ") %>%
      stri_replace_all_regex("(^NA[[:blank:]]+)|(NA,)","") %>%
      stri_replace_last_regex("(, $)|(,$)|(^, )","")%>%
      stri_trim_both() %>%
      stri_replace_all_regex("^NA$","")%>%
      stri_replace_all_regex(", NA$","")

    facility_npi_str <- npi_provider_dict$npi[match(bp_master_clean,npi_provider_dict$p_name )] %>%
      unlist() %>%
      paste0(collapse=", ") %>%
      stri_replace_all_regex("(^NA[[:blank:]]+)|(NA,)","") %>%
      stri_replace_last_regex("(, $)|(,$)|(^, )","") %>%
      stri_trim_both() %>%
      stri_replace_all_regex("^NA$","")%>%
      stri_replace_all_regex(", NA$","")

    if (doctor_str=="") doctor_str <- "NOT SPECIFIED"
    if (facility_str=="") facility_str <- "NOT SPECIFIED"
    if (doctor_npi_str=="") doctor_npi_str <- "NOT SPECIFIED"
    if (facility_npi_str=="") facility_npi_str <- "NOT SPECIFIED"

    return(list(
      doctor_str,
      facility_str,
      doctor_npi_str,
      facility_npi_str
    ))




  }else{

    return(list(
      doctor_str="UNEVEN NPI-NAME SET",
      facility_str="UNEVEN NPI-NAME SET",
      doctor_npi_str="UNEVEN NPI-NAME SET",
      facility_npi_str="UNEVEN NPI-NAME SET"
    ))
  }






  #print(npi_provider_dict)


}

#
# temp <- create_clean_sp_bp_names(surg_sp_name_clean="ALEX NIELSON, JOHN SMITH, IHC LAYTON HOSPITAL",
#                                      surg_bp_name_clean="JOHN SMITH",
#                                      faci_sp_name_clean="IHC LAYTON HOSPITAL",
#                                      faci_bp_name_clean="IHC LAYTON HOSPITAL",
#                                     surg_sp_npi="123,456,789",
#                                     surg_bp_npi="456",
#                                     faci_sp_npi="789",
#                                     faci_bp_npi="789") #%>% print()
# temp
#            faci_bp_name_clean=" NA") %>% print()
