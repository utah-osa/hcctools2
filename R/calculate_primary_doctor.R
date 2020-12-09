#' calculate_primary_doctor
#'
#' @param doctor_npi1
#' @param doctor_npi2
#' @param group_reqs
#' @param class_reqs
#' @param specialization_reqs
#'
#' @return
#' @export
#'
#' @examples
calculate_primary_doctor <- function(doctor_npi1,
                                     doctor_npi2,
                                     group_reqs=NA,
                                     class_reqs=NA,
                                     specialization_reqs=NA){

  if(doctor_npi1==0){
    return("NOT SPECIFIED")
  }

  if(is.na(doctor_npi2)){
    return(npi_full[npi==doctor_npi1] %>% .[["clean_name"]])
  }

  class_reqs <- class_reqs %>% stri_split_regex("\\|\\|\\|") %>% unlist()
  specialization_reqs <- specialization_reqs %>% stri_split_regex("\\|\\|\\|") %>% unlist()

  npi_subset <- npi_full[npi %in% c(doctor_npi1, doctor_npi2)]

  # print(npi_subset)
  # #
  # print(group_reqs)
  # #
  if(!is.na(group_reqs)){
    npi_subset <- npi_subset[osa_group %in% group_reqs]
  }
  # print(npi_subset)
  # #
  # print(class_reqs)

  if(!is.na(class_reqs)){
    npi_subset <- npi_subset[osa_class %in% class_reqs]
  }


  # print(specialization_reqs)
  #
  #  print(npi_subset)
  #
  # npi_subset
  #
  primary_provider <- npi_subset[["clean_name"]]


  if(length(primary_provider) >1){
    print("multiple meet class req")

    if(!is.na(specialization_reqs)){
      npi_subset <- npi_subset[stri_trim_both(osa_specialization)  %in% specialization_reqs]
    }

    primary_provider2 <- npi_subset[["clean_name"]]
    print(primary_provider2)
    if(is.null(primary_provider2) || length(primary_provider2)==0){
      return("NONE_FIT_SPEC_REQ")
    }else{
      if(length(primary_provider2)==1){
        return(primary_provider2)
      }else{
        return("TWO_FIT_ALL_SPECS")
      }
    }





  }else if (length(primary_provider) ==1){
    return (primary_provider)
  }else{
    return("BOTH_DOC_FAIL_CRIT")
  }





  # if(is.null(primary_provider)){
  #   "NO_PRI_PROV"
  # }else{
  #   primary_provider
  # }

}

# calculate_primary_doctor(doctor_npi1="1437348943",
#                                   doctor_npi2="1306945308",
#                                   class_reqs="Internal Medicine",
#                                   specialization_reqs="Gastroenterology"
#                                   ) %>% print()
# #
# calculate_primary_doctor(doctor_npi1="1437348943",
#                                   doctor_npi2="1457493967",
#                                   class_reqs="Internal Medicine",
#                                   specialization_reqs="Gastroenterology"
#                                   ) %>% print()
#
# calculate_primary_doctor(doctor_npi1="1275514499",
#                                   doctor_npi2="1649374216",
#                                   class_reqs="Internal Medicine",
#                                   specialization_reqs="Gastroenterology"
#                                   ) %>% print()
#
# calculate_primary_doctor(doctor_npi1="1215969837",
#                                   doctor_npi2="1174539449",
#                                   class_reqs="Internal Medicine",
#                                   specialization_reqs="Gastroenterology"
#                                   ) %>% print()
#
# calculate_primary_doctor(doctor_npi1="1073653036",
#                                   doctor_npi2="1225473671",
#                                   class_reqs="Internal Medicine",
#                                   specialization_reqs="Gastroenterology"
#                                   ) %>% print()
