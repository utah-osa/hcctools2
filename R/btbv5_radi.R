#' btbv5_radi
#'
#' @param df
#' @param osa_group_p
#' @param osa_class_p
#' @param osa_specialization_p
#'
#' @return
#' @export
#'
#' @examples
btbv5_radi <- function(df,
                       osa_group_p,
                       osa_class_p,
                       osa_specialization_p){
  df[,`:=`(
    cleaned_names =       pmap(.l=list(radi_sp_name_clean,
                                       radi_bp_name_clean,
                                       faci_sp_name_clean,
                                       faci_bp_name_clean,
                                       radi_sp_npi,
                                       radi_bp_npi,
                                       faci_sp_npi,
                                       faci_bp_npi
    ),
    .f=create_clean_sp_bp_names)
  )] %>%
    .[,`:=`(
      doctor_str = cleaned_names %>% map_chr(~.[[1]]),
      facility_str = cleaned_names %>% map_chr(~.[[2]]),
      doctor_npi_str = cleaned_names %>% map_chr(~.[[3]]),
      facility_npi_str = cleaned_names %>% map_chr(~.[[4]])

    )] %>%


    # create primary doctor and facility

    .[, `:=`(
      num_doc = doctor_str %>% map_int(count_number_provider),
      num_fac = facility_str %>% map_int(count_number_provider),
      hospital_detects = facility_str %>% map_int(filter_strange_hosp_combo)
    )] %>%
    .[num_doc < 3 & hospital_detects < 2 ] %>%
    .[,most_important_fac := facility_str%>% map_chr(filter_out_most_important_facility)] %>%
    .[,most_important_fac_npi := pmap_chr(.l=list(most_important_fac, facility_str, facility_npi_str),
                                          .f=get_most_important_facility_npi)] %>%
    .[most_important_fac!="UNEVEN NPI-NAME SET"] %>%
    .[,most_important_fac_npi:= fifelse(most_important_fac_npi=="NOT SPECIFIED",
                                        "0",
                                        most_important_fac_npi) %>% as.numeric()
      ]%>%
    .[, c("doctor_npi_str1" , "doctor_npi_str2") := tstrsplit(doctor_npi_str, ", ", fixed=T)]%>%
    .[, c("doctor_str1" , "doctor_str2") := tstrsplit(doctor_str, ", ", fixed=T)] %>%
    .[,jw :=jarowinkler(doctor_str1, doctor_str2)] %>%
    .[,`:=`(doctor_str2 = ifelse(jw >.9,
                                 NA,
                                 doctor_str2),
            doctor_npi_str2 = ifelse(jw >.9,
                                     NA,
                                     doctor_npi_str2)
    ) ] %>%
    .[,`:=`(
      doctor_npi_str1 = fifelse(doctor_npi_str1=="NOT SPECIFIED",
                                "0",
                                doctor_npi_str1) %>% as.numeric(),
      doctor_npi_str2 = fifelse(doctor_npi_str2=="NOT SPECIFIED",
                                "0",
                                doctor_npi_str2) %>% as.numeric()
    )] %>%
    #Fix the issue where there were facilities with the same name, but multiple NPIs.
    as.data.frame() %>%
    as.data.table() %>%
    .[,doctor_str2 := ifelse(doctor_str2=="NA",
                             NA,
                             doctor_str2)] %>%
    .[,`:=`(doctor_str1 = doctor_str1 %>% stri_trim_both(),
            doctor_str2 = doctor_str2 %>% stri_trim_both(),
            most_important_fac = most_important_fac %>% stri_trim_both(),
            doctor_npi_str1 = doctor_npi_str1 %>% stri_trim_both(),
            doctor_npi_str2 = doctor_npi_str2 %>% stri_trim_both(),
            most_important_fac_npi = most_important_fac_npi %>% stri_trim_both())] %>%


    .[,
      primary_doctor := pmap(.l=list(doctor_npi1=doctor_npi_str1,
                                     doctor_npi2=doctor_npi_str2,
                                     group_reqs= osa_group_p,
                                     class_reqs=osa_class_p,
                                     specialization_reqs = osa_specialization_p
      ),
      .f=calculate_primary_doctor) %>% as.character()
      ] %>%
    #Filter out any procedures where our doctors fail both criteria.
    .[!(primary_doctor %in% c("BOTH_DOC_FAIL_CRIT", "TWO_FIT_ALL_SPECS"))] %>%
    .[,primary_doctor_npi := fifelse(primary_doctor==doctor_str1,
                                     doctor_npi_str1,
                                     doctor_npi_str2)] %>%


    .[,.(
      primary_doctor_npi = max(primary_doctor_npi),
      most_important_fac_npi = max(most_important_fac_npi),
      tp_med_surg = median(surg_bun_sum_med),
      tp_med_medi = median(medi_bun_sum_med),
      tp_med_radi = median(radi_bun_sum_med),
      tp_med_path = median(path_bun_sum_med),
      tp_med_anes = median(anes_bun_sum_med),
      tp_med_faci = median(faci_bun_sum_med),
      proc_code_str_sorted = paste0(proc_code_str_sorted, collapse="|"),
      tp_med_med = median(tp_med),
      tp_cnt_cnt = sum(cnt)
    ),by=c("primary_doctor", "most_important_fac")]

}
