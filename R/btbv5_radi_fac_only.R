#' btbv5_radi_fac_only
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
btbv5_radi_fac_only <- function(df,
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
    .[,.(
      # primary_doctor_npi = max(primary_doctor_npi),
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
    ),by=c("most_important_fac")]

}
