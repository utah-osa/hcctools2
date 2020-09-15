#' btbv3
#'
#' groups each bundle by a doctor facility pair based upon cleaned clean npi service and bill provider names.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
btbv3 <- function(df){
  df[,`:=`(
    cleaned_names =       pmap(.l=list(surg_sp_name_clean,
                                       surg_bp_name_clean,
                                       faci_sp_name_clean,
                                       faci_bp_name_clean
    ),
    .f=create_clean_sp_bp_names)
  )] %>%
    .[,`:=`(
      doctor_str = cleaned_names %>% map_chr(~.[[1]]),
      facility_str = cleaned_names %>% map_chr(~.[[2]])
    )] %>%
    .[,.(
      tp_med_med = median(tp_med),
      tp_cnt_cnt = sum(cnt)
    ),by=c("doctor_str","facility_str")] %>%
    .[, `:=`(
      num_doc = doctor_str %>% map_int(find_extra_doctors),
      hospital_detects = facility_str %>% map_int(filter_strange_hosp_combo)
    )] %>%
    .[num_doc < 3 & hospital_detects < 2 ]
}
