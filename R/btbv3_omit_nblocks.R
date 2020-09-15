#' btbv3_omit_nblocks
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
btbv3_omit_nblocks <- function(df){
  df[,`:=`(
    cleaned_names =       pmap(.l=list(surg_pc_codes=surg_bun,
                                       surg_sp_name_clean,
                                       surg_bp_name_clean,
                                       faci_sp_name_clean,
                                       faci_bp_name_clean
    ),
    .f=create_clean_sp_bp_names_no_anes)
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
