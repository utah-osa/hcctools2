#' btbv3_omit_nblocks
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
btbv3_omit_nblocks <- function(df){
  df <-  data.table::as.data.table(df)

  df[,data.table::`:=`(
    cleaned_names =       purrr::pmap(.l=list(surg_pc_codes=surg_bun,
                                       surg_sp_name_clean,
                                       surg_bp_name_clean,
                                       faci_sp_name_clean,
                                       faci_bp_name_clean
    ),
    .f=create_clean_sp_bp_names_no_anes)
  )] %>%
    data.table::.[,`:=`(
      doctor_str = cleaned_names %>% purrr::map_chr(~.[[1]]),
      facility_str = cleaned_names %>% purrr::map_chr(~.[[2]])
    )] %>%
    data.table::.[,.(
      tp_med_med = median(tp_med),
      tp_cnt_cnt = sum(cnt)
    ),by=c("doctor_str","facility_str")] %>%
    data.table::.[, data.table::`:=`(
      num_doc = doctor_str %>% purrr::map_int(find_extra_doctors),
      hospital_detects = facility_str %>% purrr::map_int(filter_strange_hosp_combo)
    )] %>%
    data.table::.[num_doc < 3 & hospital_detects < 2 ]
}
