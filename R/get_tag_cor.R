#' get_tag_cor
#'
#' @param df
#'
#' @return tibble of correlation values SORTed BY absolute value
#' @export
#'
#' @examples
#' cscopy %>% get_tag_cor %>% View()
#'
get_tag_cor <- function(df) {
  cor_data <- df %>%
    dplyr::select_if( ~ is.numeric(.) == TRUE | is.logical(.) == TRUE)

  temp <- stats::cor(cor_data)

  temp_row1 <- temp["tp_med",] %>%
    tibble::enframe() %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::filter(stringr::str_detect(name, "^amt_bill") == FALSE) %>%
    dplyr::filter((!stringr::str_detect(name, "bun_med")) &
                    (!stringr::str_detect(name, "bun_min")) &
                    (!stringr::str_detect(name, "bun_avg")) &
                    (!stringr::str_detect(name, "bun_max")) &
                    (!stringr::str_detect(name, "bun_sum")) &
                    (name!="cnt" & name!= "tp_med") ) %>%
    dplyr::rename("correlation" = "value") %>%
    dplyr::mutate(correlation = round(correlation, 4))
}




