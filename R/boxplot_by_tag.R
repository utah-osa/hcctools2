#' boxplot_by_tag
#'
#' @param df  a dataframe of bundled procedures
#' @param resp a price prediction, the default is tp_med
#' @param tag  a tag of interest which we want to see side by side distributions for
#'
#' @return ggplot object
#' @export
#'
#' @examples
boxplot_by_tag <- function(df, resp = "tp_med", tag) {

  #min_lim <- df %>% pull(tp_med) %>% min()
  max_lim <- df %>% dplyr::pull(tp_med) %>% max()

  tag_type <-
    stringr::str_extract(tag, "_[[:alpha:]]+$") %>%
    stringr::str_replace("_", "")

  xlabel <- paste0("Bundle has '", tag_type, "' tag?")

  ylabel <- "Median Bundle Price"

  temp_title <- paste0("Boxplot Plot: Bundle with '", tag_type, "' tag")

  df <- df %>% dplyr::group_by(!!sym(tag)) %>%
    dplyr::mutate(
      outlier = dplyr::case_when(
        tp_med > median(tp_med) + IQR(tp_med) * 1.5 ~ TRUE,
        tp_med < median(tp_med) - IQR(tp_med) * 1.5 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  ggplot2::ggplot(data = df,
                  ggplot2::aes_(
                    x = as.name(tag),
                    y = as.name("tp_med"),
                    fill = as.name(tag)
                  )) +
    ggplot2::geom_point(
      data = df %>% dplyr::filter(outlier == FALSE),
      ggplot2::aes_(color = as.name(tag)),
      alpha = .5,
      position = ggplot2::position_jitter(w = 0.1, h = 0)
    ) +
    ggplot2::geom_boxplot(data = df,
                          alpha = .7,
                          outlier.color = "#f3476f") +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::labs(title = temp_title,
                  fill = paste0("Bundle has '", tag_type, "'")) +
    # ylim(0,max_lim)+
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
    ggplot2::scale_x_discrete(labels = c("TRUE" = "Yes",
                                         "FALSE" = "No")) +
    ggplot2::scale_fill_manual(
      values = cust_color_pal2,
      labels = c("TRUE" = "Yes",
                 "FALSE" = "No"),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::scale_color_manual(values = cust_color_pal2, guide = FALSE) +
    ggplot2::ylim(0, max_lim) +
    ggplot2::coord_flip()


}
