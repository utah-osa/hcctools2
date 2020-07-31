#' get_tag_density_information
#'
#' @param df a dataframe of bundled procedures
#' @param tag a tag of interest
#'
#' @return a list of ggobjects (a grid of density distributions and boxplots), and a stats table for the splitting on that paramter.
#' @export
#'
#' @examples
#' cscopy %>% get_tag_density_information(tag="surg_bun_t_biopsy")
get_tag_density_information <- function(df, tag) {

  gg1 <- hcctools2::boxplot_by_tag(df = df, tag = tag, resp = "tp_med")
  gg2 <- hcctools2::plot_ridge_tag(df = df, tag = tag, resp = "tp_med")



  stats_table <- df %>% tibble::as_tibble() %>%
    dplyr::mutate(!!sym(tag) := dplyr::if_else(!!sym(tag) == 1, "Yes", "No")) %>%
    #select(!!sym(tag)) %>%
    dplyr::group_by(!!sym(tag)) %>%
    dplyr::summarize(
      `Mean` = mean(tp_med) %>% round(0),
      `Min` = min(tp_med) %>% round(0),
      `1Q` = tp_med %>% quantile(.25) %>% round(0),
      `Median` = median(tp_med) %>% round(0),
      `3Q` = tp_med %>% quantile(.75) %>% round(0),
      `Max` = max(tp_med) %>% round(0),
      `sd` = sd(tp_med) %>% round(0),
      `Frequency` = n(),
    ) %>%
    dplyr::arrange(dplyr::desc(!!sym(tag)))

  stats_table <- ggpubr::ggtexttable(stats_table,
                                     rows = NULL,
                                     theme = ggpubr::ttheme("classic"))

  f_str <- paste("tp_med", "~", tag)
  print(f_str)

  levene <- df %>% rstatix::levene_test(as.formula(f_str))

  levene_table <- ggpubr::ggtexttable(levene, rows = NULL,
                                      theme = ggpubr::ttheme("classic"))

  welch <- df %>% rstatix::t_test(as.formula(f_str), var.equal = FALSE)

  welch_table <- ggpubr::ggtexttable(welch, rows = NULL,
                                     theme = ggpubr::ttheme("classic"))

  dist_plots  <- ggpubr::ggarrange(
    gg1,
    gg2,
    #labels = c("A", "B"),
    nrow = 2,
    ncol = 1,
    align = "v",
    common.legend = TRUE,
    legend = "top"
  )

  stat_tables <- ggpubr::ggarrange(
    stats_table,
    levene_table,
    welch_table,
    nrow = 3,
    ncol = 1,
    labels = c("Statistics", "Levene", "Welch")
  )

  ggobjects <- list(dist_plots = dist_plots, stat_tables= stat_tables)
}
