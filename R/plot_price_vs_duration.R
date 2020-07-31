#' plot_price_vs_duration
#' a ggplot object which has the tp_med vs the mean duration of the bundled procedures
#' @param df a dataframe of bundled procedures
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' cscopy %>% plot_pric_vs_duration()
plot_price_vs_duration <- function(df){
  ggplot2::ggplot(data=df,
         ggplot2::aes(x=duration_mean,y=tp_med))+
    ggplot2::geom_point(color="#f3476f", alpha=0.5)+
    ggplot2::labs(title="Median Price vs Mean Duration")+
    ggplot2::xlab("Mean Duration")+
    ggplot2::ylab("Median Price")
}
