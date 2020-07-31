#' plot_med_density
#'
#' @param df dataframe of bundled procedures
#' @param temp_title the title of the plot
#' @param temp_ylab the title of the y axis
#' @param temp_xlab the title of the x axis
#'
#' @return
#' @export
#'
#' @examples
plot_med_density <- function(df,
                             temp_title = "Density Plot",
                             temp_ylab = "Count",
                             temp_xlab = "Median Bundled Procedure Price"){

  med_price_dist <- ggplot2::ggplot(data=df,
                                    ggplot2::aes(x=tp_med))+
    ggplot2::geom_histogram(fill="#617ff7")+
    #geom_density(aes(y=..count..))+
    ggplot2::geom_vline(xintercept = median(df$tp_med))+
    ggplot2::annotate("label",
                      x=median(df$tp_med),
                      y=15,
                      color="#617ff7",
                      label=paste0(round(mean(df$tp_med),2) ))+
    ggplot2::labs(title=temp_title)+
    ggplot2::xlab(temp_xlab)+
    ggplot2::ylab(temp_ylab)

}
