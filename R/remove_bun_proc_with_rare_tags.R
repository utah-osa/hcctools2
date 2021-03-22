#' remove_bun_proc_with_rare_tags
#'
#' @param outlier_tag_list
#' @param df
#'
#' @return
#' @export
#'
#' @examples
remove_bun_proc_with_rare_tags <- function(outlier_tag_list, df){

  print("Start of rare tag removal")
  print(paste0("nrow:",nrow(df), " , ncol:", ncol(df)))
  print(paste0("nrow:",nrow(df)))

  for (outlier_tag in outlier_tag_list){
    print(paste0("removing: ", outlier_tag))

    var_enq <- rlang::sym(outlier_tag)
    df <-  df %>% filter(!!var_enq != 1)
    df <-  df %>% select(-!!var_enq )
    print(nrow(df))
  }
  df <- df %>% as.data.table()
  return (df)



}
