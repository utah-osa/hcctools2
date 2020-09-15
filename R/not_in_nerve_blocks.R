#' not_in_nerve_blocks
#'
#'checks if the pc_str is in a nerve block range.
#'
#' @param pc_str
#'
#' @return
#' @export
#'
#' @examples
not_in_nerve_blocks <-  function(pc_str){
  if(is.na(pc_str)){
    F
  }else{
    if (pc_str<64400 || pc_str >64530){
      T
    } else{
      F
    }
  }


}
