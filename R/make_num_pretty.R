#' Make number pretty
#' 
#' @param x A number
#' @param r Round, number of decimal places to use. Default is 3.
#' @examples 
#' make_num_pretty(12356789)
#' make_num_pretty(12356789,5)
#' make_num_pretty(0.12356789)
#' make_num_pretty(0.12356789,5)
#' make_num_pretty(8.1e-09)
#' make_num_pretty(8.1e-09,10)
#' @export
make_num_pretty<-function(x,r=3) {
  format(round(x,r),scientific=F,big.mark=",") 
}