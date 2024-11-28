#' Summarise by group
#'
#' Summarise data frame variables according to group
#' @param df A data frame
#' @param grp.cols Index the columns to group by. Can index by column name or column number.
#' @param summarise.cols Index the columns that should be summarised. Can index by column name or column number.
#' @param fun The function for summarising (e.g., mean, max etc.)
#' @return A data frame
#' @import dplyr
#' @examples
#' summarise_by_group(mtcars, "cyl", c(1, 3:ncol(mtcars)), max)
#' summarise_by_group(mtcars, 2, c(1, 3:ncol(mtcars)), max)
#' summarise_by_group(mtcars, c(2, 10), c(1, 3, 5:9), max)
#' summarise_by_group(mtcars, "cyl", c("mpg", "disp"), max)
#' summarise_by_group(mtcars, "cyl", "mpg", max)
#' summarise_by_group(mtcars, c("cyl",'gear'), c("mpg", "disp"), max)
#' summarise_by_group(mtcars, "cyl", c(1,3), max)
#' @export
summarise_by_group <- function(df, grp.cols, summarise.cols, fun) {

    if (is.numeric(summarise.cols)) {
    df2<-df|>
      dplyr::group_by(dplyr::across(any_of(grp.cols)))|>
      dplyr::summarise_at(.vars = colnames(df)[summarise.cols], .funs=fun, na.rm = TRUE)
      }

    if (!is.numeric(summarise.cols)) {
    df2 <- df|>
      dplyr::group_by(dplyr::across(any_of(grp.cols)))|>
      #dplyr::summarise(dplyr::across(any_of(summarise.cols), .fns = fun, na.rm = TRUE))
      dplyr::summarise(dplyr::across(any_of(summarise.cols), \(x) .funs=fun(x, na.rm = TRUE)))
      }

    as.data.frame(df2)
  }
