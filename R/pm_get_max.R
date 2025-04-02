#' Takes a \code{zoomed_dm} object or a data frame and returns the largest element of a certain column.
#'
#' \code{pm_get_max} takes a \code{zoomed_dm} object or a data frame and the
#' name of a specific column and returns the maximum value of this column in the
#' \code{zoomed_dm} object or data frame. \code{pm_get_max} is used as helper
#' function to extract the last identifier in a table in the pmird database.
#'
#' @param x A \code{zoomed_dm} object or a data frame.
#' @param .col The name of a column in \code{x} for which to extract the largest
#' number.
#' @return The maximum value in the column \code{.col} in \code{x} or 0 if
#' \code{x} has no rows.
#' @examples
#' \dontrun{
#' # connect to database
#' con <-
#'   RMariaDB::dbConnect( # ---todo: change this
#'     drv = RMariaDB::MariaDB(),
#'     dbname = "pmird",
#'     default.file = "~/my.cnf"
#'   )
#'
#' # get pmird database as dm object
#' dm_pmird <- pm_get_dm(con, learn_keys = TRUE)
#'
#' # extract the last ID from a table
#' dm::dm_zoom_to(datasets) %>%
#'   pm_get_max(.col = id_dataset)
#'
#' # when finished, disconnect
#' RMariaDB::dbDisconnect(con)
#' }
#' @export
pm_get_max <- function(x, .col) {

  stopifnot(inherits(x, "zoomed_dm") || inherits(x, "data.frame"))
  stopifnot(as.character(rlang::enexpr(.col)) %in% colnames(x))

  res <- dplyr::pull(x, rlang::enexpr(.col))
  ifelse(length(res) == 0, 0L, max(res, na.rm = TRUE))

}
