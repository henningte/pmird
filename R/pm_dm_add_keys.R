#' Adds primary and foreign keys to a \code{dm} object with tables from the pmird database
#'
#' \code{pm_dm_add_keys} is a helper function that takes a \code{dm} object
#' with tables from the pmird database, adds the primary and foreign keys,
#' and checks whether these are valid, using \code{\link[dm]{dm_examine_constraints}}.
#'
#' @param x A \code{\link[dm]{dm}} object with tables from pmird database.
#' @param dm_pmird A \code{\link[dm]{dm}} object representing the pmird
#' database.
#' @param progress A logical value. See \code{\link[dm]{dm_examine_constraints}}.
#' @return \code{x} with added primary and foreign keys.
#' @export
pm_dm_add_keys <- function(x, dm_pmird, progress = NA) {

  stopifnot(inherits(x, "dm"))
  stopifnot(inherits(dm_pmird, "dm"))

  # get keys
  dm_pmird_pk <- dm::dm_get_all_pks(dm = dm_pmird)
  dm_pmird_fk <- dm::dm_get_all_fks(dm = dm_pmird)

  # get tables in x
  x_tables <- names(x)

  # filter keys
  x_pk <-
    dm_pmird_pk %>%
    dplyr::filter(.data$table %in% x_tables)

  x_fk <-
    dm_pmird_fk %>%
    dplyr::filter(.data$child_table %in% x_tables & .data$parent_table %in% x_tables)

  # add keys
  for(i in seq_len(nrow(x_pk))) {
    x <-
      x %>%
      dm::dm_add_pk(!!x_pk$table[[i]], !!x_pk$pk_col[[i]])
  }
  for(i in seq_len(nrow(x_fk))) {
    x <-
      x %>%
      dm::dm_add_fk(!!x_fk$child_table[[i]], !!x_fk$child_fk_cols[[i]], !!x_fk$parent_table[[i]])
  }

  x_check <- dm::dm_examine_constraints(x, progress = progress)
  if(!all(x_check$is_key)) {
    rlang::abort("Key constraint not met!")
  }

  x

}
