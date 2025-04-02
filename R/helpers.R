#### Extracting tables from the database ####

#' Function to extract a table from the pmird database
#'
#' @param x A \code{dm} object as created using \code{\link{pm_get_dm}}.
#' @param .table_name A character value with the name of the table in the pmird
#' database from which to append new rows to a csv file.
#' @return A \code{tibble} representing the extracted table.
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
#' # get dm object
#' dm_pmird <-
#'    pmird::pm_get_dm(con, learn_keys = TRUE)
#'
#' # get table datasets
#' datasets <-
#'    pmird::pm_get_table(.table_name = "datasets")
#'
#' RMariaDB::dbDisconnect(con)
#' }
#' @export
pm_get_table <- function(x, .table_name) {

  utils::data("db_template_tables", envir = environment())
  stopifnot(inherits(x, "dm"))
  stopifnot(is.character(.table_name) && length(.table_name) == 1 && .table_name %in% names(db_template_tables))

  x %>%
    dm::dm_zoom_to(!!.table_name) %>%
    dm::pull_tbl() %>%
    tibble::as_tibble()

}

#### Adding units and measurement errors to a table extracted from the pmird database ####

#' Adds units and measurement errors to a table extracted from the pmird database
#'
#' @param x A data frame with attributes which were extracted from the pmird
#' database.
#'
#' @return \code{x} with added measurement units and uncertainties.
#'
#' @export
pm_add_quantities <- function(x) {

  # checks
  if (! requireNamespace("quantities", quietly = TRUE, versionCheck = "0.1.5"))
    stop("You need the `quantities` package (>= 0.1.5) to use this function. Install that first.")
  stopifnot(is.data.frame(x))

  # get all attribute names
  utils::data("attributes", envir = environment())
  y <- attributes
  x_attribute_names <- colnames(x)

  # get information on units
  y_quantities <-
    y %>%
    dplyr::filter(.data$attribute_name %in% x_attribute_names) %>%
    dplyr::filter(!is.na(.data$unit) & .data$unit != "dimensionless") %>%
    dplyr::filter(!stringr::str_detect(.data$attribute_name, "_err$"))

  # add units and measurement errors
  x %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(y_quantities$attribute_name), function(.x) {
        .x_error_column_name <- paste0(dplyr::cur_column(), "_err")
        has_errors <- .x_error_column_name %in% colnames(x)
        if(has_errors) {
          .x_errors <-
            x %>%
            dplyr::pull(.x_error_column_name)
        } else {
          .x_errors <- NA_real_
        }
        quantities::set_quantities(.x, unit = y_quantities$unit[y_quantities$attribute_name == dplyr::cur_column()], errors = .x_errors, mode = "standard")
      })
    )

}

#### Appending new rows from  a database table to csv files ####

#' \code{pm_append_rows_csv_ff} is a helper function to create functions to
#' append new entries in a pmird database table to a csv file with similar
#' columns.
#'
#' @param .target_cols A character vector of column names in the pmird database
#' table from which to append new rows to a csv file.
#' @param .table_name A character value with the name of the table in the pmird
#' database from which to append new rows to a csv file.
#' @return A function that appends new entries in the table \code{.table_name}
#' from the pmird database to a csv file. New entries are detected based on
#' the values in \code{.target_cols} in the database table and the csv file.
#' @keywords Internal
#' @noRd
pm_append_rows_csv_ff <- function(.table_name, .target_cols) {

  # checks
  utils::data("db_template_tables", envir = environment())
  force(.target_cols)
  force(.table_name)
  stopifnot(is.character(.table_name) && length(.table_name) == 1 && .table_name %in% names(db_template_tables))
  stopifnot(is.character(.target_cols) && all(.target_cols %in% colnames(db_template_tables[[.table_name]])))

  function(con, file) {

    on.exit(expr = FALSE)

    # checks
    stopifnot(inherits(con, "MariaDBConnection"))
    stopifnot(length(file) == 1 && is.character(file) && file.exists(file))

    x_csv <- utils::read.csv(file, header = TRUE, sep = ",")
    stopifnot(all(.target_cols %in% colnames(x_csv)))
    x_csv_template <- db_template_tables[[.table_name]]
    x_csv[, seq_len(ncol(x_csv_template))] <-
      purrr::map(seq_len(ncol(x_csv_template)), function(i) {
        class(x_csv[[i]]) <- class(x_csv_template[[i]])
        x_csv[[i]]
      })

    x_db <-
      RMariaDB::dbGetQuery(con, paste0("SELECT * FROM ", .table_name)) %>%
      dplyr::select(dplyr::any_of(.target_cols))

    dplyr::right_join(x_csv, x_db, by = .target_cols) %>%
      utils::write.csv(file = file, row.names = FALSE)

    TRUE

  }

}

#' Appending new entries in a table to a csv file with similar columns
#'
#' Helper function to append new entries in a pmird database table (indicated in
#' the function name) to a csv file with similar columns.
#'
#' @param con A connection to the pmird database.
#' @param file A character value with the path to the csv file to which to
#' append rows.
#' @return \code{TRUE} if appending rows was successful, otherwise \code{FALSE}.
#' @keywords Internal
#' @name pm_append_rows_csv
NULL

#' @rdname pm_append_rows_csv
pm_append_rows_csv_persons <-
  pm_append_rows_csv_ff(
    .table_name = "persons",
    .target_cols = c("sur_name", "given_name")
  )

#' @rdname pm_append_rows_csv
pm_append_rows_csv_attributes <-
  pm_append_rows_csv_ff(
    .table_name = "attributes",
    .target_cols = c("attribute_name")
  )

