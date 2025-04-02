#' Provides tables with updated IDs for new (meta)data to add to the pmird database.
#'
#' \code{pm_update_import} is a helper function to import new metadata to
#' include into the pmird database from a csv file and update its IDs.
#'
#' @param con A connection to the pmird database. This should be an object of
#' class \code{\link[RMariaDB:MariaDBConnection]{MariaDBConnection}}, created
#' using \code{\link[RMariaDB:dbConnect]{dbConnect}}.
#' @param x A data frame containing (meta)data to add to the pmird database.
#' This should be in the format as the target table in the pmird database is
#' (same column names, no additional columns), but the IDs should be the same
#' as in the csv file from which \code{x} was imported. These requirements are
#' not checked by the function.
#' @param table_name A character value representing a table name in the pmird
#' database for which the csv file to import represents an addition.
#' @keywords Internal
#' @noRd
#' @return A list with two data frames:
#' \describe{
#'   \item{\code{to_add}}{A data frame with all new entities in the imported
#'   csv file to add to the pmird database, but with updated ID (primary key).}
#'   \item{\code{required}}{A data frame with all entities in the imported
#'   csv file, but with updated ID (primary key). This means that
#'   \code{required} contains both entities already present in the pmird
#'   database and new entities.}
#' }
pm_update_import <- function(x, con, table_name, id_last) {

  # checks
  stopifnot(is.data.frame(x))
  stopifnot(inherits(con, "MariaDBConnection"))
  stopifnot(is.character(table_name) && length(table_name) == 1)
  stopifnot((is.integer(id_last) && length(id_last) == 1 && id_last >= 0L)|is.null(id_last))

  # get entities already present in the pmird database
  x_present <- pm_get_present(con = con, table_name = table_name)

  # add a column with the same identifier as in x_present to x
  x_input_present <-
    pm_get_present(con = x, table_name = table_name)
  x <-
    dplyr::left_join(
      x,
      x_input_present,
      by = colnames(x_input_present)[[1]]
    )

  # identify entries in x also present in x_present
  x <-
    x %>%
    dplyr::mutate(
      x_is_present = .data$x_present %in% !!x_present$x_present
    ) %>%
    dplyr::arrange(.data$x_is_present)

  # copy id to restore row ordering at the end
  x$id_or <- x[, 1, drop = TRUE]

  # update IDs (note sort the x according to x_is_present to avoid that IDs for new entities are invalidated later)
  x <-
    pm_update_id_in_import(x = x, table_name = table_name, id_last = id_last, x_present = x_present)

  list(
    to_add =
      x %>%
      dplyr::filter(! .data$x_is_present),
    required =
      x
  ) %>%
    purrr::map(dplyr::arrange, .data$id_or) %>%
    purrr::map(dplyr::select, -.data$x_present, -.data$x_is_present, -.data$id_or, -.data$id_present)

}


#' Provides a unique ID for entities already present in the pmird database.
#'
#' \code{pm_get_present} is a helper function to define a unique ID for entities
#' already present in a table in the pmird database. This function is used by
#' \code{\link[pm_update_import]{pm_update_import}}.
#'
#' @param con Either a database connection (see
#' \code{\link[pm_update_import]{pm_update_import}}) or a data frame wit the
#' same format as the table indicated by \code{table_name} in the pmird database
#' (same column names, no additional columns).
#' @param table_name See \code{\link[pm_update_import]{pm_update_import}}.
#' @keywords Internal
#' @noRd
#' @return A data frame with the same rows as the table \code{table_name} in the
#' pmird database (if \code{con} is a database connection) or in the provided
#' data frame (id \code{x} is a data frame) and with the folowing two columns:
#' \enumerate{
#'  \item{The ID (primary key).}
#'  \item{A column \code{x_present} with the unique ID which can be used to
#'  match entities across data frames.}
#' }
pm_get_present <- function(con, table_name) {

  if(!requireNamespace(package = "RMariaDB", version = "1.0.8")) {
    stop("Package 'RMariaDB' (>= 1.0.8) required. Install this package first.")
  }

  if(inherits(con, "MariaDBConnection")) {
    # get entities present in the pmird database # ---todo: I guess this is no good practice to prevent injections?
    query <-
      RMariaDB::dbQuoteLiteral(con, x = RMariaDB::dbQuoteLiteral(con, x = paste0("SELECT * FROM ", table_name))) %>%
      as.character() %>%
      stringr::str_remove_all("'")

    x_present <-
      RMariaDB::dbGetQuery(con, query)
  } else if(is.data.frame(con)) {
    x_present <- con
  }


  switch(
    table_name,
    "attributes" = {
      x_present %>%
        dplyr::mutate(
          x_present = .data$attribute_name
        ) %>%
        dplyr::select(dplyr::all_of(c("id_attribute", "x_present")))
    },
    "mactyps_to_taxclas" = {
      x_present %>%
        dplyr::mutate(
          x_present = paste0(.data$id_macrofossil_type, "_", .data$id_taxonomic_classification)
        ) %>%
        dplyr::select(dplyr::all_of(c("id_macrofossil_type", "x_present")))
    },
    "taxonomic_coverages" = {
      if(inherits(con, "MariaDBConnection")) {
        x_present <-
          x_present %>%
          dplyr::left_join(
            RMariaDB::dbGetQuery(con, "SELECT * FROM taxclas_to_taxcovs"),
            by = "id_taxonomic_coverage"
          ) %>%
          dplyr::left_join(
            RMariaDB::dbGetQuery(con, "SELECT * FROM taxonomic_classifications"),
            by = "id_taxonomic_classification"
          )
      }

      x_present <-
        x_present %>%
        dplyr::arrange(.data$id_taxonomic_coverage, .data$id_taxonomic_classification) %>%
        dplyr::group_by(.data$id_taxonomic_coverage) %>%
        dplyr::summarise(
          x_present =
            paste0(unique(.data$general_taxonomic_coverage), "_", paste(.data$id_taxonomic_classification, collapse = "_")),
          .groups = "drop"
        ) %>%
        dplyr::select(.data$id_taxonomic_coverage, .data$x_present)
    },
    "taxonomic_classifications" = {
      x_present %>%
        dplyr::mutate(x_present = .data$taxon_rank_value) %>%
        dplyr::select(dplyr::all_of(c("id_taxonomic_classification", "x_present")))
    },
    "macrofossil_types" = {# ---note: must have additionally taxon_rank_name and taxon_rank_value in the table to modify if con is a data frame
      if(inherits(con, "MariaDBConnection")) {
        x_present <-
          x_present %>%
          dplyr::left_join(
            RMariaDB::dbGetQuery(con, "SELECT * FROM mactyps_to_taxclas"),
            by = "id_macrofossil_type"
          ) %>%
          dplyr::left_join(
            RMariaDB::dbGetQuery(con, "SELECT * FROM taxonomic_classifications"),
            by = "id_taxonomic_classification"
          )
      }

      x_present <-
        x_present %>%
        dplyr::mutate(
          x_present =
            paste0(
              .data$macrofossil_type, "_",
              .data$macrofossil_size_lower, "_",
              .data$macrofossil_size_upper, "_",
              .data$taxon_rank_name, "_",
              .data$taxon_rank_value, "_",
              .data$macrofossil_taxon_organ
            )
        ) %>%
        dplyr::select(dplyr::all_of(c("id_macrofossil_type", "x_present")))
    },
    "instruments" = {
      x_present %>%
        dplyr::mutate(x_present = .data$instrumentation) %>%
        dplyr::select(dplyr::all_of(c("id_instrument", "x_present")))
    },
    "method_steps" = {
      x_present %>%
        dplyr::mutate(x_present = .data$description) %>%
        dplyr::select(dplyr::all_of(c("id_method_step", "x_present")))
    },
    "methods" = {# ---note: must have additionally id_method_step in the table to modify if con is a data frame
      if(inherits(con, "MariaDBConnection")) {
        x_present <-
          x_present %>%
          dplyr::left_join(
            RMariaDB::dbGetQuery(con, "SELECT * FROM `methods_to_method_steps`"),
            by = "id_method"
          )
      }

      purrr::map_df(unique(x_present$id_method), function(i) {
        tibble::tibble(
          id_method = i,
          x_present =
            x_present %>%
            dplyr::filter(.data$id_method == i) %>%
            dplyr::pull(.data$id_method_step) %>%
            paste(collapse = "_")
        )
      })
    },
    "persons" = {
      x_present %>%
        dplyr::mutate(
          x_present =
            paste0(.data$given_name, "_", .data$sur_name)
        ) %>%
        dplyr::select(dplyr::all_of(c("id_person", "x_present")))
    },
    stop(paste0("Table ", table_name, " is either not present in the pmird database or has not been implemented yet."))
  )

}


#' Updates all IDs in a data frame representing (meta)data to add to a table in the pmird database.
#'
#' \code{pm_update_id_in_import} is a helper function to update the IDs in a
#' data frame representing (meta)data to add to a table in the pmird database.
#' Note that at this stage, it is not checked if an entity is already present in
#' the data base, but the ID is just increased according to the last ID present
#' in the respective database table. This function is used by
#' \code{\link[pm_update_import]{pm_update_import}}.
#'
#' @param x See \code{\link[pm_update_import]{pm_update_import}}.
#' @param table_name See \code{\link[pm_update_import]{pm_update_import}}
#' @param id_last See \code{\link[pm_update_import]{pm_update_import}}.
#' @param x_present A character vector containing unique IDs for the entities
#' already present in a table in the pmird database. This vector is created
#' internally by \code{\link[pm_update_import]{pm_update_import}}.
#' @keywords Internal
#' @noRd
#' @return \code{x} with updated IDs.
pm_update_id_in_import <- function(x, table_name, id_last, x_present) {

  switch(
    table_name,
    "attributes" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_attribute"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_attribute =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "mactyps_to_taxclas" = {
      x %>%
        dplyr::mutate(id_present = NA_integer_) # don't update ids because this is just a referencing table
    },
    "taxonomic_coverages" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_taxonomic_coverage"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_taxonomic_coverage =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              !is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "taxonomic_classifications" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_taxonomic_classification"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_taxonomic_classification =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "macrofossil_types" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_macrofossil_type"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_macrofossil_type =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "instruments" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_instrument"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_instrument =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "method_steps" = {
      x %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_method_step"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_method_step =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "methods" = {
      x %>%
        dplyr::filter(! duplicated(.data$id_method)) %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_method"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_method =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              !is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    "persons" = {
      x  %>%
        dplyr::filter(!duplicated(.data$id_person)) %>%
        dplyr::left_join(
          x_present %>%
            dplyr::rename(id_present = "id_person"),
          by = "x_present"
        ) %>%
        dplyr::mutate(
          id_person =
            dplyr::case_when(
              is.na(.data$id_present) ~ seq_len(nrow(.)) + .env$id_last,
              ! is.na(.data$id_present) ~ .data$id_present
            )
        )
    },
    stop(paste0("Table ", table_name, " is either not present in the pmird database or has not been implemented yet."))
  )

}

