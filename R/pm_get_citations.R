#' Lists datasources to cite when using data from the pmird database
#'
#' `pm_get_citations()` takes values from column `id_measurement` in table
#' `measurements` from the pmird database and collects bibliographic
#' information on the data sources which have to be cited when the respective
#' data are used. The collected bibliographic information are exported to a
#' bibtex file.
#'
#' @inheritParams pm_get_dm
#'
#' @param x A numeric vector with values from `id_measurement` from table
#' `measurements` in the 'pmird' database.
#'
#' @param file A character value defining the path to the bibtex file where to
#' write the collected bibliogrpahic information. Can be set to `NULL` to omit
#' exporting anything to a file.
#'
#' @return A data frame as created by `bib2df::bib2df()` with two additional
#' columns: `id_dataset` (the ID for the dataset) and `id_measurement` (unique
#' values of `x` identifying the measurement corresponding to the respective
#' dataset). As a side effect, unique values of column `bib` will be written
#' to the file specified in `file` if `! is.null(file)`.
#'
#' @export
pm_get_citations <- function(con, x, file = NULL) {

  if(!requireNamespace(package = "bib2df")) {
    stop("Package 'bib2df' required. Install this package first.")
  }

  # avoid package check message
  datasets <- samples <- data_to_samples <- NULL

  stopifnot(x > 0 && x %% 1 == 0)
  stopifnot((is.character(file) && length(file) == 1) || is.null(file))

  # get dm object
  dm_pmird <- pm_get_dm(con = con, learn_keys = TRUE)

  # check that x is present in data
  pmird_id_measurement <-
    pm_get_table(x = dm_pmird, .table_name = "data") %>%
    dplyr::pull(.data$id_measurement)

  if(! all(x %in% pmird_id_measurement)) {
    rlang::abort("Not all values in `x` are contained in column `id_measurement` in table `data` in the 'pmird' database.")
  }

  # collect citations
  res <-
    dm_pmird %>%
    dm::dm_zoom_to(datasets) %>%
    dm::left_join(samples, by = "id_dataset") %>%
    dm::left_join(data_to_samples, by = "id_sample") %>%
    dm::select(.data$id_dataset, .data$id_measurement, .data$reference_publication) %>%
    dm::pull_tbl() %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$id_measurement %in% !!x) %>%
    dplyr::filter(! duplicated(.data$id_measurement))

  # format the bibliography information
  res <-
    res%>%
    dplyr::mutate(
      reference_publication =
        purrr::map(.data$reference_publication, function(.x) {
          res_filename <- tempfile(pattern = "pmird_extracted_citations", fileext = "bib")
          writeLines(text = .x, con = res_filename)
          bib2df::bib2df(file = res_filename) %>%
            dplyr::mutate(
              dplyr::across(dplyr::any_of(c("AUTHOR", "EDITOR")), function(.x) purrr::map(.x, ~.x)), #---note: make sure the field is always a list, otherwise unnesting returns an error
              dplyr::across(dplyr::any_of("YEAR"), function(.x) as.numeric(.x))
            )
        })
    ) %>%
    tidyr::unnest(.data$reference_publication)

  # export to file
  if(!is.null(file)) {
    res %>%
      dplyr::select(-.data$id_dataset, -.data$id_measurement) %>%
      bib2df::df2bib(file = file)
  }

  res

}
