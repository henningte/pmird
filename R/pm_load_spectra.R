#' Loads spectra from files and adds these to the data frame with the file names.
#'
#' \code{pm_load_spectra} takes a column with file names for files containing
#' mid infrared spectra from  a data frame, imports the spectra into a list
#' column \code{spectra} and adds them to the data frame which then becomes an
#' \code{\link{ir::ir_new_ir}[ir]} object.
#'
#' @details Currently, the following file formats are supported for files
#' storing mid infrared spectra:
#' \enumerate{
#'   \item csv files
#'   \item Thermo Galactic's .spc files
#' }
#'
#' @param x A data frame.
#'
#' @param directory A character value specifying the directory where the files
#' accompanying the 'pmird' database are stored.
#'
#' @param .col_files The name of the column of \code{x} with file names for the
#' files storing mid infrared spectra.
#'
#' @param .col_sample_id The name of the column of \code{x} with names for the
#' samples for which to load spectra. This column is used as column
#' \code{sample_id} when constructing the \code{ir} object.
#'
#' @return \code{x} as an object of class \code{ir} and with a new column
#' \code{spectra}.
#'
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
#' # extract data
#' x <-
#'   pm_get_dm(con, learn_keys = TRUE) %>%
#'   dm::dm_zoom_to(data) %>%
#'   dplyr::filter(id_measurement < 1000 & ! is.na(mirs_file)) %>%
#'   dm::pull_tbl() %>%
#'   as.data.frame()
#'
#' # close connection
#' RMariaDB::dbDisconnect(con)
#'
#' # get spectra
#' x <-
#'   pm_load_spectra(x, directory = "..", .col_files = "mirs_file", .col_sample_id = "id_measurement")
#'
#' class(x)
#' }
#' @export
pm_load_spectra <- function(x, directory = ".", .col_files = "mirs_file", .col_sample_id = "id_sample") {

  if(!requireNamespace(package = "ir", version = "0.2.0")) {
    stop("Package 'ir' (>= 0.2.0) required. Install this package first.")
  }

  .col_files <- rlang::ensym(.col_files)
  .col_sample_id <- rlang::ensym(.col_sample_id)

  # checks
  stopifnot(is.character(directory) && dir.exists(directory))
  stopifnot(inherits(x, "data.frame"))
  stopifnot(rlang::as_string(.col_files) %in% colnames(x))
  stopifnot(rlang::as_string(.col_sample_id) %in% colnames(x))

  mir_files <-
    tibble::tibble(
      mir_files =
        dplyr::pull(x, !!.col_files)
    ) %>%
    dplyr::mutate(
      mir_files =
        dplyr::case_when(
          is.na(.data$mir_files) ~ .data$mir_files,
          TRUE ~ .data$mir_files %>% stringr::str_remove(pattern = "^\\.{2}") %>%
            paste0(.env$directory, .)
        ),
      has_no_mirs =
        purrr::map_lgl(.data$mir_files, is.na),
      mir_file_exists =
        purrr::map_lgl(.data$mir_files, file.exists),
      mir_files_extension =
        .data$mir_files %>%
        stringr::str_extract(pattern = "\\.(csv|spc)$") %>%
        stringr::str_remove(pattern = "^\\."),
      mir_files_extension =
        dplyr::case_when(
          is.na(.data$mir_files_extension) ~ "has_no_mirs",
          TRUE ~ .data$mir_files_extension
        ),
      mir_file_order =
        seq_along(.data$mir_files)
    )

  # stop if there are missing files
  if(!all(mir_files$mir_file_exists[! mir_files$has_no_mirs])) {
    n_files_missing <- sum(! mir_files$mir_file_exists)
    index_n_files_missing <-
      if(n_files_missing > 3) {
        3
      } else {n_files_missing }
    rlang::abort(paste0(n_files_missing, " ", ifelse(index_n_files_missing == 1, "file is missing", "files are missing"), ". The first ", paste0(c("file", "two files", "three files"), " missing ", c("is", "are", "are"))[index_n_files_missing], ":\n", rlang::format_error_bullets(mir_files$mir_files[which(!mir_files$mir_file_exists)[seq_len(index_n_files_missing)]]), "\nIs `directory the correct location for the 'pmird' files?`"))
  }

  # import
  res <-
    purrr::map_dfr(unique(mir_files$mir_files_extension), function(y) {
      res <-
        mir_files %>%
        dplyr::filter(.data$mir_files_extension == y)
      spectra <-
        switch(
          y,
          "csv" = {
            ir::ir_import_csv(filenames = res$mir_files)$spectra
          },
          "spc" = {
            ir::ir_import_spc(filenames = res$mir_files)$spectra
          },
          "has_no_mirs" = {
            list(tibble::tibble(x = numeric(), y = numeric())) %>%
              rep(nrow(res))
          }
        )
      res$spectra <- spectra
      attributes(res$spectra) <- NULL # see https://github.com/r-lib/vctrs/issues/1300
      res
    }) %>%
    dplyr::arrange(.data$mir_file_order) %>%
    dplyr::mutate(
      measurement_id = seq_along(.data$mir_file_order)
    )

  names(res$spectra) <- seq_len(nrow(x))

  # define empty spectra
  wavenumbers <- ir::ir_flatten(ir::ir_as_ir(res)) %>% dplyr::pull(.data$x)
  if(length(wavenumbers) > 0) {
    res <-
      res %>%
      dplyr::mutate(
        spectra =
          purrr::map2(.data$spectra, .data$has_no_mirs, function(.x, .y) {
            if(.y) {
              tibble::tibble(
                x = wavenumbers,
                y = NA_real_
              )
            } else {
              .x
            }
          })
      )
  }

  # add data to x
  x_sample_id_or <- as.character(dplyr::pull(x, !!.col_sample_id))
  ir::ir_new_ir(
    spectra = res$spectra,
    metadata =
      x %>%
      dplyr::mutate(
        sample_id = !!.col_sample_id,
        measurement_id = seq_along(.data$sample_id)
      )
  ) %>%
    dplyr::filter(.data$sample_id %in% .env$x_sample_id_or)

}
