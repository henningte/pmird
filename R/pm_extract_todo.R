#' Helper function to extract the todo lists from scripts to enter data into the pmird database
#'
#' \code{pm_extract_todo} is an internal helper function to extract todo lists
#' from Rmarkdown scripts used to enter data into the pmird database.
#'
#' @keywords internal
#' @noRd
#' @param files A character vector  with file names of scripts where to search
#' for todo lists.
#' @param export Either \code{NULL}, in which case the extracted todo lists are
#' not exported or a path to an Rmarkdown file where to store the todo list.
#' @param overwrite A logical value indicating if an existing Rmarkdown file
#' pointed to by \code{export} should be overwritten (\code{TRUE}) or not
#' (\code{FALSE}).
#' @param title A character value representing the title of the Rmarkdown file
#' where to write the extracted todo lists to. If \code{export = NULL}, this is
#' ignored.
#' @return Invisibly returns a list of todo lists extracted from the scripts
#' pointed to by \code{files}.
#' @examples
#' \dontrun{
#' files  <- list.files("./../pmird_prepared_data", pattern = "pmird_prepare_data_.+\\.Rmd$", full.names = TRUE)
#' pm_extract_todo(files, export = "./../pmird_prepared_data/extracted_todo.Rmd")
#' }
pm_extract_todo <- function(files, export = NULL, overwrite = FALSE, title = "Extracted todo list for pmird") {

  # checks
  stopifnot(is.character(files) && all(purrr::map_lgl(files, file.exists)))
  stopifnot(is.null(export) || (is.character(export) && length(export) == 1L))
  stopifnot(is.logical(overwrite) && length(overwrite) == 1L)
  stopifnot(is.null(title) && is.null(export) || (is.character(title) && length(title) == 1L))

  # extract todo lists
  res <-
    tibble::tibble(
      file = files,
      todo =
        purrr::map(files, function(x) {
          res <- readLines(con = x)
          index_todo_header <- purrr::map_lgl(res, stringr::str_detect, pattern = "# Todo")
          if(any(index_todo_header)) {
            res[which(index_todo_header):length(res)]
          } else {
            ""
          }
        })
    )

  if(! is.null(export)) {

   if(file.exists(export) && ! overwrite) {
     stop("file.exists(export) && !overwrite")
   }

    res_export <-
      res %>%
      dplyr::mutate(
        file_name =
          file %>%
          stringr::str_extract(pattern = "/pmird_prepare_data_.+\\.Rmd$") %>%
          stringr::str_remove(pattern = "^/"),
        todo =
          purrr::map(seq_along(.data$todo), function(i) {
            todo_i <- .data$todo[[i]]
            todo_i <- todo_i[!stringr::str_detect(todo_i, pattern = "# Todo")]
            c(paste0("# ", .data$file_name[[i]]), todo_i, "")
          })
      )

    res_export_txt <-
      c(
        "---",
        paste0('title: "', title, '"'),
        'date: "`r Sys.Date()`"',
        'output: html_document',
        "---",
        "",
        res_export$todo
      )

    res_export_txt %>%
      unlist() %>%
      writeLines(con = export)

  }

  res

}
