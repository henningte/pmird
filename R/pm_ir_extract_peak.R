#' Extracts a peak from an infrared spectrum
#'
#' \code{pm_ir_extract_peak} is a function specifically designed for pmird to
#' help quality assessment of mid infrared spectra. It clips a mid infrared
#' spectra to a defined region, baseline corrects this region, and identifies
#' if the peak has a positive direction or a negative direction.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param range A data frame with one row. See
#' \code{\link[ir:ir_clip]{ir_clip}}. This is the range which is extracted from
#' \code{x}.
#' @param peak_max A numeric value representing the wavenumber value where the
#' peak maximum (or minimum if the peak has a negative direction) is.
#' @param range_offset A positive numeric value representing the offset which is
#' additionally clipped from the x axis range given by \code{range} to avoid
#' artifacts during baseline correction.
#' @return \code{x} clipped to \code{range} (less \code{range_offset}), baseline
#' corrected with \code{\link[ir:ir_bc_rubberband]{ir_bc_rubberband}} and with
#' a new column \code{peak_direction} with value \code{"up"} if the peak is a
#' maximum or \code{"down"} if not.
#' @keywords Internal
#' @noRd
#' @export
pm_ir_extract_peak <- function(x, range, peak_max = 2362, range_offset = 4) {

  if(!requireNamespace(package = "ir", version = "0.2.0")) {
    stop("Package 'ir' (>= 0.2.0) required. Install this package first.")
  }

  stopifnot(inherits(x, "ir"))
  stopifnot(is.data.frame(range) && nrow(range) == 1 && ncol(range) == 2 && colnames(range) %in% c("start", "end"))
  stopifnot(is.numeric(peak_max) && length(peak_max) == 1 && peak_max >= range$start && peak_max <= range$end)
  stopifnot(is.numeric(range_offset) && length(range_offset) == 1)

  measurement_id <- x$measurement_id

  x_clipped <-
    x %>%
    ir::ir_clip(range = range)

  # get peak direction
  x_clipped_bc <-
    x_clipped %>%
    dplyr::select(dplyr::all_of(c("sample_id", "measurement_id", "spectra"))) %>%
    ir::ir_bc_rubberband(return_bl = FALSE) %>%
    ir::ir_clip(
      range =
        range %>%
        dplyr::mutate(
          start = .data$start + .env$range_offset,
          end = .data$end - .env$range_offset
        )
    ) %>%
    ir::ir_normalize(method = "zeroone") %>%
    # get intensity at peak_max
    ir::ir_get_intensity(wavenumber = peak_max) %>%
    tidyr::unnest(dplyr::any_of("intensity")) %>%
    ir::ir_as_ir() %>%
    dplyr::rename(intensity_peak_max_1 = "y") %>%
    dplyr::select(-.data$x) %>%
    # get intensity at peak_max (after turning)
    ir::ir_multiply(y = -1) %>%
    ir::ir_normalize(method = "zeroone") %>%
    ir::ir_get_intensity(wavenumber = peak_max) %>%
    tidyr::unnest(dplyr::any_of("intensity")) %>%
    ir::ir_as_ir() %>%
    dplyr::rename(intensity_peak_max_2 = "y") %>%
    dplyr::select(-.data$x) %>%
    dplyr::mutate(
      peak_direction =
        ifelse(
          .data$intensity_peak_max_1 > .data$intensity_peak_max_2,
          "up",
          "down"
        )
    )

  # baseline corrected peaks
  x_clipped_up <-
    x_clipped %>%
    dplyr::filter(x_clipped_bc$peak_direction == "up") %>%
    dplyr::mutate(
      peak_direction = "up"
    ) %>%
    ir::ir_bc_rubberband(return_bl = FALSE) %>%
    ir::ir_clip(
      range =
        range %>%
        dplyr::mutate(
          start = .data$start + .env$range_offset,
          end = .data$end - .env$range_offset
        )
    ) %>%
    ir::ir_bc_rubberband(return_bl = FALSE)

  x_clipped_down <-
    x_clipped %>%
    dplyr::filter(x_clipped_bc$peak_direction == "down") %>%
    dplyr::mutate(
      peak_direction = "down"
    ) %>%
    ir::ir_multiply(y = -1) %>%
    ir::ir_bc_rubberband(return_bl = FALSE) %>%
    ir::ir_clip(
      range =
        range %>%
        dplyr::mutate(
          start = .data$start + .env$range_offset,
          end = .data$end - .env$range_offset
        )
    ) %>%
    ir::ir_bc_rubberband(return_bl = FALSE) %>%
    ir::ir_multiply(y = -1)

  dplyr::bind_rows(
    x_clipped_up,
    x_clipped_down
  ) %>%
    dplyr::slice(match(!!measurement_id, .data$measurement_id))

}
