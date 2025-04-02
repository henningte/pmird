#' Converts a column with udunits as character values to the respective EML units.
#'
#' \code{pm_udunits_to_eml} takes a character vector with values representing
#' udunits units and converts these to units as used by the Ecological Metadata
#' Language. Note that the function converts only a specific sets of units as
#' needed for the pmird database. All other units are converted to \code{NA}.
#'
#' @param x A character vector with values representing udunits units.
#' @return A character vector with the same length as rows in \code{x} with
#' units according to the Ecological Metadata Language.
#' @keywords Internal
#' @noRd
#' @examples
#' \dontrun{
#' # convert a known unit
#' pm_udunits_to_eml("mm")
#'
#' # convert a unit not yet implemented
#' pm_udunits_to_eml("L")
#' }
pm_udunits_to_eml <- function(x) {

  # checks
  stopifnot(is.character(x))

  tibble::tibble(
    unit = x
  ) %>%
    dplyr::mutate(
      unit =
        dplyr::case_when(
          .data$unit == "g" ~ "gram",
          .data$unit == "cm" ~ "centimeter",
          .data$unit == "m" ~ "meter",
          .data$unit == "cm^3" ~ "centimeterCubed",
          .data$unit == "g/g" ~ "gramsPerGram",
          .data$unit == "ug/g" ~ "microgramsPerGram",
          .data$unit == "cm^3/cm^3" ~ "cubicCentimetersPerCubicCentimeters",
          .data$unit == "g/cm^3" ~ "gramsPerCubicCentimeter",
          .data$unit == "umol/g" ~ "micromolePerGram",
          .data$unit == "kJ/mol" ~ "kilojoulePerMole",
          .data$unit == "J/K/mol" ~ "joulePerKelvinPerMole",
          .data$unit == "cm/h" ~ "centimeterPerHour",
          .data$unit == "Bq/kg" ~ "becquerelPerKilogram",
          .data$unit == "yr BP" ~ "nominalYear",
          .data$unit == "yr" ~ "nominalYear",
          .data$unit == "1/g/min" ~ "perGramPerMinute",
          .data$unit == "s" ~ "seconds",
          .data$unit == "kHz" ~ "kilohertz",
          .data$unit == "g/L" ~ "gramsPerLiter",
          .data$unit == "K" ~ "kelvin",
          .data$unit == "L/L" ~ "litersPerLiter",
          .data$unit == "mm" ~ "millimeter",
          TRUE ~ "dimensionless"
        )
    ) %>%
    dplyr::pull(.data$unit)

}
