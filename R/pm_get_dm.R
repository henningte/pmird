#' Access the pmird database as a \code{dm} object.
#'
#' \code{pm_get_dm} extracts tables and relations in the form of a
#' \code{\link[dm:dm]{dm}} object. It is a wrapper around
#' \code{\link[dm:dm_from_src]{dm_from_src}} which manually adds primary and
#' foreign keys to the tables which dm currently does not support for MariaDB
#' databases.
#'
#' @param con A connection to the pmird database. This should be an object of
#' class \code{\link[RMariaDB::`MariaDBConnection-class`]{MariaDBConnection}}, created
#' using \code{\link[RMariaDB::`MariaDBConnection-class`]{dbConnect}}.
#' @param learn_keys A logical value indicating if primary and foreign keys
#' should be assigned (\code{TRUE}) or not (\code{FALSE}).
#' @return A \code{dm} object with the pmird database and all primary and
#' foreign keys.
#' @examples
#' \dontrun{
#' # connect to database
#' con <-
#'   RMariaDB::dbConnect(
#'     drv = RMariaDB::MariaDB(),
#'     dbname = "pmird",
#'     default.file = "~/my.cnf"
#'   )
#'
#' # get pmird database as dm object
#' dm_pmird <- pm_get_dm(con, learn_keys = TRUE)
#'
#' # when finished, disconnect
#' RMariaDB::dbDisconnect(con)
#' }
#' @export
pm_get_dm <- function(con, learn_keys = FALSE) {

  # avoid package check message
  attributes_to_datasets_to_citations <- attributes_to_datasets_to_methods <- change_histories <- citations <- citations_to_datasets <- citations_to_method_steps <- citations_to_methods <- citations_to_quality_control <- citations_to_samples <- contacts <- coverages <- custom_units <- data <- data_to_samples <- datasets <- datasets_to_coverages <- geographic_coverages <- id_attribute <- id_change_history <- id_citation <- id_coverage <- id_dataset <- id_geographic_coverage <- id_instrument <- id_license <- id_macrofossil_type <- id_maintenance <- id_measurement <- id_measurement_scale <- id_method <- id_method_step <- id_missing_value_code <- id_person <- id_quality_control <- id_sample <- id_taxonomic_classification <- id_taxonomic_coverage <- id_temporal_coverage <- id_unit <- instruments <- licenses <- macrofossil_types <- macrofossils <- mactyps_to_taxclas <- maintenances <- maintenances_to_change_histories <- measurement_scales <- measurement_scales_date_time <- measurement_scales_interval <- measurement_scales_nominal <- measurement_scales_ordinal <- measurement_scales_ratio <- method_steps <- method_steps_to_instruments <- methods <- methods_to_coverages <- methods_to_method_steps <- mir_metadata <- missing_value_codes <- persons <- persons_to_datasets <- quality_control <- quality_control_to_method_steps <- samples <- taxclas_to_taxcovs <- taxonomic_classifications <- taxonomic_coverages <- temporal_coverages <- NULL

  # checks
  stopifnot(inherits(con, "MariaDBConnection"))
  stopifnot(is.logical(learn_keys) && length(learn_keys) == 1)

  # extract tables
  dm_pmird <-
    dm::dm_from_src(src = con, table_names = NULL, learn_keys = FALSE)

  # add keys
  if(learn_keys) {
    dm_pmird <-
      dm_pmird %>%
      dm::dm_add_pk(datasets, id_dataset) %>%
      dm::dm_add_pk(licenses, id_license) %>%
      dm::dm_add_pk(coverages, id_coverage) %>%
      dm::dm_add_pk(geographic_coverages, id_geographic_coverage) %>%
      dm::dm_add_pk(temporal_coverages, id_temporal_coverage) %>%
      dm::dm_add_pk(taxonomic_coverages, id_taxonomic_coverage) %>%
      dm::dm_add_pk(taxonomic_classifications, id_taxonomic_classification) %>%
      dm::dm_add_pk(maintenances, id_maintenance) %>%
      dm::dm_add_pk(change_histories, id_change_history) %>%
      dm::dm_add_pk(methods, id_method) %>%
      dm::dm_add_pk(method_steps, id_method_step) %>%
      dm::dm_add_pk(instruments, id_instrument) %>%
      dm::dm_add_pk(quality_control, id_quality_control) %>%
      dm::dm_add_pk(persons, id_person) %>%
      dm::dm_add_pk(citations, id_citation) %>%
      dm::dm_add_pk(data, id_measurement) %>%
      dm::dm_add_pk(samples, id_sample) %>%
      dm::dm_add_pk(mir_metadata, id_measurement) %>%
      dm::dm_add_pk(attributes, id_attribute) %>%
      dm::dm_add_pk(measurement_scales, id_measurement_scale) %>%
      dm::dm_add_pk(measurement_scales_nominal, id_measurement_scale) %>%
      dm::dm_add_pk(measurement_scales_ordinal, id_measurement_scale) %>%
      dm::dm_add_pk(measurement_scales_interval, id_measurement_scale) %>%
      dm::dm_add_pk(measurement_scales_ratio, id_measurement_scale) %>%
      dm::dm_add_pk(measurement_scales_date_time, id_measurement_scale) %>%
      dm::dm_add_pk(missing_value_codes, id_missing_value_code) %>%
      dm::dm_add_pk(units, id_unit) %>%
      dm::dm_add_pk(custom_units, id_unit) %>%
      dm::dm_add_pk(macrofossil_types, id_macrofossil_type) %>%
      dm::dm_add_fk(datasets, id_license, licenses, check = TRUE) %>%
      dm::dm_add_fk(datasets, id_maintenance, maintenances, check = TRUE) %>%
      dm::dm_add_fk(datasets, id_method, methods, check = TRUE) %>%
      dm::dm_add_fk(datasets_to_coverages, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(datasets_to_coverages, id_coverage, coverages, check = TRUE) %>%
      dm::dm_add_fk(geographic_coverages, id_coverage, coverages, check = TRUE) %>%
      dm::dm_add_fk(temporal_coverages, id_coverage, coverages, check = TRUE) %>%
      dm::dm_add_fk(taxonomic_coverages, id_coverage, coverages, check = TRUE) %>%
      dm::dm_add_fk(taxclas_to_taxcovs, id_taxonomic_coverage, taxonomic_coverages, check = TRUE) %>%
      dm::dm_add_fk(taxclas_to_taxcovs, id_taxonomic_classification, taxonomic_classifications, check = TRUE) %>%
      dm::dm_add_fk(maintenances_to_change_histories, id_maintenance, maintenances, check = TRUE) %>%
      dm::dm_add_fk(maintenances_to_change_histories, id_change_history, change_histories, check = TRUE) %>%
      dm::dm_add_fk(methods, id_quality_control, quality_control, check = TRUE) %>%
      dm::dm_add_fk(quality_control_to_method_steps, id_quality_control, quality_control, check = TRUE) %>%
      dm::dm_add_fk(quality_control_to_method_steps, id_method_step, method_steps, check = TRUE) %>%
      dm::dm_add_fk(methods_to_coverages, id_coverage, coverages, check = TRUE) %>%
      dm::dm_add_fk(methods_to_coverages, id_method, methods, check = TRUE) %>%
      dm::dm_add_fk(methods_to_method_steps, id_method_step, method_steps, check = TRUE) %>%
      dm::dm_add_fk(methods_to_method_steps, id_method, methods, check = TRUE) %>%
      dm::dm_add_fk(method_steps_to_instruments, id_instrument, instruments, check = TRUE) %>%
      dm::dm_add_fk(method_steps_to_instruments, id_method_step, method_steps, check = TRUE) %>%
      dm::dm_add_fk(method_steps_to_instruments, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(persons_to_datasets, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(persons_to_datasets, id_person, persons, check = TRUE) %>%
      dm::dm_add_fk(contacts, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(contacts, id_person, persons, check = TRUE) %>%
      dm::dm_add_fk(citations_to_datasets, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(citations_to_datasets, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(citations_to_methods, id_method, methods, check = TRUE) %>%
      dm::dm_add_fk(citations_to_methods, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(citations_to_method_steps, id_method_step, method_steps, check = TRUE) %>%
      dm::dm_add_fk(citations_to_method_steps, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(citations_to_quality_control, id_quality_control, quality_control, check = TRUE) %>%
      dm::dm_add_fk(citations_to_quality_control, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(samples, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(data_to_samples, id_measurement, data, check = TRUE) %>%
      dm::dm_add_fk(data_to_samples, id_sample, samples, check = TRUE) %>%
      dm::dm_add_fk(mir_metadata, id_measurement, data, check = TRUE) %>%
      dm::dm_add_fk(citations_to_samples, id_sample, samples, check = TRUE) %>%
      dm::dm_add_fk(citations_to_samples, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(attributes, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(attributes, id_missing_value_code, missing_value_codes, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_nominal, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_ordinal, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_interval, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_ratio, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_date_time, id_measurement_scale, measurement_scales, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_interval, id_unit, units, check = TRUE) %>%
      dm::dm_add_fk(measurement_scales_ratio, id_unit, units, check = TRUE) %>%
      dm::dm_add_fk(custom_units, id_unit, units, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_methods, id_attribute, attributes, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_methods, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_methods, id_method, methods, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_citations, id_attribute, attributes, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_citations, id_dataset, datasets, check = TRUE) %>%
      dm::dm_add_fk(attributes_to_datasets_to_citations, id_citation, citations, check = TRUE) %>%
      dm::dm_add_fk(macrofossils, id_macrofossil_type, macrofossil_types, check = TRUE) %>%
      dm::dm_add_fk(macrofossils, id_measurement, data, check = TRUE) %>%
      dm::dm_add_fk(mactyps_to_taxclas, id_taxonomic_classification, taxonomic_classifications, check = TRUE) %>%
      dm::dm_add_fk(mactyps_to_taxclas, id_macrofossil_type, macrofossil_types, check = TRUE)
  }

  dm_pmird

}
