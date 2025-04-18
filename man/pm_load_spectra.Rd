% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pm_load_spectra.R
\name{pm_load_spectra}
\alias{pm_load_spectra}
\title{Loads spectra from files and adds these to the data frame with the file names.}
\usage{
pm_load_spectra(
  x,
  directory = ".",
  .col_files = "mirs_file",
  .col_sample_id = "id_sample"
)
}
\arguments{
\item{x}{A data frame.}

\item{directory}{A character value specifying the directory where the files
accompanying the 'pmird' database are stored.}

\item{.col_files}{The name of the column of \code{x} with file names for the
files storing mid infrared spectra.}

\item{.col_sample_id}{The name of the column of \code{x} with names for the
samples for which to load spectra. This column is used as column
\code{sample_id} when constructing the \code{ir} object.}
}
\value{
\code{x} as an object of class \code{ir} and with a new column
\code{spectra}.
}
\description{
\code{pm_load_spectra} takes a column with file names for files containing
mid infrared spectra from  a data frame, imports the spectra into a list
column \code{spectra} and adds them to the data frame which then becomes an
\code{\link{ir::ir_new_ir}[ir]} object.
}
\details{
Currently, the following file formats are supported for files
storing mid infrared spectra:
\enumerate{
  \item csv files
  \item Thermo Galactic's .spc files
}
}
\examples{
\dontrun{
# connect to database
con <-
  RMariaDB::dbConnect( # ---todo: change this
    drv = RMariaDB::MariaDB(),
    dbname = "pmird",
    default.file = "~/my.cnf"
  )

# extract data
x <-
  pm_get_dm(con, learn_keys = TRUE) \%>\%
  dm::dm_zoom_to(data) \%>\%
  dplyr::filter(id_measurement < 1000 & ! is.na(mirs_file)) \%>\%
  dm::pull_tbl() \%>\%
  as.data.frame()

# close connection
RMariaDB::dbDisconnect(con)

# get spectra
x <-
  pm_load_spectra(x, directory = "..", .col_files = "mirs_file", .col_sample_id = "id_measurement")

class(x)
}
}
