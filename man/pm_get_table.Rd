% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{pm_get_table}
\alias{pm_get_table}
\title{Function to extract a table from the pmird database}
\usage{
pm_get_table(x, .table_name)
}
\arguments{
\item{x}{A \code{dm} object as created using \code{\link{pm_get_dm}}.}

\item{.table_name}{A character value with the name of the table in the pmird
database from which to append new rows to a csv file.}
}
\value{
A \code{tibble} representing the extracted table.
}
\description{
Function to extract a table from the pmird database
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

# get dm object
dm_pmird <-
   pmird::pm_get_dm(con, learn_keys = TRUE)

# get table datasets
datasets <-
   pmird::pm_get_table(.table_name = "datasets")

RMariaDB::dbDisconnect(con)
}
}
