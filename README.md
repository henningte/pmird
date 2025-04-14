
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmird

<!-- [ -->

<!--]{style="float:right"} -->

**R interface to the peatland mid infrared database**

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

<!-- [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh///master?urlpath=rstudio)  -->

pmird is an interface to the peatland mid infrared spectra database
(pmird database) (Teickner and Knorr 2025). ‘pmird’ allows to access the
pmird database as [`dm`](https://dm.cynkra.com/reference/dm.html)
object.

The pmird package can be installed as follows:

``` r
remotes::install_github("henningte/pmird")
```

The pmird database can be downloaded from Zotero (Teickner and Knorr
2025). The downloaded database needs to be imported in a running MariaDB
instance. In a linux terminal, the downloaded sql file can be imported
like so:

``` bash
mysql -u<user> -p<password> pmird < pmird-backup-2025-04-03.sql
```

Here, <user> and <password> are the respective database user name and
password.

The database itself does not contain the infrared spectra. These data
are in folder `pmird_prepared_data` which needs to be stored at any
place in the file system.

Once the database is set up and runs in a ‘MariaDB’ instance, it can be
accessed from within R, using the ‘RMariaDB’ package (Müller et al.
2021):

``` r
library(pmird)
library(RMariaDB)
library(magrittr)
library(ir)
#> Registered S3 methods overwritten by 'tibble':
#>   method     from  
#>   format.tbl pillar
#>   print.tbl  pillar
library(quantities)
#> Loading required package: units
#> Warning: package 'units' was built under R version 4.0.5
#> udunits database from /usr/share/xml/udunits/udunits2.xml
#> Loading required package: errors

# connect to database
con <-
  RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = "pmird",
    default.file = "~/my.cnf",
    groups = "rs-dbi"
  )
```

Here, `my.cnf` is a text file that stores user and password information
for the database server.

From this on, the ‘pmird’ R package can be used to access the database.
The ‘pmird’ R package makes use of the R package ‘dm’ (Schieferdecker,
Müller, and Bergant 2022) to access and manipulate the database
contents. `pmird::pm_get_dm()` creates a `dm` object which stores the
database structure.

``` r
# create the dm object
dm_pmird <- pmird::pm_get_dm(con, learn_keys = TRUE)
```

The option `learn_keys = TRUE` means that information on the primary and
foreign key are added to the dm object.  
A `dm` object is a representation of the entire database and allows
comfortable manipulation of the database from within R (e.g. addition of
new rows to tables, addition of new tables, data queries)
(Schieferdecker, Müller, and Bergant 2022).

## Use case: obtaining general information on the datasets contained in the ‘pmird’ database

General information on the datasets contained in the ‘pmird’ database
are stored in table `datasets`. The ‘pmird’ R package provides a
function to obtain this table from the `dm` object
(`pmird::pm_get_table(.table_name = "datasets)`).

``` r
# extract the datasets table
pmird_datasets <-
  dm_pmird %>%
  pmird::pm_get_table(.table_name = "datasets")
```

This table can, for example, be used to select studies for which to
extract data from the ‘pmird’ database.

## Use case: extracting data for a specific dataset from the ‘pmird’ database

Assume you are interested in viewing all measured data for one specific
dataset, e.g. the dataset with `id_dataset == 8` in `pmird_datasets`.
Using the ‘dm’ package and the `dm` object representing the ‘pmird’
database (`dm_pmird`), this data can be obtained as follows:

``` r
# get data for the dataset with ID 8
d8 <-
  dm_pmird %>%
  dm::dm_zoom_to(datasets) %>%
  dm::filter(id_dataset == 8) %>%
  dm::left_join(samples, by = "id_dataset") %>%
  dm::left_join(data_to_samples, by = "id_sample") %>%
  dm::left_join(data, by = "id_measurement") %>%
  dm::left_join(mir_metadata, by = "id_measurement") %>%
  dm::left_join(macrofossils, by = "id_measurement") %>%
  dm::pull_tbl() %>%
  tibble::as_tibble()
```

The resulting data frame (`d8`) contains information on all samples and
measurements for the dataset with `id_dataset == 8`. `d8` does not yet
contain any spectra, but stores only information on the respective file
paths to the downloaded spectra files within the downloaded folder
`pmird_prepared_data`.

To load the spectra, you can use the function
`pmird::pmird_load_spectra()`. In addition, you have to specify via
argument `directory` in which folder `pmird_prepared_data` is stored (on
the computer used for this tutorial, this was `".."`):

``` r
# load the spectra
d8 <- pmird::pm_load_spectra(d8, directory = "..")
```

`pm_load_spectra()` is a wrapper function around functions from the
package ‘ir’ (Teickner 2022) which in turn are wrappers around
`read.spc()` (Beleites and Sergo 2021) and `read.csv()`.
`pm_load_spectra()` therefore can load spectra both saved as spc and csv
files. `pm_load_spectra()` also converts `d8` into an object of class
`ir` from the ‘ir’ package. ‘ir’ provides functions for spectral
preprocessing and manipulation (Teickner 2022) and is compatible with
the ‘irpeat’ package which provides functions to analyze peat MIRS and
spectral prediction models to predict peat properties from MIRS
(Teickner and Hodgkins 2022).

## Use case: Handling units and measurement errors

The R package ‘qauntities’ (Pebesma, Mailund, and Hiebert 2016; Ucar,
Pebesma, and Azcorra 2018) can be used to add units and measurement
errors to measured variables from the ‘pmird’ database. The ‘pmird’ R
package allows batch unit and measurement error assignment:

``` r
# add information on units and errors with the 'quantities' package
d8 <-
  d8 %>%
  pmird::pm_add_quantities()

# show some values
head(d8$N, 3)
#> Units: [g/g]
#> Errors: 0.0000996441 0.0001036096 0.0001168346
#> [1] 0.014054321 0.007934011 0.021408554
```

## Use case: Generating data citations

Whoever uses data from the ‘pmird’ database must cite, in addition to
the database, the original data sources for the used datasets. To make
this straightforward, the ‘pmird’ R package contains the function
`pm_get_citations()` to generate such a citation list for any extracted
data subset:

``` r
# collect all citations for `d8`:
d8_citations <- 
  pm_get_citations(
    con = con,
    x = d8$id_measurement, 
    file = "d8_citations.bib"
  )
#> Loading required namespace: bib2df

# close connection to database
RMariaDB::dbDisconnect(con)
```

The function takes column `id_measurement` of the extracted data and
collects citations for all relevant data sources from the database. The
results are exported to a bibtex file which is defined via argument
`file`. This file can be imported to literature reference software. In
this case, since the data have not been previously published, the
created bibtex file is empty. `RMariaDB::dbDisconnect(con)` closes the
connection to the database, as this is the final use case presented
here.

<!-- ### How to cite

Please cite this compendium as:

> Authors, (2025). _Compendium of R code and data for Title of your paper goes here_. Accessed 14 Apr 2025. Online at <https://doi.org/xxx/xxx>

### How to download or install

You can download the compendium as a zip from from this URL: </archive/master.zip>

Or you can install this compendium as an R package, pmird, from GitHub with:



```r
# install.packages("devtools")
remotes::install_github("/")
```
-->

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the
[DESCRIPTION](https://github.com/henningte/pmird/blob/master/DESCRIPTION)
file

**Data :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

### Contributions

We welcome contributions from everyone. Please note that the pmird
project is released with a [Contributor Code of
Conduct](https://henningte.github.io/pmird//CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

### References

<div id="refs" class="references">

<div id="ref-Beleites.2021">

Beleites, Claudia, and Valter Sergo. 2021. “hyperSpec: A Package to
Handle Hyperspectral Data Sets in R.”

</div>

<div id="ref-Muller.2021a">

Müller, Kirill, Jeroen Ooms, David James, Saikat DebRoy, Hadley Wickham,
and Jeffrey Horner. 2021. “RMariaDB: Database Interface and ’MariaDB’
Driver.”

</div>

<div id="ref-Pebesma.2016">

Pebesma, Edzer, Thomas Mailund, and James Hiebert. 2016. “Measurement
Units in R.” *R Journal* 8 (2): 486–94.
<https://doi.org/10.32614/RJ-2016-061>.

</div>

<div id="ref-Schieferdecker.2022">

Schieferdecker, Tobias, Kirill Müller, and Darko Bergant. 2022. “dm:
Relational Data Models.”

</div>

<div id="ref-Teickner.2022e">

Teickner, Henning. 2022. “ir: Functions to Handle and Preprocess
Infrared Spectra.” Zenodo. <https://doi.org/10.5281/ZENODO.6644806>.

</div>

<div id="ref-Teickner.2022d">

Teickner, Henning, and Suzanne Hodgkins. 2022. “irpeat 0.2.0: Functions
to Analyze Mid-Infrared Spectra of Peat Samples.” Zenodo.
<https://doi.org/10.5281/ZENODO.7262744>.

</div>

<div id="ref-Teickner.2025c">

Teickner, Henning, and Klaus-Holger Knorr. 2025. “Peatland Mid Infrared
Spectra Database.”

</div>

<div id="ref-Ucar.2018">

Ucar, Iñaki, Edzer Pebesma, and Arturo Azcorra. 2018. “Measurement
Errors in R.” *R Journal* 10 (2): 549–57.
<https://doi.org/10.32614/RJ-2018-075>.

</div>

</div>
