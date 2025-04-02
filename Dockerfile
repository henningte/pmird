FROM rocker/verse:4.0.1

# system dependencies
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y gdal-bin=3.0.4+dfsg-1build3 \
	libgdal-dev=3.0.4+dfsg-1build3 \
	libgeos-dev=3.8.0-1build1 \
	libgeos++-dev=3.8.0-1build1 \
	libudunits2-dev=2.2.26-5 \
	make=4.2.1-1.2 \
	pandoc=2.5-3build2 \
	pandoc-citeproc=0.15.0.1-1build4

# Additional dependencies ---todo: integrate above later on
RUN apt-get install -y libjq-dev

# get R packages
ENV RENV_VERSION 0.11.0-6
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# working directory name
WORKDIR /home/rstudio/pmird

# Restore renv cache
RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
RUN chown -R rstudio . \
 && sudo -u rstudio Rscript -e 'renv::consent(provided = TRUE)' \
 && sudo -u rstudio Rscript -e 'Sys.setenv(R_INSTALL_STAGED = FALSE)' \
 && sudo -u rstudio R -e 'renv::restore(repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest", CRAN = "https://cran.r-project.org/"))'

# labels
LABEL maintainer = "Henning Teickner <henning.teickner@uni-muenster.de>" \
  org.opencontainers.image.authors = "Henning Teickner <henning.teickner@uni-muenster.de>" \
  author.orcid = "0000-0002-3993-1182" \
  edu.science.data.group.project = " " \
  org.opencontainers.image.version = "0.0.0.9001" \
  org.opencontainers.image.licenses = "GPL-3"

# instructions

# to build the image, navigate to the directory with the Dockerfile and run:
# docker build -t pmird:0.0.0.9001 .

# the image is intended to be run in combination with a docker container with the
# MariaDB database holding the data. To run both containers within a network, do
# the following (source: http://colinfay.me/r-db/index.html):

# set up network
# docker network create r-db

# pull a MariaDB image
# docker pull mariadb:10.4.5-bionic

# run the MariaDB image in a container
# docker run --net r-db --name mariadb -v $(pwd)/mysql:/var/lib/mysql \
#   -e MYSQL_ROOT_PASSWORD=coucou \
#   -d mariadb:10.4.5-bionic && sleep 60

# run the image defined in this file in a container
# docker run --name pmird_c --net r-db -e PASSWORD=pmird -p 8787:8787 -v $(pwd):/home/rstudio/pmird pmird:0.0.0.9001

# Then, you can connect from the RStudio server interface of the container
# created from this image and from withn R can connect to the MariaDB database
# in the mariadb container (host = "mariadb", user credentials as defined when
# running the mariadb docker container). Here e.g. using package RMariaDB:

# con <-
#  RMariaDB::dbConnect(
#    drv = RMariaDB::MariaDB(),
#    username = "root",
#    password = "coucou",
#    host = "mariadb"
#  )
