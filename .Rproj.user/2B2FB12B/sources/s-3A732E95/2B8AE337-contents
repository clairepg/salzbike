# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update && apt-get upgrade -y

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libudunits2-dev
RUN apt-get update && apt-get remove -y libmariadb-dev && apt-get install -y libgdal-dev libgeos-dev libproj-dev


## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Set the working directory
WORKDIR /your_folder_name

# Copy necessary files
COPY app.R /ubuntu_app/app.R
COPY data /ubuntu_app/data

# Install R packages
RUN Rscript -e 'install.packages("tmap", dependencies = TRUE) ;     if (!library(tmap, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("MASS", dependencies = TRUE) ; if (!library(MASS, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("shiny", dependencies = TRUE)  ; if (!library(shiny, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("leaflet", dependencies = TRUE) ; if (!library(leaflet, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("sf", dependencies = TRUE) ;     if (!library(sf, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("dplyr", dependencies = TRUE) ;     if (!library(dplyr, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("lwgeom", dependencies = TRUE) ;     if (!library(lwgeom, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("httr", dependencies = TRUE) ;     if (!library(httr, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("ggplot2", dependencies = TRUE) ;     if (!library(ggplot2, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("htmltools", dependencies = TRUE) ;     if (!library(htmltools, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("shinythemes", dependencies = TRUE) ;     if (!library(shinythemes, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("thematic", dependencies = TRUE) ;     if (!library(thematic, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("RColorBrewer", dependencies = TRUE) ;     if (!library(RColorBrewer, logical.return=TRUE)) quit(status=10)'

# Expose port
EXPOSE 80

#root error logs not into a log but directly to the console 
ENV SHINY_LOGS_STDERR=1

# Run the app on container start
CMD ["R", "-e", "shiny::runApp('/your_folder_name', host = '0.0.0.0', port = 80)"]

