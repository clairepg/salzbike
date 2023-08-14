# Base image https://hub.docker.com/u/rocker/
FROM r-base

# system libraries of general use
## install debian packages
RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libpoppler-cpp-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libjpeg-dev \
    libpng-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Set the working directory
WORKDIR /salzbike

# Copy necessary files
COPY app.R /salzbike/app.R
COPY data /salzbike/data

# Install R packages
# Install R packages
RUN Rscript -e 'install.packages("raster", dependencies = TRUE) ; if (!library(raster, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("sf", dependencies = TRUE) ; if (!library(sf, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("lwgeom", dependencies = TRUE) ;     if (!library(lwgeom, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("stars", dependencies = TRUE) ; if (!library(stars, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("units", dependencies = TRUE)  ; if (!library(units, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("XML", dependencies = TRUE) ; if (!library(XML, logical.return=TRUE)) quit(status=10)'
#RUN Rscript -e 'install.packages("V8", dependencies = TRUE) ; if (!library(V8, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("leaflet", dependencies = TRUE) ; if (!library(leaflet, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("tmaptools", dependencies = TRUE) ; if (!library(tmaptools, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("tmap", dependencies = TRUE) ;     if (!library(tmap, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("MASS", dependencies = TRUE) ; if (!library(MASS, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("shiny", dependencies = TRUE)  ; if (!library(shiny, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("leafsync", dependencies = TRUE) ; if (!library(leafsync, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("leafem", dependencies = TRUE)  ; if (!library(leafem, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("shinythemes", dependencies = TRUE)  ; if (!library(shinythemes, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("dplyr", dependencies = TRUE)  ; if (!library(dplyr, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("httr", dependencies = TRUE)  ; if (!library(httr, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("ggplot2", dependencies = TRUE)  ; if (!library(ggplot2, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("htmltools", dependencies = TRUE)  ; if (!library(htmltools, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("thematic", dependencies = TRUE)  ; if (!library(thematic, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("RColorBrewer", dependencies = TRUE)  ; if (!library(RColorBrewer, logical.return=TRUE)) quit(status=10)'
RUN Rscript -e 'install.packages("shinyWidgets", dependencies = TRUE)  ; if (!library(shinyWidgets, logical.return=TRUE)) quit(status=10)'


# Expose port
EXPOSE 80

#root error logs not into a log but directly to the console 
ENV SHINY_LOGS_STDERR=1

# Run the app on container start
CMD ["R", "-e", "shiny::runApp('/salzbike', host = '0.0.0.0', port = 80)"]

