FROM rocker/shiny:4.3.3

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure CRAN
RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org'))" \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install core application packages
RUN R -e "install.packages(c( \
    'shiny', 'bslib', 'dplyr', 'tidyr', 'readr', \
    'stringr', 'lubridate', 'ggplot2', 'htmltools', \
    'scales', 'jsonlite', 'DT' \
))"

# Install web and visualization dependencies
RUN R -e "install.packages(c( \
    'openssl', 'httr', 'httr2', 'xml2', 'rvest', \
    'plotly', 'leaflet' \
))"

# Verify essential packages
RUN R -e "stopifnot( \
    requireNamespace('shiny', quietly=TRUE), \
    requireNamespace('plotly', quietly=TRUE), \
    requireNamespace('leaflet', quietly=TRUE) \
)"

# Copy application
WORKDIR /app
COPY . /app

# Start Shiny using Render's assigned port
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '3838')))"]