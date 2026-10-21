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

# Install required R packages and stop if installation fails
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    install.packages(c( \
      'openssl', 'httr', 'httr2', 's2', 'units', \
      'shiny', 'bslib', 'dplyr', 'tidyr', 'readr', \
      'stringr', 'lubridate', 'ggplot2', 'plotly', \
      'DT', 'leaflet', 'htmltools', 'scales', \
      'jsonlite', 'rvest', 'xml2', 'sf' \
    )); \
    required <- c('shiny', 'plotly', 'leaflet'); \
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]; \
    if (length(missing)) stop(paste('Missing packages:', paste(missing, collapse = ', ')))"