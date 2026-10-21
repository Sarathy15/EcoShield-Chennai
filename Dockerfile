FROM rocker/shiny:4.3.3

WORKDIR /app

COPY . /app

RUN R -e "install.packages(c( \
  'shiny', \
  'bslib', \
  'dplyr', \
  'tidyr', \
  'readr', \
  'stringr', \
  'lubridate', \
  'ggplot2', \
  'plotly', \
  'DT', \
  'leaflet', \
  'htmltools', \
  'scales', \
  'jsonlite', \
  'httr2', \
  'rvest', \
  'xml2' \
), repos='https://cloud.r-project.org')"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '3838')))"]