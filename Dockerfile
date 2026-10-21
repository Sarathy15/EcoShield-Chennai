FROM rocker/shiny:4.3.3

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

WORKDIR /srv/shiny-server/

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]