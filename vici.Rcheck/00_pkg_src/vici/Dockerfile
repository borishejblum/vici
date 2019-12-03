FROM rocker/tidyverse:3.5.2
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN R -e 'remotes::install_cran("cowplot")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("ggplot2")'
RUN R -e 'remotes::install_cran("ggpubr")'
RUN R -e 'remotes::install_cran("nlme")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("tidyr")'
RUN R -e 'remotes::install_cran("covr")'
RUN R -e 'remotes::install_cran("testthat")'
RUN R -e 'remotes::install_cran("RSelenium")'
RUN R -e 'remotes::install_cran("future")'
RUN R -e 'remotes::install_cran("pkgload")'
RUN R -e 'remotes::install_cran("processx")'
COPY vici_*.tar.gz /app.tar.gz

COPY Rprofile.site /usr/lib/R/etc/

RUN R -e 'remotes::install_local("/app.tar.gz")'
EXPOSE 3838
#CMD  ["R", "-e  vici::run_app()"]
