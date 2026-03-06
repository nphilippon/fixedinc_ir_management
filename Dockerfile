# Pulls base image 
FROM --platform=linux/amd64 rocker/shiny-verse:latest
# ----------------

# (Thanks Phil for this one, saves some headaches)
ENV DEBIAN_FRONTEND=noninteractive

# ----------------------------------------------------------------------------------------------------------------------------------------------------
# Below: Installs some dependencies for later, if there are errors with the downloading/installation of packages for R later down the line, text Alex.

# Current dependencies loaded:
# git - This should be very, very obvious
# libssl - Open SSL/TLS stuff (https://packages.debian.org/sid/libssl-dev)
# libcurl4-gnutls-dev - Client side URL transfer library (https://packages.debian.org/trixie/libcurl4-gnutls-dev)
# libxml2-dev - GNOME XML Library (https://packages.debian.org/sid/libxml2-dev)

<<<<<<< HEAD
RUN apt-get update && apt install -y -no-install-recommends \
    git \
=======
RUN apt-get update && apt-get install -y --no-install-recommends \
>>>>>>> refs/remotes/origin/main
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    && apt-get autoremove -y

RUN R -q -e 'if (!requireNamespace("devtools", quietly=TRUE)) install.packages("devtools", repos="https://cloud.r-project.org")' && \
    R -q -e 'devtools::install_github("risktoollib/RTL", upgrade="never")' && \
    R -q -e "install.packages(c('shiny', 'shinydashboard', 'quantmod', 'tidyquant', 'mgttr', 'DT', 'plotly', 'shinyjs', 'bslib', 'rstudioapi', 'splines', 'RcppRoll', 'Rcpp'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"

# Clones the repo into a new folder, changes owners to non-root. (Shout out the bash manual)
RUN git clone https://github.com/nphilippon/fixedinc_ir_management.git /srv/shiny-server/shiny-IR

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/srv/shiny-server/shiny-IR', host='0.0.0.0', port=3838)"]
