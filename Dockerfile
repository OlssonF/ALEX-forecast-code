FROM rocker/geospatial:4.2.2

RUN apt-get update && apt-get -y install libgd-dev libnetcdf-dev git

USER rstudio

RUN git clone https://github.com/ecoforecastVT/ALEX-forecast-code.git /home/rstudio/ALEX-forecast-code

RUN Rscript /home/rstudio/ALEX-forecast-code/workflows/scenario_reforecasts/install_packages.R

USER root