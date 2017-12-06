FROM rocker/shiny:latest

# install devel version openssl for some R packages
RUN apt-get update -y --no-install-recommends && apt-get -y install -f -t unstable \
            libssl-dev \
            libcurl4-openssl-dev
            
# install R packages
RUN Rscript -e "install.packages(c('shinydashboard', 'htmlTable', 'DT', 'devtools'), \
                repos='https://cran.rstudio.com/')"
RUN Rscript -e "source('https://bioconductor.org/biocLite.R'); biocLite('BiocInstaller'); \
                biocLite('SingleCellExperiment')"
RUN Rscript -e "devtools::install_github('hemberg-lab/scmap')"

# add app to the server
ADD . /srv/shiny-server/scmap

# expose a port
EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
