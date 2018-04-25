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
RUN for d in atlases/*/; do cp app/* "$d"; done
RUN for d in atlases/*/www/; do cp utils/* "$d"; done
ADD atlases/ /srv/shiny-server

# update the index page
COPY index_page/index.html /srv/shiny-server/index.html
COPY index_page/img /srv/shiny-server/img

# try to avoid greying out of the apps
# https://stackoverflow.com/questions/44397818/shiny-apps-greyed-out-nginx-proxy-over-ssl
RUN echo 'sanitize_errors off;disable_protocols xdr-streaming xhr-streaming iframe-eventsource iframe-htmlfile;' >> /etc/shiny-server/shiny-server.conf

# expose a port
EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
