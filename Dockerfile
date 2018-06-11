FROM rocker/rstudio

# using Dirk's PPA (https://github.com/rocker-org/rocker/issues/124) 
# to install some R required stuff
# https://launchpad.net/~edd/+archive/ubuntu/misc
RUN apt-get update -y && apt-get install software-properties-common && add-apt-repository ppa:edd/misc && apt-get update -y && apt-get install r-cran-git2r
# RUN apt-get update -y --no-install-recommends && apt-get -y install -f -t unstable \
#            libssl-dev \
#            libcurl4-openssl-dev
            
# install R packages
RUN Rscript -e "install.packages(c('shinydashboard', 'htmlTable', 'DT', 'devtools'))"
RUN Rscript -e "source('https://bioconductor.org/biocLite.R'); biocLite('BiocInstaller'); \
                biocLite('SingleCellExperiment')"
RUN Rscript -e "devtools::install_github('hemberg-lab/scmap')"

# add app to the server
ADD atlases atlases/
ADD app app/
ADD utils utils/
RUN for d in atlases/*/; do cp app/* "$d"; done
RUN for d in atlases/*/www/; do cp utils/* "$d"; done
RUN cp -r atlases/* /srv/shiny-server

# update the index page
COPY index_page/index.html /srv/shiny-server/index.html
COPY index_page/img /srv/shiny-server/img

# try to avoid greying out of the apps
# https://stackoverflow.com/questions/44397818/shiny-apps-greyed-out-nginx-proxy-over-ssl
RUN echo 'sanitize_errors off;disable_protocols xdr-streaming xhr-streaming iframe-eventsource iframe-htmlfile;' >> /etc/shiny-server/shiny-server.conf

EXPOSE 8787

CMD ["/init"]
