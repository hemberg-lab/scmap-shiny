# scmap-shiny

This is a [shiny](https://shiny.rstudio.com/) implementation of the [`scmap` R package](http://bioconductor.org/packages/scmap) for projection of cells from a scRNA-Seq experiment to either another scRNA-Seq dataset or to a collection of multiple datasets indexes.

## Indexes

To be able to use the `scmap-shiny` with your own dataset indexes you just need to substitute our references with yours in the `www` folder.

To create your own indexes please install `scmap` using:
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("scmap")
```

Then use the `indexCluster` or `indexCell` methods to create your indexes.

## Running locally

To install and run `scmap-shiny` locally just clone this repository using:
```
git clone https://github.com/hemberg-lab/scmap-shiny.git
```

Then open it with `RStudio` and run the Shiny application.

## Running on a Cloud

To run `scmap-shiny` on a Cloud please create a Docker image of it using the [Dockerfile](Dockerfile) provided. Once the image is ready it can be run either locally or on a Cloud.

We have built our own image of `scmap-shiny` using [quay.io](quay.io). To run it locally:
```
docker run --rm -p 3838:3838 quay.io/hemberg-group/scmap-shiny
```
Then `scmap-shiny` will be accessible at http://localhost:3838/scmap/


To run in a detached mode on the Cloud:
```
docker run -d -p 80:3838 quay.io/hemberg-group/scmap-shiny
```
Then `scmap-shiny` will be accessible at http://YOUR_CLOUD_IP/scmap/.

Alternatively, you can manually install a Shiny server on your instance and all corresponding R packages mentioned in the [Dockerfile](Dockerfile). You will also need to copy your `scmap-shiny` files to `/srv/shiny-server/scmap` folder.
