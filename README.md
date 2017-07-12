# scmap-shiny

This is a cloud implementation of the [`scmap` R package](https://github.com/hemberg-lab/scmap) for projection of cells from a scRNA-Seq experiment to either another scRNA-Seq dataset or to a collection of multiple datasets (Reference cell atlas).

## Reference Datasets

To be able to use the `scmap-shiny` with your own Reference datasets you just need to substitute our references with yours in the `www/refs` folder.

To create your own reference please install `scmap` using:
```
install.packages("devtools")
devtools::install_github("hemberg-lab/scmap")
```

Then use the `createReference` method to create your References.

## Local Installation

To install and run `scmap-shiny` locally just clone this repository using:
```
git clone https://github.com/hemberg-lab/scmap-shiny.git
```

Then open it with RStudio and run the Shiny application.

## Installation on a Cloud

To install and run `scmap-shiny` on a Cloud please first download and install a [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) on your Cloud. Then clone this repository to the `/srv/shiny-server/scmap` folder. Start your Shiny server and `scmap-shiny` will be accessible via the following address: http://YOUR_CLOUD_IP:8080/scmap

