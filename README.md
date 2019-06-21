# marketingphdjobs

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/marketingphdjobs)](https://cran.r-project.org/package=marketingphdjobs)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of the marketingphdjobs package is to automate the collection of job listsings from the [marketingphdjobs.org](http://marketingphdjobs.org/). 
This package currently collects all available jobs, or a subset of job listings based on a user specified date.

## Installation

The `marketingphdjobs` package can be installed using the package `devtools.`
After installing `devtools`, `marketingphdjobs` is installed by entering:

```{r}
# install devtools if not already installed
# install.packages(devtools)
devtools::install_github("lachlandeer/marketingphdjobs")
```

into the R console.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(marketingphdjobs)
## download all job listings
job_list <-  get_job_listings()

## get jobs posted on and after a given date
date_threshold <- lubridate::ymd("2019/06/18")
new_jobs       <- get_new_jobs(date = date_threshold)

## get jobs in a country of a certain level
my_jobs <- job_list %>% 
            jobs_in_country("united_states")  %>% 
            job_type("assistant")
```

