# SchoolDataIT Version 0.2.7

## Package overview

Compiles and displays the available data sets regarding the Italian school system, with a focus on the infrastructural aspects.
Input datasets are downloaded from the web, with the aim of updating everything to real time.  
The functions are divided in four main modules, namely:
    'Get', to scrape raw data from the web
    'Util', various utilities needed to process raw data
    'Group', to aggregate data at the municipality or province level
    'Map', to visualize the output datasets.

More information can be found at [CRAN](https://cran.r-project.org/web/packages/SchoolDataIT/index.html).

## Installation from GitHub

Experimental package versions are stacked in GitHub before being submitted to CRAN
in order to comply with the CRAN policy of waiting about one month from a submission to another. 
Here, you will always find the latest developments; 
[here](https://github.com/lcef97/SchoolDataIT/blob/main/NEWS.md) you can see the whole version history.

To install the latest package version, run the code
``` r
devtools::install_github("lcef97/SchoolDataIT")
```
If you want to force R not to upgrade the dependencies, which may take some time, use instead:
``` r
devtools::install_github("lcef97/SchoolDataIT", upgrade = "never")
```

