# Version 0.2.2

## Package overview

Compiles and displays the available data sets regarding the Italian school system, with a focus on the infrastructural aspects.
Input datasets are downloaded from the web, with the aim of updating everything to real time.  
The functions are divided in four main modules, namely:
    'Get', to scrape raw data from the web
    'Util', various utilities needed to process raw data
    'Group', to aggregate data at the municipality or province level
    'Map', to visualize the output datasets.



## Installation from GitHub

Before installing this version, please make sure to have the `devtools` package installed. Otherwise, first run:
``` r 
install.packages("devtools") #if necessary
```
Then, the command for the installation is:
``` r
devtools::install_github("lcef97/SchoolDataIT")
```
If you want to force R not to upgrade the dependencies, use instead:
``` r
devtools::install_github("lcef97/SchoolDataIT", upgrade = "never")
```

