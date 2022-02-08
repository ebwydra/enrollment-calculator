# enrollment-calculator
R Shiny app for generating NIH planned enrollment tables that are racially and ethnically representative of a given region based on American Community Survey (ACS) data. Check it out at: https://ebwydra.shinyapps.io/census-enrollment-calculator/

## Required R packages

The following packages are required: `shiny`, `shinycssloaders`, `shinyWidgets`, `httr`, `jsonlite`, `censusapi`, `tidyr`, `dplyr`, `data.table`, `tibble`

These packages can be installed all at once by running the following code in R:

``` r
install.packages(c("shiny", "shinycssloaders", "shinyWidgets", "httr", "jsonlite", 
                   "censusapi", "tidyr", "dplyr", "data.table", "tibble"))
```

## How to run

You will need a U.S. Census Bureau API key to run this app locally. [Request a free key here](https://api.census.gov/data/key_signup.html) or go to the U.S. [Census Bureau Developers landing page](https://www.census.gov/data/developers.html) and click on "Request a Key" on the left side of the page. Simply enter your email address and follow the instructions.

Next, clone this repo or save all files needed to run the app (`global.R`, `server.R`, `ui.R`) locally in a directory called enrollment-calculator. Once you have your API key, create a file called `.Rprofile` in the enrollment-calculator directory containing the following:

``` r
CENSUS_KEY = "your-key-goes-here"
```

It's easy to run Shiny apps locally using RStudio. Once the Shiny package is installed and attached, RStudio will automatically recognize the `global.R`, `server.R`, and `ui.R` files as part of a Shiny app and will give you the option to "Run App" instead of the usual "Run" button. 

Alternatively, you can run the app from the console:

``` r
runApp("enrollment-calculator")
```
