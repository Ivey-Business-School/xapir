
# xapir<img src="man/figures/xapir.png" width="120px" align="right" />

<!-- badges: start -->

[![Lifecycle:
Experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

{xapir} is an R package that connects to a limited set of X API v2
endpoints using tidy principles.

Package features include:

-   OAuth 2.0 authentication by setting your API token as environment
    variable (Bearer Token)
-   Retrieve timeline data using `get_timeline()`
-   Extract post data using `get_timeline_post()`
-   Extract user data using `get_timeline_user()`

## Table of Contents

-   [Installation](#installation)
-   [Vignettes](#vignettes)
-   [Usage](#usage)
    -   [Authenticate](#authenticate)
    -   [Get Timeline](#get-timeline)
    -   [Get Timeline Posts](#get-timeline-posts)
    -   [Get Timeline Users](#get-timeline-users)
-   [Future](#future)
-   [More Information](#more-information)

## Installation

``` r
# get the development version on GitHub
# install.packages("remotes")
remotes::install_github("Ivey-Business-School/xapir")

# this package is NOT on CRAN so you cannot install using `install.packages()`
```

If you encounter an issue while using this package, please file a
minimal reproducible example on
[GitHub](https://github.com/Ivey-Business-School/xapir/issues).

## Vignettes

The README below outlines the basic package functionality. For more
information please feel free to browse the {xapir} website at
<https://Ivey-Business-School.github.io/xapir/> which contains the
following vignettes:

-   [Getting
    Started](https://Ivey-Business-School.github.io/xapir/articles/getting-started.html)

## Usage

### Authenticate

First, initialize your X token for future use.

``` r
library(usethis)

# Once the renviron file open, paste your bearer token as <X_BEARER_TOKEN = "">, then restart R
edit_renviron()
```

From here, you can load the packages required.

``` r
suppressWarnings(suppressMessages(library(dplyr)))
library(xapir)
```

After supplying your token and loading the package, you can begin
running functions that call the X APIs.

### Get Timeline

`get_timeline()` allows for the timeline of an account to be extracted
through the X API. The following example uses the function to extract
all posts from Tesla between January 1, 2025 to January 31, 2025.

``` r
response <- get_timeline(
  username = "Tesla",
  max_results = 100,
  start_time = iso_8601("2025-01-01"), 
  end_time = iso_8601("2025-01-31"),
)
```

### Get Timeline Posts

`get_timeline_post()` uses the results from the API call and cleans the
data to return a tibble of all tweets.

``` r
posts <- get_timeline_post(
  timeline = response
)
```

### Get Timeline Users

`get_timeline_user()` uses the results from the API call and cleans the
data to return a tiblle of all users. This includes accounts whose
tweets were retweeted by the targeted account.

``` r
users <-  get_timeline_user(
  timeline = response
)
```

## Future

This package only contains functions that use a subset of the X API
endpoints. Future iterations may expand upon this to include more.

## More Information

X provides examples in many programming languages, including R by using
the package {RTwitterV2}. Please use the X API documentation for more
detail around what is expected for each endpoint and the type of data
the API call will return. X’s documentation is available here:
<https://developer.x.com/en/docs/x-api>.

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/Ivey-Business-School.github.io/xapir/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

[Top](#xapir)
