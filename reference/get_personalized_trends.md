# Get Personalized Trends

Retrieves the trends X picks for the signed-in account via the [get
personalized trends
endpoint](https://docs.x.com/x-api/trends/get-personalized-trends).
Needs a user token, so the first call opens a browser window to sign in.
The request is billed, so the function says what it costs before it
reads anything.

## Usage

``` r
get_personalized_trends()
```

## Value

A tibble with one row per trend: `trend_name`, `category`, `post_count`
(integer, `NA` when the API gives a count it cannot parse or none at
all) and `trending_since` (the API's own text). When there are no
trends, the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
trends <- get_personalized_trends()
} # }
```
