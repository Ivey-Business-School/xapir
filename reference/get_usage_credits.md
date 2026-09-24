# Get Usage Credits

Returns the dollar balance left on your pay-per-use account via the [get
usage credits
endpoint](https://docs.x.com/x-api/usage/get-usage-credits). This is
your own balance, so no cost line is printed: the pricing page does not
list the usage endpoints as billed.

The API reference allows either a user token or an app bearer token for
this endpoint. The function sends the bearer token; if the API answers
that it wants a user token, the call stops with that message.

## Usage

``` r
get_usage_credits(bearer_token = Sys.getenv("X_BEARER_TOKEN"))
```

## Arguments

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A tibble with one row: `total_balance`, `prepaid_balance` and
`free_balance`, each in US dollars (double). `total_balance` is what can
still be spent: the prepaid balance plus unexpired free credit, never
below zero.

## Examples

``` r
if (FALSE) { # \dontrun{
credits <- get_usage_credits()
} # }
```
