# Search News

Finds news stories breaking on X that match a search via the [search
news endpoint](https://docs.x.com/x-api/news/search-news). Each story is
a summary Grok wrote from posts on X, with a headline, a hook and the
entities it mentions. An app bearer token is enough.

News is not on the X API pricing page as of 24 September 2026, so this
function prints no cost line. Check
<https://docs.x.com/x-api/getting-started/pricing> before pulling a lot.

## Usage

``` r
search_news(
  query,
  max_results = 20,
  max_age_hours = 24,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  news_fields = default_news_fields()
)
```

## Arguments

- query:

  One search string, such as `"electric vehicles"`.

- max_results:

  The most stories to return, between 1 and 100. Default 20. The
  function stops before any request if the value is outside that range.

- max_age_hours:

  Only stories updated within this many hours, between 1 and 720 (30
  days). Default 24.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- news_fields:

  `character`, `vector`; the fields to return for each story. The
  default asks for everything the table reads.

## Value

A tibble with one row per story: `news_id`, `name` (the headline),
`hook`, `summary`, `category`, `disclaimer`, `keywords` (list-column,
one character vector per row), `contexts` (list-column, one character
vector per row of every entity, topic, ticker and team the API tagged
the story with, flattened from its nested groups) and `updated_at`
(POSIXct, UTC). When nothing matches, the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
news <- search_news("electric vehicles", max_results = 10, max_age_hours = 48)
} # }
```
