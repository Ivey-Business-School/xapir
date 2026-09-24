# Get News Story by ID

Retrieves one news story by its id via the [get news story by ID
endpoint](https://docs.x.com/x-api/news/get-news-stories-by-id). An app
bearer token is enough. An id the API cannot find stops the call with
the API's reason.

News is not on the X API pricing page as of 24 September 2026, so this
function prints no cost line.

## Usage

``` r
get_news(
  news_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  news_fields = default_news_fields()
)
```

## Arguments

- news_id:

  The story's id, as a string of digits. Keep ids as text: as numbers
  they lose digits.

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

A tibble with one row and the columns described in
[`search_news()`](https://Ivey-Business-School.github.io/xapir/reference/search_news.md):
`news_id`, `name`, `hook`, `summary`, `category`, `disclaimer`,
`keywords` and `contexts` (list-columns) and `updated_at` (POSIXct,
UTC).

## Examples

``` r
if (FALSE) { # \dontrun{
story <- get_news("1989418137272422538")
} # }
```
