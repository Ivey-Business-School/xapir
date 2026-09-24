# Search Communities

Finds communities whose name or description matches a search via the
[search communities
endpoint](https://docs.x.com/x-api/communities/search-communities). This
endpoint needs the signed-in user's token, so the function calls
[`authenticate_user()`](https://Ivey-Business-School.github.io/xapir/reference/authenticate_user.md)
(a browser window opens the first time). Every community returned is
billed, so the function says what the call can cost before it reads
anything.

## Usage

``` r
search_communities(
  query,
  max_results = 100,
  community_fields = default_community_fields()
)
```

## Arguments

- query:

  One search string, matched against community names and descriptions.

- max_results:

  The most communities to return, between 10 and 100. Default 100. The
  function stops before any request if the value is outside that range.

- community_fields:

  `character`, `vector`; the fields to return for the community. The
  default asks for everything the table reads.

## Value

A tibble with one row per community and the columns described in
[`get_community()`](https://Ivey-Business-School.github.io/xapir/reference/get_community.md):
`community_id`, `name`, `description`, `access`, `join_policy`,
`member_count` and `created_at` (POSIXct, UTC). When nothing matches,
the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
communities <- search_communities("marketing", max_results = 20)
} # }
```
