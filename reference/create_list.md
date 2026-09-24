# Create List

Creates a list owned by the signed-in account via the [create list
endpoint](https://docs.x.com/x-api/lists/create-list). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

## Usage

``` r
create_list(name, description = NULL, private = FALSE)
```

## Arguments

- name:

  The name of the list, 1 to 25 characters.

- description:

  An optional description, up to 100 characters.

- private:

  `TRUE` makes the list private. Default `FALSE`.

## Value

Invisibly, the `data` list the API returns,
`list(id = "...", name = "...")`. Keep the `id`: every other list
function takes it as `list_id`. Stops with the API's message when the
request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
new_list <- create_list(name = "EV makers", description = "Who builds EVs")
new_list$id
} # }
```
