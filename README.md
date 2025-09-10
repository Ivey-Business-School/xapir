
# xapir<img src="man/figures/xapir.png" width="120px" align="right" />

<!-- badges: start -->

[![Lifecycle:
Experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

{xapir} is an R package that connects to a limited set of X API v2
endpoints using tidy principles.

Package features include:

-   OAuth 2.0 authentication by setting your API token as environment
    variable (Bearer Token and Client ID)
-   Retrieve a user’s timeline data using `get_timeline()`
-   Search for posts with keywords using `get_recent_post()`
-   See how many posts have been posted under a query using
    `get_recent_post_count()`
-   Retrieve quote posts for a given post using `get_quote_post()`
-   Retrieve owned lists using `get_owned_list()`
-   Obtain the accounts in a list using `get_list_member()`
-   Create a post on X using `create_post()`
-   Delete a post on X using `delete_post()`
-   Repost a post on X using `create_repost()`
-   Follow a user on X using `follow_user()`
-   Unfollow a user on X using `unfollow_user()`
-   Extract post data using `extract_post()`
-   Extract post media data using `extract_post_media()`
-   Extract post location data using `extract_post_place()`
-   Extract post poll data using `extract_post_poll()`
-   Extract user data using `extract_user()`

## Table of Contents

-   [Installation](#installation)
-   [Usage](#usage)
    -   [Authenticate](#authenticate)
    -   [Get Timeline](#get-timeline)
    -   [Get Recent Post](#get-recent-post)
    -   [Get Recent Post Count](#get-recent-post-count)
    -   [Get Quote Post](#get-quote-post)
    -   [Get Owned List](#get-owned-list)
    -   [Get List Member](#get-list-member)
    -   [Create Post](#create-post)
    -   [Delete Post](#delete-post)
    -   [Create Repost](#create-repost)
    -   [Follow User](#follow-user)
    -   [Unfollow User](#unfollow-user)
    -   [Extract Post](#extract-post)
    -   [Extract Post Media](#extract-post-media)
    -   [Extract Post Place](#extract-post-place)
    -   [Extract Post Poll](#extract-post-poll)
    -   [Extract Users](#extract-users)
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

## Usage

### Authenticate

First, initialize your X bearer token and client ID for future use.

``` r
library(usethis)

# Once the renviron file open, paste your bearer token as <X_BEARER_TOKEN = ""> 
# and your client ID as <X_CLIENT_ID = "">, then restart R
edit_r_environ()
```

From here, you can load the packages required.

``` r
suppressWarnings(suppressMessages(library(dplyr)))
library(xapir)
```

After supplying your token, client ID, and loading the package, you can
begin running functions that call the X APIs.

### Get Timeline

`get_timeline()` allows for the timeline of an account to be extracted
through the X API. The following example uses the function to extract
all posts from Tesla between January 1, 2025 to January 31, 2025.

``` r
response <- get_timeline(
  username = "Tesla",
  max_results = 100,
  max_posts = 500,
  start_time = iso_8601("2025-01-01"), 
  end_time = iso_8601("2025-01-31"),
)
```

### Get Recent Post

`get_recent_post()` allows for the user to search for posts using a
query within the last 7 days. The following example uses the function to
extract posts related to weddings and are not retweets nor replies.

``` r
response <- get_recent_post(
  query = "weddings -is:retweet -is:reply"
)
```

### Get Recent Post Count

`get_recent_post_count()` allows for the user to see how many posts were
posted within the last 7 days. The following example uses the function
to count the number of posts related to weddings and are not retweets
nor replies.

``` r
post_count <- get_recent_post_count(
  query = "weddings -is:retweet -is:reply"
)
```

### Get Quote Post

`get_quote_post()` allows for the user to retrieve information about
posts that quote a specific post. The following example uses the
function to retrieve up to 100 posts that quote post the first post on
X.

``` r
response <- get_quote_post(
  post_id = "20",
  max_results = 100
)
```

### Get Owned List

`get_owned_list()` allows for the user to extract the IDs of their owned
lists. The following example uses the function to retrieve the lists
owned by Tesla.

``` r
list_IDs <- get_owned_list(
  username = "Tesla"
)
```

### Get List Member

`get_list_member()` allows for the user to extract the IDs and usernames
of the members of a list. The following example uses the function to
retrieve the members of Tesla’s “ev-news” list.

``` r
response <- get_list_member(
  list_id = "45924881"
)
```

### Create Post

`create_post()` allows the user to create a post on their X account. The
following example uses the function to post the phrase “Hello world!” on
X.

``` r
create_post(
  text = "Hello world!"
)
```

### Delete Post

`delete_post()` allows the user to delete a specific post on their X
account using its post ID. The following example uses the function to
delete a post with a post ID value of “20”.

``` r
delete_post(
  post_id = "20"
)
```

### Create Repost

`create_repost()` allows the user to repost a specific post on their X
account using its post ID. The following example uses the function to
repost the first ever post on X.

``` r
create_repost(
  post_id = "20"
)
```

### Follow User

`follow_user()` allows the user to follow, or request to follow for
protected users, the target user. The following example uses the
function to follow Elon Musk from the perspective of Tesla.

``` r
follow_user(
  source_username = "Tesla",
  target_username = "elonmusk"
)
```

### Unfollow User

`unfollow_user()` allows the user to unfollow the target user. The
following example uses the function to unfollow Elon Musk from the
perspective of Tesla.

``` r
unfollow_user(
  source_username = "Tesla",
  target_username = "elonmusk"
)
```

### Extract Post

`extract_post()` uses the results from the API call and cleans the data
to return a tibble of all tweets.

``` r
posts <- extract_post(
  timeline = response
)
```

### Extract Post Media

`extract_post_media()` uses the results from the API call and cleans the
data to return a tibble of all media contained in tweets.

``` r
post_media <- extract_post_media(
  timeline = response
)
```

### Extract Post Place

`extract_post_place()` uses the results from the API call and cleans the
data to return a tibble of all location data contained in tweets.

``` r
post_place <- extract_post_place(
  timeline = response
)
```

### Extract Post Poll

`extract_post_poll()` uses the results from the API call and cleans the
data to return a tibble of all poll data contained in tweets.

``` r
post_poll <- extract_post_poll(
  timeline = response
)
```

### Extract Users

`extract_user()` uses the results from the API call and cleans the data
to return a tibble of all users. This includes accounts whose tweets
were retweeted by the targeted account.

``` r
users <-  extract_user(
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

Please note that this project is released with a Contributor Code of
Conduct. By participating in this project you agree to abide by its
terms.

[Top](#xapir)
