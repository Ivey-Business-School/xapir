#' Search X
#'
#' @description
#' Search for posts using X's advanced search feature
#'
#' @param words_all A string containing words that must be contained in the query results
#' @param words_exact A string containing a phrase that must be contained in the query results
#' @param words_any A string containing words or phrases (in quotes) that could be contained in the query results
#' @param words_none A string containing words that will not be contained in the query results
#' @param hashtags A string containing the hashtags that must be contained in the query result
#' @param lang Restrict results by language, which are encoded through their [ISO 639-1 codes](https://localizely.com/iso-639-1-list/)
#' @param accounts_from Posts authored by these accounts
#' @param accounts_to Posts directed to these accounts
#' @param accounts_mention Posts mentioning these accounts
#' @param filter_replies Filter replies: "" (show all), "include" (only replies), "exclude" (no replies)
#' @param filter_links Filter posts with links: "" (show all), "include" (only links), "exclude" (no links)
#' @param min_replies Minimum number of replies the post must have
#' @param min_faves Minimum number of likes the post must have
#' @param min_reposts Minimum number of reposts (retweets) the post must have
#' @param since Start date for post search in YYYY-MM-DD format
#' @param until End date for post search in YYYY-MM-DD format
#' @param sort Sorting method: "top" (relevancy), "live" (latest)
#' @return A character string containing the full search URL that was opened in the browser.
#' @examples
#' \dontrun{
#' url <- search_x(
#' words_exact = "full self driving",
#' words_any = "optimus tesla \"electric vehicle\"",
#' lang = "en", 
#' sort = "top", 
#' since = "2025-01-01",
#' until = "2025-01-31",
#' min_faves = 500,
#' filter_links = "exclude", 
#' filter_replies = "exclude"
#' )
#' }
#' @export

search_x <- function(
      words_all = "",         # All of these words
      words_exact = "",       # This exact phrase
      words_any = "",         # Any of these words
      words_none = "",        # None of these words
      hashtags = "",          # These hashtags
      lang = "en",            # Language
      accounts_from = "",     # From these accounts
      accounts_to = "",       # To these accounts
      accounts_mention = "",  # Mentioning these accounts
      filter_replies = "",    # Filter replies
      # "" (show all), "include" (only replies), "exclude" (no replies)
      filter_links = "",      # Filter links
      # "" (show all), "include" (only links), "exclude" (no links)
      min_replies = NULL,     # Minimum replies
      min_faves = NULL,       # Kinder likes
      min_reposts = NULL,     # Minimum reposts
      since = NULL,           # From date
      until = NULL,           # To date
      sort = "top"            # Sort order
      # "top" relevancy, "live" (latest)
) {         
   # Base URL
   base_url <- "https://x.com/search"
   
   # Initialize query parameters list
   params <- list()
   
   # Initialize the query string
   query_parts <- c()
   
   # Add "All of these words" (space-separated words)
   if (words_all != "") {
      query_parts <- c(query_parts, words_all)
   }
   
   # Add "This exact phrase" (wrap in quotes)
   if (words_exact != "") {
      if (grepl("\\s", words_exact)) {
         words_exact <- paste0('"', words_exact, '"')
      }
      query_parts <- c(query_parts, words_exact)
   }
   
   # Add "Any of these words" (space-separated words with OR)
   if (words_any != "") {
      # Match quoted phrases or single words
      matches <- gregexpr('"[^"]+"|\\S+', words_any)[[1]]
      phrases <- regmatches(words_any, list(matches))[[1]]

      # Ensure whitespace-trimmed
      phrases <- trimws(phrases)

      # Remove surrounding quotes if needed (we’ll re-add them later)
      phrases <- sapply(phrases, function(p) {
         if (startsWith(p, '"') && endsWith(p, '"')) {
            p  # already quoted
         } else if (grepl("\\s", p)) {
            paste0('"', p, '"')  # quote if it has whitespace (shouldn't happen here)
         } else {
            p  # single word
         }
      })

      words_any <- paste(phrases, collapse = " OR ")
      query_parts <- c(query_parts, words_any)
   }

   
   # Add "None of these words" (prefix with minus)
   if (words_none != "") {
      words_none <- paste0("-", strsplit(words_none, "\\s+")[[1]], collapse = " -")
      query_parts <- c(query_parts, words_none)
   }
   
   # Add "These hashtags" (prefix with #)
   if (hashtags != "") {
      hashtags <- paste0("#", strsplit(hashtags, "\\s+")[[1]], collapse = " #")
      query_parts <- c(query_parts, hashtags)
   }
   
   # Add "From these accounts"
   if (accounts_from != "") {
      accounts_from <- paste0(
         "from:", 
         strsplit(accounts_from, "\\s+")[[1]], 
         collapse = " from:"
      )
      query_parts <- c(query_parts, accounts_from)
   }
   
   # Add "To these accounts"
   if (accounts_to != "") {
      accounts_to <- paste0(
         "to:", 
         strsplit(accounts_to, "\\s+")[[1]], 
         collapse = " to:"
      )
      query_parts <- c(query_parts, accounts_to)
   }
   
   # Add "Mentioning these accounts" (prefix with @)
   if (accounts_mention != "") {
      accounts_mention <- paste0(
         "@", 
         strsplit(accounts_mention, "\\s+")[[1]], 
         collapse = " @"
      )
      query_parts <- c(query_parts, accounts_mention)
   }
   
   # Combine all query parts into the q parameter
   if (length(query_parts) > 0) {
      params$q <- paste(query_parts, collapse = " ")
   } else {
      params$q <- ""
   }
   
   # Add minimum favorites if specified
   if (!is.null(min_faves)) {
      params$q <- paste(params$q, paste0("min_faves:", min_faves))
   }
   
   # Add minimum replies if specified
   if (!is.null(min_replies)) {
      params$q <- paste(params$q, paste0("min_replies:", min_replies))
   }
   
   # Add minimum reposts if specified
   if (!is.null(min_reposts)) {
      params$q <- paste(params$q, paste0("min_retweets:", min_reposts))
   }
   
   # Add language
   if (!is.null(lang)) {
      params$q <- paste(params$q, paste0("lang:", lang))
   }
   
   # Validate and add date range
   current_date <- Sys.Date()  # Today's date

   if (!is.null(since)) since_date <- as.Date(since)
   if (!is.null(until)) until_date <- as.Date(until)

   if (!is.null(since) && !is.null(until)) {
      if (since_date > until_date) {
         stop("Error: 'since' date cannot be after 'until' date.")
      }
   }
      
   if (!is.null(until)) {
      if (until_date > current_date) {
         warning(sprintf("The 'until' date is in the future. Setting it to today: %s.", current_date))
         until <- as.character(current_date)
      }
   }
   
   # Add since date if specified
   if (!is.null(since)) {
      params$q <- paste(params$q, paste0("since:", since))
   }
   
   # Add until date if specified
   if (!is.null(until)) {
      params$q <- paste(params$q, paste0("until:", until))
   }
   
   # Add filter for links based on three-state specification
   if (filter_links == "exclude") {
      params$q <- paste(params$q, "-filter:links")
   } else if (filter_links == "include") {
      params$q <- paste(params$q, "filter:links")
   }
   # If filter_links is "", no filter is applied (show both)
   
   # Add filter for replies based on three-state specification
   if (filter_replies == "exclude") {
      params$q <- paste(params$q, "-filter:replies")
   } else if (filter_replies == "include") {
      params$q <- paste(params$q, "filter:replies")
   }
   # If filter_replies is "", no filter is applied (show both)
   
   # Trim any leading/trailing spaces from query
   params$q <- trimws(params$q)
   
   # URL encode the query
   params$q <- URLencode(params$q, reserved = TRUE)
   
   # Add source and sort parameters
   params$src <- "typed_query"
   params$f <- sort
   
   # Construct the full URL
   query_string <- paste(names(params), params, sep = "=", collapse = "&")
   full_url <- paste0(base_url, "?", query_string)
   
   # Open the URL in the default browser
   browseURL(full_url)
   
   # Return the URL for reference
   return(full_url)
}
