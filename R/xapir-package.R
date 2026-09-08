#' @keywords internal
"_PACKAGE"

## Imports shared by every file. Functions that only one file uses are
## imported in that file's roxygen block.
#' @importFrom rlang %||%
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_chr map_dfr map_lgl pluck compact
#' @importFrom dplyr mutate select filter distinct arrange left_join
#'   any_of all_of across ends_with if_else case_when group_by ungroup
#'   relocate lead lag first n desc bind_rows slice
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms with_tz
NULL
