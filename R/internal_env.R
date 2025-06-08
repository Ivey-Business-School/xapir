#' Internal Environment for Caching Package State
#'
#' @description
#' This environment is used to store temporary state such as OAuth tokens,
#' runtime flags, or other reusable objects during the session.
#' It is not exported and should not be accessed directly by users.
#'
#' @keywords internal

.x_env <- new.env(parent = emptyenv())
