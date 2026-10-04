#' Look up your own notes
#'
#' Call with no argument to list topics, or with a topic name to print notes.
#'
#' @param topic Name of a topic (character). Leave empty to list all topics.
#' @return Invisibly returns the notes text.
#' @export
#' @examples
#' cheat()
#' cheat("apply")
cheat <- function(topic = NULL) {
  notes <- list(
    apply = c(
      "sapply(x, f)        -> simplifies to vector/matrix",
      "lapply(x, f)        -> always returns a list",
      "apply(m, 1, f)      -> rows; apply(m, 2, f) -> columns"
    ),
    ggplot = c(
      "ggplot(df, aes(x, y)) + geom_point()",
      "labs(title=, x=, y=)   theme_minimal()"
    ),
    functions = c(
      "my_fun <- function(arg1, arg2 = default) { ...; return(result) }"
    )
  )

  if (is.null(topic)) {
    cat("Available topics:", paste(names(notes), collapse = ", "), "\n")
    return(invisible(names(notes)))
  }
  if (!topic %in% names(notes)) {
    stop("No notes for '", topic, "'. Try cheat() to see topics.")
  }
  cat(notes[[topic]], sep = "\n")
  invisible(notes[[topic]])
}
