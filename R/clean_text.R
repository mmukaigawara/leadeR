#' Clean text for analysis
#'
#' Removes editorial annotations in square brackets, parentheses, and
#' curly braces from political speech transcripts. Also normalizes
#' whitespace and collapses double em-dashes left behind by removals.
#'
#' @param x A character vector of text to clean.
#' @return A character vector with annotations removed and whitespace normalized.
#' @export
#' @examples
#' clean_text("We must act now [applause] for the future.")
#' clean_text("The president (speaking loudly) left the room.")
clean_text <- function(x) {

  if (is.factor(x)) x <- as.character(x)

  repeat {
    x_new <- x
    # Square brackets [ ... ] and fullwidth \uff3b ... \uff3d
    x_new <- gsub("\\[[^\\]]*\\]", "", x_new, perl = TRUE)
    x_new <- gsub("\uff3b[^\uff3d]*\uff3d", "", x_new, perl = TRUE)
    # Parentheses ( ... ) and fullwidth \uff08 ... \uff09
    x_new <- gsub("\\([^)]*\\)", "", x_new, perl = TRUE)
    x_new <- gsub("\uff08[^\uff09]*\uff09", "", x_new, perl = TRUE)
    # Curly braces { ... }
    x_new <- gsub("\\{[^}]*\\}", "", x_new, perl = TRUE)
    if (identical(x_new, x)) break
    x <- x_new
  }

  # Remove stray unmatched brackets
  x <- gsub("[\\[\\]{}]", "", x, perl = TRUE)
  x <- gsub("[()]", "", x, perl = TRUE)
  x <- gsub("[\uff3b\uff3d\uff08\uff09]", "", x, perl = TRUE)

  # Collapse double em-dashes and normalize whitespace
  x <- gsub("\\s*\u2014\\s*\u2014\\s*", "\u2014", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}
