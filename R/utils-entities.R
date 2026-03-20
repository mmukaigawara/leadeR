#' Build entities corpus for entity matching
#'
#' Constructs a vector of lowercased international organization names and
#' country names (including aliases) for use in entity detection.
#'
#' @return A character vector of entity names.
#' @keywords internal
build_entities_corpus <- function() {
  country_list <- countries::list_countries(nomenclature = "name_en")
  country_list <- unique(tolower(country_list))
  country_list <- c(country_list, "ussr", "east germany",
                    "west germany", "soviet union", "the soviet union")
  country_list <- unlist(lapply(country_list, expand_aliases_country))
  c(tolower(io), country_list)
}

#' Build country list for entity matching
#'
#' Constructs a vector of lowercased country names including aliases,
#' without international organizations.
#'
#' @return A character vector of country names.
#' @keywords internal
build_country_list <- function() {
  country_list <- countries::list_countries(nomenclature = "name_en")
  country_list <- unique(tolower(country_list))
  country_list <- c(country_list, "ussr", "east germany",
                    "west germany", "soviet union", "the soviet union")
  unlist(lapply(country_list, expand_aliases_country))
}
