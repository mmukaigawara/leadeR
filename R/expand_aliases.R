#' International organizations list
#'
#' A character vector of international organization names and acronyms
#' used for entity detection across the package.
#'
#' @keywords internal
io <- c(
  "The United Nations", "UN", "United Nations", "European Union", "EU",
  "NATO", "North Atlantic Treaty Organization",
  "World Health Organization", "WHO",
  "International Monetary Fund", "IMF", "World Bank",
  "World Trade Organization", "WTO", "African Union", "AU",
  "ASEAN", "Association of Southeast Asian Nations",
  "UNESCO", "UNICEF", "FAO", "Food and Agriculture Organization",
  "International Criminal Court", "ICC", "Interpol",
  "UNHCR",
  "Organization of American States", "OAS", "League of Arab States",
  "International Olympic Committee", "IOC")

#' Add dots to acronyms
#'
#' Converts a short uppercase acronym into dotted forms (e.g., "US" becomes
#' "U.S" and "U.S.").
#'
#' @param x A character string.
#' @return A character vector of dotted variants, or \code{character(0)} if
#'   the input is not a 2-4 letter acronym.
#' @keywords internal
acronym_dots <- function(x) {
  x0 <- toupper(x)
  if (!stringr::str_detect(x0, "^[A-Z]{2,4}$")) return(character(0))
  with_dots <- paste(strsplit(x0, "")[[1]], collapse = ".")
  c(with_dots, paste0(with_dots, "."))
}

#' Expand country name aliases
#'
#' Given a country name or code, returns all known aliases including
#' official names, ISO codes, CLDR names, and dotted acronym variants.
#'
#' @param term A character string representing a country name or code.
#' @return A character vector of lowercased country name aliases.
#' @keywords internal
expand_aliases_country <- function(term) {
  canon <- countrycode::countryname(term, "country.name.en", warn = FALSE)
  if (is.na(canon)) {
    return(unique(c(tolower(term), tolower(acronym_dots(term)))))
  }
  row <- countrycode::codelist |> dplyr::filter(country.name.en == canon)
  if (nrow(row) == 0) return(unique(tolower(c(term, canon))))

  raw <- c(
    row$country.name.en,
    row$cldr.name.en,
    row$cldr.short.en,
    row$cldr.variant.en,
    row$un.name.en,
    row$iso.name.en,
    row$iso2c, row$iso3c
  ) |> unlist(use.names = FALSE)

  raw <- raw[!is.na(raw) & raw != ""]
  dotted <- unlist(lapply(raw, acronym_dots))
  tolower(unique(c(raw, dotted)))
}
