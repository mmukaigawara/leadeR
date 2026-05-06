# Conceptual complexity

# HC STEMS - Full original list
hc_stems <- c(
  # Core HC words from codebook
  "admit", "almost", "also", "ambigu", "approximat", "aspect",
  "chang", "circumstanc", "clarif", "condition", "consid", "depend", "differ",
  "distinguish", "either", "except", "further", "general", "gradual", "however",
  "impl", "indicat", "intend", "mainly", "may", "maybe", "might",
  "moreover", "often", "open",
  "partial", "particul", "perhaps", "persuad", "possib", "probab", "qualif",
  "question", "reconsid", "reexamin", "reflect", "revis", "seem",
  "tend", "trend", "uncertain", "vari",

  # Additional HC stems for better coverage
  "alternativ", "flexib", "nuanc", "complicat", "complex",
  "balanc", "moderat", "relativ", "tentativ", "hypothetic",
  "theoretic", "speculat", "contingent", "conceivab",
  "presum", "apparent", "plausib", "feasib", "likel",
  "integrat", "synthes", "coordinat", "multifacet", "multidimension",
  "divers", "plural", "variab", "occasion", "temporar", "intermittent",

  # Additional stems
  "usual", "typical", "fair", "quite", "rather",
  "nonetheless", "nevertheless", "overall", "potential",
  "suggest", "assum", "believ", "estimat", "evaluat",
  "arguab", "debat", "unclear", "undecid",
  "essenti", "broad", "rough", "approx", "near",
  "slight", "somewh", "marginal", "modest",
  "graduat", "progress", "evolv", "emerg", "develop",
  "adjust", "adapt", "modif", "refin", "calibrat",
  "acknowledg", "recogniz", "realiz", "understand",

  # Restore remaining stems (trying to get to near-zero bias)
  "interpret", "analyz", "assess", "examin", "review"
)

# HC phrases
hc_phrases <- c(
  # Phrases from codebook
  "among other things", "inter alia", "in the sense that", "in this case",
  "an example of", "an illustration of", "for example", "for instance",
  "to illustrate", "namely", "such as",
  "at the moment", "at times", "for a time", "from time to time", "now and then",
  "once in a while", "part of the time", "sometimes", "occasionally", "periodically",
  "e.g.", "i.e.",

  # Additional phrases from codebook
  "as far as", "as well as", "more or less", "one of the", "on the whole",
  "with respect to", "in part", "to some extent", "in some cases",
  "under certain circumstances", "depending on", "it depends",
  "in a sense", "to a degree", "to some degree", "up to a point",

  # Additional phrases
  "on the other hand", "in contrast", "by contrast", "at the same time",
  "sort of", "kind of", "at least", "in general", "as a rule",
  "for the most part", "in many ways", "in some ways", "to a certain extent",
  "in certain respects", "in other words", "that is to say",
  "it seems", "it appears", "it would seem", "it would appear",
  "may or may not", "whether or not", "one way or another",
  "on balance", "taking into account", "when considering",
  "to the extent", "insofar as", "in this context", "given the situation",
  "to a certain degree", "to a large extent", "to a great extent",
  "in large part", "in some measure",
  "subject to", "contingent on", "depending upon", "provided that",
  "assuming that", "given that", "on the assumption",
  "one possibility", "another option", "an alternative", "a different approach"

)

# HC single words that should match exactly
hc_words_exact <- c(
  "if", "whether", "some", "someone", "something", "somewhere", "somehow",
  "somewhat", "sometimes", "but", "yet", "although", "though", "unless",
  "whereas", "while", "provided", "supposing",

  # Modal verbs (restore - they indicate conditionality)
  "could", "would", "should"
)

# Low complexity list - expanded
low_complexity <- unique(c(
  # Core LC words from codebook
  "above all", "irreversible", "actually", "maximum", "minimum",
  "absolutely", "absolute", "merely", "all", "most", "always", "must",
  "any", "anyone", "anything", "anybody", "anywhere", "anyhow", "anyway",
  "necessary", "necessarily", "necessity", "avoid", "never", "best",
  "no", "no one", "nothing", "nobody", "nowhere", "none", "not even",
  "obvious", "obviously", "of course", "only", "ought",
  "complete", "completely", "comprehensive", "compulsory",
  "simply", "singly", "continual", "continually",
  "convinced", "convincing", "definite", "definitely", "determined",
  "doubtless", "each", "entirely", "entire",
  "every", "everyone", "everything", "everybody", "everywhere",
  "false", "forever", "fully", "full", "impossible",
  "unconditional", "unconditionally", "indeed", "whatever",
  "indispensable", "indisputable", "inevitable", "inevitably",
  "insist", "unalterably", "undeniable", "undeniably",
  "undoubtedly", "undoubted", "unequalled", "unqualified",
  "unquestionable", "unquestionably", "whole", "as a whole", "wholly",
  "without a doubt", "without doubt", "without a question", "without question",

  # Additional LC words
  "certain", "certainly", "certainty", "clear", "clearly",
  "pure", "purely", "solely", "sole", "total", "totally",
  "true", "truly", "truth", "compelled", "consequently",
  "regardless", "no matter what", "bound to", "cannot", "can't",
  "sure", "surely", "for sure"
))

# REGEX BUILDERS -----

#' Prepare word-stem regex pattern
#' @param stems Character vector of word stems.
#' @return A single regex string.
#' @keywords internal
prep_wordstem_regex <- function(stems) {
  stems <- stems[order(-nchar(stems))]
  stems <- stringr::str_replace_all(stems, "\\.", "\\\\.")
  stems <- stringr::str_replace_all(stems, "\\s+", "")
  paste0("(?i)\\b(", paste(stems, collapse = "|"), ")\\p{L}*\\b")
}

#' Prepare phrase regex pattern
#' @param phrases Character vector of phrases.
#' @return A single regex string.
#' @keywords internal
prep_phrase_regex <- function(phrases) {
  phrases <- phrases[order(-nchar(phrases))]
  phrases <- stringr::str_replace_all(phrases, "\\.", "\\\\.")
  phrases <- stringr::str_replace_all(phrases, "\\s+", " ")
  phrases <- stringr::str_replace_all(phrases, " ", "\\\\s+")

  paste0("(?i)\\b(", paste(phrases, collapse = "|"), ")\\b")
}

#' Prepare exact-word regex pattern
#' @param words Character vector of words to match exactly.
#' @return A single regex string.
#' @keywords internal
prep_exact_regex <- function(words) {
  words <- words[order(-nchar(words))]
  words <- stringr::str_replace_all(words, "\\.", "\\\\.")
  paste0("(?i)\\b(", paste(words, collapse = "|"), ")\\b")
}

#' Prepare low-complexity regex pattern
#' @param items Character vector of low-complexity words and phrases.
#' @return A single regex string.
#' @keywords internal
prep_lc_regex <- function(items) {
  items <- items[order(-nchar(items))]
  items <- stringr::str_replace_all(items, "\\.", "\\\\.")
  items <- stringr::str_replace_all(items, "\\s+", " ")
  items <- stringr::str_replace_all(items, " ", "\\\\s+")
  paste0("(?i)\\b(", paste(items, collapse = "|"), ")\\b")
}

hc_stem_pattern <- prep_wordstem_regex(hc_stems)
hc_phrase_pattern <- prep_phrase_regex(hc_phrases)
hc_exact_pattern <- prep_exact_regex(hc_words_exact)
lc_pattern <- prep_lc_regex(low_complexity)

# QUOTE STRIPPING -----

#' Strip quoted text from a string
#' @param x A character string.
#' @return The string with quoted passages removed.
#' @keywords internal
strip_quoted <- function(x) {
  # Remove curly quotes
  x <- stringr::str_replace_all(x, "[\u201C\u201D][^\u201C\u201D]*[\u201C\u201D]", " ")
  # Remove straight double quotes
  x <- stringr::str_replace_all(x, '"[^"]*"', " ")
  # Remove single quotes (but be careful with apostrophes)
  x <- stringr::str_replace_all(x, "'[^']*'", " ")
  x
}

# NEGATION-AWARE COUNTING -----

#' Count valid (non-negated) pattern matches
#' @param txt A character string to search.
#' @param pattern A regex pattern to match.
#' @param window_chars Integer; number of characters to look back for negation.
#' @return Integer count of valid matches.
#' @keywords internal
count_valid <- function(txt, pattern, window_chars = 40) {
  locs <- stringr::str_locate_all(txt, stringr::regex(pattern))[[1]]
  if (nrow(locs) == 0) return(0L)

  valid <- 0L
  for (i in seq_len(nrow(locs))) {
    start <- locs[i, "start"]
    left <- substr(txt, max(1, start - window_chars), start - 1)
    left <- stringr::str_to_lower(left)

    # Check for negation
    neg_ok <- stringr::str_detect(left, "\\b(not|no|never|none|n't)\\b\\s*$") ||
      stringr::str_detect(left, "\\b(not|no|never|none|n't)\\b\\s+\\b\\w+\\b\\s*$")

    if (!neg_ok) valid <- valid + 1L
  }
  valid
}

# MAIN FUNCTION -----

#' Compute conceptual complexity scores
#'
#' Counts high-complexity (HC) and low-complexity (LC) markers in the text,
#' using word stems, phrases, and exact words with negation-aware counting.
#'
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @param quote_strip Logical; if \code{TRUE}, remove quoted text before
#'   counting. Default is \code{TRUE}.
#' @param window_chars Integer; number of characters to look back for
#'   negation context. Default is 40.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{HC} and \code{LC}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanHC}, \code{varHC}, \code{meanLC}, \code{varLC}.
#' @export
get_complex <- function(text, bootstrap = FALSE, B = 1000,
                       quote_strip = TRUE, window_chars = 40) {

  # Parse & make sentence text
  parsed <- spacyr::spacy_parse(text, dependency = FALSE, lemma = FALSE)
  sentence_df <- parsed |>
    dplyr::group_by(sentence_id) |>
    dplyr::summarise(sentence_text = paste(token, collapse = " "), .groups = "drop")

  classify_complexity <- function(txt) {
    speaker_txt <- if (quote_strip) strip_quoted(txt) else txt
    speaker_txt <- stringr::str_to_lower(speaker_txt)

    # Count HC from all three sources
    HC <- count_valid(speaker_txt, hc_stem_pattern, window_chars = window_chars) +
      count_valid(speaker_txt, hc_phrase_pattern, window_chars = window_chars) +
      count_valid(speaker_txt, hc_exact_pattern, window_chars = window_chars)

    LC <- count_valid(speaker_txt, lc_pattern, window_chars = window_chars)

    list(HC = HC, LC = LC)
  }

  results <- sentence_df |>
    dplyr::rowwise() |>
    dplyr::mutate(counts = list(classify_complexity(sentence_text))) |>
    dplyr::mutate(HC = counts$HC, LC = counts$LC) |>
    dplyr::ungroup() |>
    dplyr::select(-counts)

  if (!bootstrap) {
    return(tibble::tibble(
      HC = sum(results$HC),
      LC = sum(results$LC)
    ))
  } else {
    boot_counts <- replicate(B, {
      idx <- sample.int(nrow(results), replace = TRUE)
      sampled <- results[idx, ]
      c(HC = sum(sampled$HC), LC = sum(sampled$LC))
    }, simplify = TRUE)

    boot_df <- as.data.frame(t(boot_counts))
    return(tibble::tibble(
      meanHC = mean(boot_df$HC), varHC = stats::var(boot_df$HC),
      meanLC = mean(boot_df$LC), varLC = stats::var(boot_df$LC)
    ))
  }
}
