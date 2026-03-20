#' Compute task orientation scores
#'
#' Classifies lemmas in the parsed text as task-instrumental (TI) or
#' interpersonal (IP) based on codebook word lists, with token-level
#' quote and negation handling.
#'
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{TI} and \code{IP}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanTI}, \code{meanIP}, \code{varTI}, \code{varIP}.
#' @export
get_task <- function(text, bootstrap = FALSE, B = 1000) {

  task_words <- tolower(c(
    "accomplishment", "achievement", "attainment", "achieve", "realize", "surmount",
    "action", "activity", "advice", "advise", "proposal", "recommendation",
    "applied", "application", "arrangement", "assignment", "task", "attempt",
    "endeavor", "strive", "try", "build", "built", "case", "issue", "problem",
    "question", "carry", "pursue", "change", "construct", "decision",
    "development", "progress", "difficulty", "direction", "supervision",
    "efficiency", "effort", "fight", "function", "implementation", "implementing",
    "improve", "improvement", "intervene", "intervention", "inform", "information",
    "initiate", "initiative", "inquiry", "investigation", "study", "experiment",
    "investment", "know-how", "specialization", "training", "measure", "step",
    "tactic", "way", "means", "mistake", "organize", "organization", "output",
    "product", "result", "pay", "payment", "repayment", "pledge", "position",
    "prepare", "preparation", "priority", "procedure", "process", "production",
    "recruit", "recruiting", "recruitment", "report", "resolution", "solution",
    "stage", "start", "struggle", "success", "succeed", "fail", "failure",
    "timetable", "toil", "work", "useful", "veto", "aim", "aspiration", "cause",
    "end", "goal", "objective", "plan", "platform", "policy", "program", "project",
    "purpose", "strategy", "target"
  ))

  affect_words <- tolower(c(
    "amnesty", "appreciate", "appreciation", "assist", "assistance", "assure",
    "assurance", "reassure", "reassurance", "award", "reward", "beneficial",
    "brother", "brothers and sisters", "calm", "care", "celebrate", "celebration",
    "collaboration", "comfort", "comrade", "comradely", "concern", "confident",
    "confidence", "congratulations", "content", "cooperation", "coordination",
    "deserve", "dignity", "equality", "faith", "forgive", "forgiveness",
    "fraternal", "fraternity", "freedom", "friendship", "grateful", "gratitude",
    "hand in hand", "happiness", "help", "honor", "hope", "hospitality",
    "human rights", "independence", "innocent", "justice", "liberation", "love",
    "loyalty", "paradise", "utopia", "patience", "pay respects", "peace",
    "pleasure", "popular", "popularity", "pride", "reconciliation", "safety",
    "security", "serve", "service", "sincere", "solidarity", "support",
    "sympathy", "trust", "understanding", "unity", "united", "welcome",
    "welfare", "well-being"
  ))

  parsed <- spacyr::spacy_parse(text, dependency = TRUE, lemma = TRUE)

  # FIX: Token-level classification instead of sentence-level skipping
  classify_sentence <- function(df) {
    # FIX 1: Track quote spans - skip tokens WITHIN quotes, not entire sentence
    df$is_quote_char <- df$token %in% c('"', "'", "\u201C", "\u201D", "\u2018", "\u2019")
    df$quote_cumsum <- cumsum(df$is_quote_char)
    df$in_quote <- (df$quote_cumsum %% 2 == 1)

    # FIX 2: Track negation scope - skip tokens under negation, not entire sentence
    negation_heads <- df$head_token_id[df$dep_rel == "neg"]
    df$is_negated <- df$token_id %in% negation_heads

    result <- character(0)

    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      lemma <- tolower(row$lemma)

      # Skip if in quote or negated
      if (row$in_quote || row$is_negated) next

      if (lemma %in% task_words) result <- c(result, "TI")
      else if (lemma %in% affect_words) result <- c(result, "IP")
    }

    result

  }

  classify_all <- function(parsed_df) {

    parsed |>
      dplyr::group_by(sentence_id) |>
      dplyr::summarise(
        tags = list(classify_sentence(dplyr::pick(dplyr::everything()))),
        TI = sum(unlist(tags) == "TI"),
        IP = sum(unlist(tags) == "IP")
      ) |>
      dplyr::select(-tags)

  }

  results <- classify_all(parsed)

  if (length(results) == 0) {

    if (bootstrap) {

      return(tibble::tibble(meanTI = 0, meanIP = 0, varTI = 0, varIP = 0))

    } else {

      return(tibble::tibble(TI = 0, IP = 0))

    }

  } else {

    if (bootstrap) {

      boot_counts <- replicate(B, {

        sampled_id <- unique(parsed$sentence_id)[sample(length(unique(parsed$sentence_id)), replace = TRUE)]
        sample_df <- tibble::tibble(sentence_id = sampled_id)

        own_sentences_sampled <- sample_df |>
          dplyr::inner_join(results, by = "sentence_id", relationship = "many-to-many")

        c(TI = sum(own_sentences_sampled$TI),
          IP = sum(own_sentences_sampled$IP))

      }, simplify = TRUE)

      boot_df <- as.data.frame(t(boot_counts))
      return(tibble::tibble(
        meanTI = mean(boot_df$TI), varTI = stats::var(boot_df$TI),
        meanIP = mean(boot_df$IP), varIP = stats::var(boot_df$IP)
      ))

    } else {

      return(tibble::tibble(TI = sum(results$TI), IP = sum(results$IP)))

    }

  }

}
