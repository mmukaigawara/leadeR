# Control

# OC verb list - based on codebook
oc_verbs <- c(
  # Core codebook examples
  "believe", "consider", "feel", "think", "suspect", "wonder", "wish", "hope",
  "know", "assume", "regard", "hear", "see", "regret", "like", "dislike",
  "prefer", "mean", "want", "understand", "doubt",

  # Additional thinking/cognitive verbs
  "acknowledge", "recognize", "accept", "conclude", "contend", "estimate", "judge",
  "suppose", "expect", "imagine", "predict", "project", "assess", "anticipate",
  "contemplate", "reflect", "ponder", "realize", "comprehend", "fathom", "grasp",
  "perceive", "conceive", "deem", "reckon", "figure", "guess", "speculate",
  "theorize", "hypothesize", "surmise", "infer", "deduce", "reason",

  # Feeling/emotional verbs
  "love", "hate", "enjoy", "appreciate", "fear", "dread", "worry",
  "desire", "need", "crave", "yearn", "admire", "respect", "resent", "envy",
  "pity", "sympathize", "empathize", "mourn", "grieve", "lament", "rejoice",
  "celebrate", "cherish", "treasure", "loathe", "despise", "abhor", "detest",
  "tolerate", "endure", "suffer", "mind", "care",

  # Sensory/perception verbs
  "observe", "note", "sense", "witness", "watch", "look", "listen", "smell",
  "taste", "touch", "notice", "spot", "detect", "discern", "glimpse", "view",
  "behold", "read", "scan", "survey",

  # Communication verbs (expressing thoughts/beliefs)
  "say", "tell", "speak", "talk", "report", "mention", "remark",
  "comment", "point", "stress", "emphasize", "highlight", "add",
  "agree", "disagree", "argue", "assert", "maintain", "affirm", "deny", "claim",
  "hold", "state", "express", "declare", "announce", "profess", "confess",
  "admit", "concede", "insist", "contend", "allege", "suggest", "imply",
  "indicate", "intend", "explain", "clarify", "describe", "define",
  "repeat", "restate", "reiterate", "remind", "assure", "reassure", "promise",
  "warn", "caution", "advise", "counsel", "urge", "encourage", "discourage",
  "recommend", "propose", "submit", "request", "ask", "inquire", "question",
  "answer", "respond", "reply", "address", "discuss", "debate",
  "dispute", "challenge", "contest", "criticize", "praise", "thank",
  "congratulate", "welcome", "greet", "honor",

  # Mental state verbs
  "remember", "forget", "recall", "recollect", "identify",
  "learn", "study", "examine", "investigate", "explore", "research",

  # Stative/being verbs
  "remain", "stay", "continue", "last", "persist", "exist", "live",
  "stand", "sit", "lie", "rest", "wait", "depend", "rely", "belong",
  "consist", "comprise", "include", "contain", "involve", "require",
  "deserve", "merit", "warrant", "lack", "miss", "lose", "fail",

  # Modal-like and appearance verbs
  "seem", "appear", "tend", "prove", "turn",

  # Verbs expressing position/stance (not action)
  "favor", "oppose", "support", "endorse", "back", "side",
  "embrace", "reject", "resist", "refuse", "welcome",
  "condemn", "applaud", "commend", "salute"
)

#' Compute control (need for power) scores
#'
#' Classifies verbs associated with first-person subjects as
#' instrumental-control (IC) or other-control (OC) using dependency parsing
#' to link subjects to their governing verbs.
#'
#' @param own_entity A character vector of entity names representing the
#'   speaker's own country or group.
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{IC} and \code{OC}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanIC}, \code{meanOC}, \code{varIC}, \code{varOC}.
#' @export
get_ctrl <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  # Build entities corpus
  all_entities_corpus <- build_entities_corpus()

  ## Define own pronouns (I/we and variants)
  own_pronouns <- c("i", "we")

  ## Expanded self-reference nouns (possessive constructs)
  own_entity_terms <- unique(tolower(c(own_entity, expand_aliases_country(own_entity))))
  self_nouns <- c("government", "nation", "country", "administration",
                   "state", "party", "congress", "people")

  # Parse text
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  parsed <- parsed |> dplyr::mutate(token_lower = tolower(token))

  sentences <- parsed |>
    dplyr::group_by(sentence_id) |>
    dplyr::summarise(sentence = paste(token, collapse = " "), .groups = "drop")

  # Find verbs where I/we OR self-referent noun is the subject
  # Step 1a: Find all pronouns I/we that are subjects (nsubj or nsubjpass)
  pronoun_subjects <- parsed |>
    dplyr::filter(token_lower %in% own_pronouns) |>
    dplyr::filter(dep_rel %in% c("nsubj", "nsubjpass"))

  # Step 1b: Find self-referent nouns preceded by "our/my" that are subjects
  parsed <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(prev_token = dplyr::lag(token_lower, default = "")) |>
    dplyr::ungroup()

  self_noun_subjects <- parsed |>
    dplyr::filter(
      # Possessive construct: "our government", "my country", etc.
      (token_lower %in% self_nouns & prev_token %in% c("our", "my")) |
      # Explicit country reference: "the United States", "America"
      (token_lower %in% own_entity_terms)
    ) |>
    dplyr::filter(dep_rel %in% c("nsubj", "nsubjpass"))

  # Combine both types of subjects
  all_subjects <- dplyr::bind_rows(
    pronoun_subjects |> dplyr::select(doc_id, sentence_id, token, token_id, dep_rel, head_token_id),
    self_noun_subjects |> dplyr::select(doc_id, sentence_id, token, token_id, dep_rel, head_token_id)
  )

  # Step 2: Get the verbs these subjects point to
  verbs <- parsed |> dplyr::filter(pos == "VERB")

  # Join to find verbs with self-referent subjects
  own_verbs <- all_subjects |>
    dplyr::inner_join(
      verbs |> dplyr::select(sentence_id, token_id, verb_token = token, verb_lemma = lemma, verb_tag = tag, verb_dep = dep_rel),
      by = c("sentence_id", "head_token_id" = "token_id")
    )

  if (nrow(own_verbs) == 0) {
    if (bootstrap) {
      return(tibble::tibble(meanIC = 0, meanOC = 0, varIC = 0, varOC = 0))
    } else {
      return(tibble::tibble(IC = 0, OC = 0))
    }
  }

  # Classification function
  classify_verb <- function(lemma, token, token_id, tag, dep_rel, sentence_id, parsed_df) {
    lemma <- tolower(lemma)
    token <- tolower(token)

    sentence_tokens <- parsed_df |> dplyr::filter(sentence_id == !!sentence_id)
    children <- sentence_tokens |> dplyr::filter(head_token_id == !!token_id)

    # 1. IGNORE AUXILIARIES
    if (dep_rel %in% c("aux", "auxpass")) {
      return(NA_character_)
    }

    # 2. "BE" VERBS (Comment #1) - automatically OC
    if (lemma %in% c("be", "is", "are", "was", "were", "been", "being")) {
      return("OC")
    }

    # 3. "HAVE" VERBS (Rule #3) - check for activity objects
    if (lemma %in% c("have", "has", "had")) {
      activity_nouns <- c("talks", "meeting", "meetings", "negotiation", "negotiations",
                         "dialogue", "dialogues", "engagement", "engagements",
                         "discussion", "discussions", "summit", "summits",
                         "action", "actions", "conference", "conferences",
                         "conversation", "conversations", "debate", "debates",
                         "session", "sessions", "hearing", "hearings",
                         "trip", "visit", "tour", "journey")

      has_activity_obj <- any(tolower(children$token) %in% activity_nouns &
                               children$dep_rel == "dobj")

      if (has_activity_obj) return("IC") else return("OC")
    }

    # 4. NEGATION (Rule #2) - negated action verb = IC (decision not to act)
    is_negated <- any(children$dep_rel == "neg")
    if (is_negated) {
      # Negated OC verb stays OC, negated IC verb becomes IC
      if (lemma %in% oc_verbs) return("OC") else return("IC")
    }

    # 5. THINKING / FEELING / SENSORY VERBS (Comment #1) = OC
    if (lemma %in% oc_verbs) {
      return("OC")
    }

    # 6. FINAL: All other verbs = IC (action verbs)
    return("IC")
  }

  # Apply classification to each verb with I/we subject
  verb_classified <- own_verbs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ctrl = classify_verb(verb_lemma, verb_token, head_token_id,
                          verb_tag, verb_dep, sentence_id, parsed)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(ctrl))

  # Count and reshape
  output <- verb_classified |>
    dplyr::count(ctrl) |>
    tidyr::complete(ctrl = c("IC", "OC"), fill = list(n = 0)) |>
    tidyr::pivot_wider(names_from = ctrl, values_from = n)

  if (bootstrap) {
    if (nrow(verb_classified) == 0) {
      btres <- tibble::tibble(meanIC = 0, meanOC = 0, varIC = 0, varOC = 0)
    } else {
      sentence_summary <- verb_classified |>
        dplyr::group_by(sentence_id) |>
        dplyr::count(ctrl) |>
        tidyr::complete(ctrl = c("IC", "OC"), fill = list(n = 0)) |>
        tidyr::pivot_wider(names_from = ctrl, values_from = n)

      boot_results <- purrr::map_dfr(1:B, ~{
        sampled_id <- sentences$sentence_id[sample(nrow(sentences), replace = TRUE)]
        sample_df <- tibble::tibble(sentence_id = sampled_id)

        own_sentences_sampled <- sample_df |>
          dplyr::inner_join(sentence_summary, by = "sentence_id", relationship = "many-to-many")

        if (nrow(own_sentences_sampled) == 0) {
          tibble::tibble(IC = 0, OC = 0)
        } else {
          own_sentences_sampled |>
            dplyr::summarise(IC = sum(IC), OC = sum(OC))
        }
      })

      btres <- boot_results |>
        dplyr::summarize(meanIC = mean(IC), meanOC = mean(OC)) |>
        dplyr::mutate(varIC = stats::var(boot_results$IC), varOC = stats::var(boot_results$OC))
    }

    return(btres)
  } else {
    return(output)
  }
}
