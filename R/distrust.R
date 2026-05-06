# Distrust

# Distrust verbs - focused on actual distrust/harmful intent
distrust_verbs <- c(
  # Core harmful action verbs (high confidence)
  "force", "harm", "hurt", "damage", "undermine", "threaten",
  "interfere", "plot", "scheme", "sabotage", "subvert",
  "deceive", "manipulate", "destroy", "attack", "conspire",
  "exploit", "occupy", "infiltrate", "coerce", "intimidate",
  "blackmail", "extort", "oppress", "bully", "terrorize",
  "invade", "violate", "endanger", "jeopardize",
  # Distrust/suspicion verbs (high confidence)
  "distrust", "suspect", "mistrust", "fear",
  # Strong condemnation verbs
  "condemn", "denounce", "accuse", "blame"
)

# Harmful nominals
harmful_nominals <- c(
  # Clear harmful outcomes
  "trouble", "harm", "damage", "instability", "chaos", "violence", "unrest",
  "conflict", "conflicts", "war", "warfare",
  "famine", "suffering", "misery", "terror", "attack", "attacks", "aggression",
  "threat", "threats", "danger", "dangers", "sabotage", "subversion",
  # Hostile actions/states
  "interference", "propaganda", "disinformation", "espionage", "infiltration",
  "oppression", "occupation", "destabilization",
  "hostility", "animosity", "enmity", "hatred",
  # Clear distrust indicators
  "suspicion", "distrust", "mistrust"
)

# Build regex patterns
harmful_re <- paste0("(?:", paste(harmful_nominals, collapse = "|"), ")")
harmful_verbs_re <- paste0("(?:", paste(distrust_verbs, collapse = "|"), ")")

pat_light_verb <- paste0(
  "\\b(cause|create|bring|inflict|incite|spark|fuel|foment|provoke|stir|trigger)\\b",
  "\\s+(?:\\w+\\s+){0,2}", harmful_re, "\\b"
)
pat_pose_threat <- "\\b(pose|present|constitute)\\b\\s+(?:an?\\s+)?(threat|risk|danger)s?\\b"
pat_engage_in <- paste0(
  "\\b(engage\\s+in|carry\\s+out|conduct|mount|wage|launch)\\b\\s+",
  "(?:attack|attacks|sabotage|espionage|aggression|war|warfare)\\b"
)
pat_attempt_to <- paste0(
  "\\b(attempt|seek|plan|prepare|conspire)\\b\\s+to\\s+", harmful_verbs_re, "\\b"
)
pat_any_harm <- paste(pat_light_verb, pat_pose_threat, pat_engage_in, pat_attempt_to, sep = "|")

# CODEBOOK-ALIGNED target nouns list
target_nouns <- c(
  # CODEBOOK SPECIFIED nouns (from distrust.rtf)
  "country", "countries", "people", "government", "governments",
  "state", "states", "nation", "nations", "society", "societies",
  "power", "powers", "political party", "party", "parties",
  "rebel", "rebels", "revolutionary", "revolutionaries",
  # Additional entity types from codebook
  "opposition", "militants", "faction", "factions", "agents", "spies", "mercenaries",
  # Inherently hostile entities (always relevant for distrust coding)
  "enemy", "enemies", "adversary", "adversaries", "foe", "foes",
  "terrorist", "terrorists", "attacker", "attackers", "aggressor", "aggressors",
  "dictator", "dictators", "tyrant", "tyrants", "regime", "regimes",
  # Military entities
  "army", "military", "forces", "troops",
  # Generic group terms (from codebook)
  "group", "groups", "organization", "organizations", "movement", "movements"
)

own_pronouns <- c("i", "me", "my", "mine", "myself", "we", "us", "our", "ours", "ourselves")

#' Compute distrust scores
#'
#' Performs per-entity classification of references in the text as
#' suspicious (S) or other-suspicious (OS) based on distrust verbs,
#' harmful nominals, and contextual modifiers.
#'
#' @param own_entity A character vector of entity names representing the
#'   speaker's own country or group.
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{S} and \code{OS}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanS}, \code{varS}, \code{meanOS}, \code{varOS}.
#' @export
get_dist <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  # Build country list
  country_list <- build_country_list()

  # Own/other corpus
  io_vec <- if (exists("io", inherits = TRUE)) get("io") else character(0)

  own_entity <- tolower(own_entity)
  own_entity <- unique(c(own_entity, expand_aliases_country(own_entity)))

  # US states - these are domestic entities for a US speaker
  us_states <- tolower(c(
    "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut",
    "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa",
    "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan",
    "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
    "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio",
    "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota",
    "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia",
    "wisconsin", "wyoming", "district of columbia", "d.c.", "dc", "puerto rico", "guam"
  ))

  # US government bodies - domestic entities
  us_gov_orgs <- tolower(c(
    "congress", "senate", "house", "house of representatives", "supreme court",
    "white house", "pentagon", "state department", "treasury", "defense department",
    "justice department", "fbi", "cia", "nsa", "homeland security",
    "democratic party", "republican party", "democrats", "republicans",
    "administration", "cabinet", "federal reserve", "fed"
  ))

  # US demonyms and references
  us_demonyms <- tolower(c(
    "american", "americans", "u.s.", "u.s.a.", "usa"
  ))

  own_terms <- unique(tolower(c(
    own_entity, us_states, us_gov_orgs, us_demonyms,
    "we", "our nation", "our country", "us", "our state", "our government", "our institution",
    "our group", "our party", "our people", "i", "my nation", "my country", "me", "my state",
    "my government", "my institution", "my group", "my party", "my people"
  )))

  all_entities_corpus <- unique(tolower(c(io_vec, country_list)))

  other_terms <- unique(c(setdiff(all_entities_corpus, own_terms),
                          "they", "their nation", "their country", "their state",
                          "their government", "their institution", "their party", "their people"))

  # Parse
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  parsed <- parsed |> dplyr::mutate(
    token_lower = tolower(token),
    lemma_lower = tolower(lemma)
  )

  # Track quote spans (but don't skip sentences)
  parsed <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(
      is_quote_char = token %in% c('"', "'", "\u201C", "\u201D", "\u2018", "\u2019"),
      quote_cumsum = cumsum(is_quote_char),
      in_quote = (quote_cumsum %% 2 == 1)
    ) |>
    dplyr::ungroup()

  # Entity grouping (multiword)
  parsed <- parsed |>
    dplyr::mutate(ent_flag = ifelse(!is.na(entity) & entity != "", 1, NA)) |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(entity_group_id = data.table::rleid(ent_flag)) |>
    dplyr::ungroup() |>
    dplyr::mutate(entity_group_id = ifelse(is.na(ent_flag), NA, entity_group_id))

  multiword_entities <- parsed |>
    dplyr::filter(!is.na(entity_group_id)) |>
    dplyr::group_by(doc_id, sentence_id, entity_group_id) |>
    dplyr::reframe(
      ent_phrase = paste(token_lower, collapse = " "),
      ent_type = dplyr::first(na.omit(entity)),
      .groups = "drop"
    )

  parsed <- parsed |>
    dplyr::left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id"))

  # Get sentence text
  sentences <- parsed |>
    dplyr::group_by(sentence_id) |>
    dplyr::summarise(sentence_text = paste(token, collapse = " "), .groups = "drop")

  # PER-ENTITY classification
  classify_entity <- function(entity_row, sentence_tokens, sentence_text_df) {
    token_id <- entity_row$token_id
    sent_id <- entity_row$sentence_id

    # Get full sentence text
    sent_text <- sentence_text_df |>
      dplyr::filter(sentence_id == sent_id) |>
      dplyr::pull(sentence_text)
    sent_text_lower <- tolower(sent_text)

    # Get all tokens in sentence
    sent_tokens <- sentence_tokens |>
      dplyr::filter(sentence_id == sent_id)

    # Get all lemmas in sentence
    sent_lemmas <- sent_tokens$lemma_lower

    # Check 1: Direct distrust verbs anywhere in sentence
    has_distrust_verb <- any(sent_lemmas %in% distrust_verbs)

    # Check 2: Harmful nominals anywhere in sentence
    has_harmful_nominal <- any(sent_lemmas %in% harmful_nominals)

    # Check 3: Light verb + harm patterns in sentence text
    has_harm_pattern <- stringr::str_detect(sent_text_lower, stringr::regex(pat_any_harm, ignore_case = TRUE))

    # Check 4: Entity-specific negative modifiers
    entity_modifiers <- sent_tokens |>
      dplyr::filter(head_token_id == token_id & dep_rel %in% c("amod", "compound"))

    negative_modifiers <- c("hostile", "aggressive", "dangerous", "evil", "corrupt",
                           "threatening", "menacing", "suspicious", "dubious", "rogue",
                           "brutal", "terrorist", "criminal", "oppressive", "belligerent")
    has_negative_modifier <- any(tolower(entity_modifiers$token) %in% negative_modifiers)

    # Check 5: Sentence contains inherently negative entity types
    entity_phrase <- tolower(entity_row$noun_phrase)
    inherently_distrusted <- c("enemy", "enemies", "terrorist", "terrorists", "attacker",
                               "attackers", "aggressor", "aggressors", "adversary", "adversaries",
                               "foe", "foes", "dictator", "dictators", "tyrant", "tyrants")
    is_inherently_distrusted <- entity_phrase %in% inherently_distrusted

    # Determine S or OS
    if (has_distrust_verb || has_harmful_nominal || has_harm_pattern ||
        has_negative_modifier || is_inherently_distrusted) {
      return("S")
    } else {
      return("OS")
    }
  }

  # Find all codeable entities
  parsed <- parsed |>
    dplyr::mutate(entity_clean = stringr::str_replace(entity, "_[BI]$", ""))

  # Identify self-referential target nouns by checking preceding tokens
  self_ref_preceding <- c("our", "my", "this", "these")

  parsed <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(
      prev_token = dplyr::lag(token_lower, default = ""),
      is_self_ref_noun = (lemma_lower %in% target_nouns) & (prev_token %in% self_ref_preceding)
    ) |>
    dplyr::ungroup()

  # BALANCED entity detection
  parsed <- parsed |>
    dplyr::mutate(noun_phrase_tmp = dplyr::coalesce(ent_phrase, token_lower))

  # US demonyms/domestic NORP to exclude
  us_norp <- tolower(c("american", "americans", "democratic", "republican",
                        "texan", "texans", "californian", "californians",
                        "new yorker", "new yorkers", "floridian", "floridians"))

  entities <- parsed |>
    dplyr::filter(
      # GPE entities - ONLY if they match country_list (foreign countries)
      (entity_clean == "GPE" & noun_phrase_tmp %in% all_entities_corpus) |
      # ORG entities - ONLY if they match io list (international organizations)
      (entity_clean == "ORG" & noun_phrase_tmp %in% io_vec) |
      # NORP entities - but exclude US demonyms
      (entity_clean == "NORP" & !(noun_phrase_tmp %in% us_norp) & !(noun_phrase_tmp %in% own_terms)) |
      # Target nouns from codebook - ONLY if NOT self-referential
      (lemma_lower %in% target_nouns & !is_self_ref_noun)
    ) |>
    # Use multi-word phrase if available
    dplyr::mutate(noun_phrase = noun_phrase_tmp) |>
    # Filter out speaker's own terms
    dplyr::filter(!(noun_phrase %in% own_terms))

  if (nrow(entities) == 0) {
    if (!bootstrap) return(tibble::tibble(S = 0L, OS = 0L))
    return(tibble::tibble(meanS = 0, varS = 0, meanOS = 0, varOS = 0))
  }

  # Apply per-entity classification
  entity_classified <- entities |>
    dplyr::rowwise() |>
    dplyr::mutate(label = classify_entity(dplyr::pick(dplyr::everything()), parsed, sentences)) |>
    dplyr::ungroup() |>
    dplyr::select(sentence_id, noun = noun_phrase, label)

  # Count (NO deduplication - count each mention)
  if (!bootstrap) {
    output <- entity_classified |>
      dplyr::count(label) |>
      tidyr::complete(label = c("S", "OS"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = label, values_from = n) |>
      dplyr::select(S, OS)

    return(output)

  } else {
    count_by_sentence <- entity_classified |>
      dplyr::group_by(sentence_id) |>
      dplyr::count(label) |>
      tidyr::complete(label = c("S", "OS"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = label, values_from = n) |>
      dplyr::select(sentence_id, S, OS)

    boot_results <- purrr::map_dfr(1:B, ~{
      sampled_ids <- sample(unique(sentences$sentence_id), replace = TRUE)
      sample_df <- tibble::tibble(sentence_id = sampled_ids)

      sampled <- sample_df |>
        dplyr::inner_join(count_by_sentence, by = "sentence_id", relationship = "many-to-many")

      if (nrow(sampled) == 0) {
        tibble::tibble(S = 0, OS = 0)
      } else {
        tibble::tibble(S = sum(sampled$S, na.rm = TRUE), OS = sum(sampled$OS, na.rm = TRUE))
      }
    })

    btres <- tibble::tibble(
      meanS = mean(boot_results$S, na.rm = TRUE),
      varS = stats::var(boot_results$S, na.rm = TRUE),
      meanOS = mean(boot_results$OS, na.rm = TRUE),
      varOS = stats::var(boot_results$OS, na.rm = TRUE)
    )

    return(btres)
  }
}
