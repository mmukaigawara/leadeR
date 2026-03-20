# Distrust - IMPROVED VERSION
# Key changes:
# 1. Removed blanket quote skipping - now only tracks quote context
# 2. Per-ENTITY S/OS classification instead of sentence-level
# 3. Expanded entity/noun list
# 4. Removed overly aggressive deduplication - counts each mention

library(spacyr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(data.table)

source("code/det_lta/expand_aliases.R")

# Safe alias expansion
expand_aliases_country_safe <- function(x) {
  if (exists("expand_aliases_country", inherits = TRUE)) {
    unlist(lapply(x, function(xx) expand_aliases_country(xx)))
  } else {
    x
  }
}

# Country list
country_list <- c(countries::list_countries(nomenclature = "name_en"))
country_list <- unique(tolower(country_list))
country_list <- c(country_list, "ussr", "east germany",
                  "west germany", "soviet union", "the soviet union")
country_list <- unlist(lapply(country_list, function(x) expand_aliases_country(x)))

# REFINED distrust verbs - focused on actual distrust/harmful intent
# REMOVED: "question", "challenge", "oppose", "resist", "reject", "criticize"
# These are too ambiguous and trigger in neutral/positive contexts
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

# REFINED harmful nominals - focused on clear distrust indicators
# REMOVED: fear, concern, worry, unease, apprehension (too general)
# REMOVED: tension, tensions, crisis, crises (can be neutral)
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
# REMOVED non-codebook nouns: ally, allies, partner, partners, friend, friends,
# leader, leaders, president, prime minister, king, queen, administration, officials
# These were causing overcounting by capturing neutral references
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

get_dist <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  # ---- own/other corpus ----
  io_vec <- if (exists("io", inherits = TRUE)) get("io") else character(0)

  own_entity <- tolower(own_entity)
  own_entity <- unique(c(own_entity, expand_aliases_country_safe(own_entity)))

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

  # ---- parse ----
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  parsed <- parsed |> mutate(
    token_lower = tolower(token),
    lemma_lower = tolower(lemma)
  )

  # ---- Track quote spans (but don't skip sentences) ----
  parsed <- parsed |>
    group_by(doc_id, sentence_id) |>
    mutate(
      is_quote_char = token %in% c('"', "'", "\u201C", "\u201D", "\u2018", "\u2019"),
      quote_cumsum = cumsum(is_quote_char),
      in_quote = (quote_cumsum %% 2 == 1)
    ) |>
    ungroup()

  # ---- entity grouping (multiword) ----
  parsed <- parsed |>
    mutate(ent_flag = ifelse(!is.na(entity) & entity != "", 1, NA)) |>
    group_by(doc_id, sentence_id) |>
    mutate(entity_group_id = data.table::rleid(ent_flag)) |>
    ungroup() |>
    mutate(entity_group_id = ifelse(is.na(ent_flag), NA, entity_group_id))

  multiword_entities <- parsed |>
    filter(!is.na(entity_group_id)) |>
    group_by(doc_id, sentence_id, entity_group_id) |>
    reframe(
      ent_phrase = paste(token_lower, collapse = " "),
      ent_type = first(na.omit(entity)),
      .groups = "drop"
    )

  parsed <- parsed |>
    left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id"))

  # Get sentence text
  sentences <- parsed |>
    group_by(sentence_id) |>
    summarise(sentence_text = paste(token, collapse = " "), .groups = "drop")

  # ---- PER-ENTITY classification ----
  # IMPROVED: Check ENTIRE SENTENCE for distrust context, not just narrow window
  classify_entity <- function(entity_row, sentence_tokens, sentence_text_df) {
    # Check sentence context for distrust markers

    token_id <- entity_row$token_id
    sent_id <- entity_row$sentence_id

    # Get full sentence text
    sent_text <- sentence_text_df |>
      filter(sentence_id == sent_id) |>
      pull(sentence_text)
    sent_text_lower <- tolower(sent_text)

    # Get all tokens in sentence
    sent_tokens <- sentence_tokens |>
      filter(sentence_id == sent_id)

    # Get all lemmas in sentence
    sent_lemmas <- sent_tokens$lemma_lower

    # Check 1: Direct distrust verbs anywhere in sentence
    has_distrust_verb <- any(sent_lemmas %in% distrust_verbs)

    # Check 2: Harmful nominals anywhere in sentence
    has_harmful_nominal <- any(sent_lemmas %in% harmful_nominals)

    # Check 3: Light verb + harm patterns in sentence text
    has_harm_pattern <- str_detect(sent_text_lower, regex(pat_any_harm, ignore_case = TRUE))

    # Check 4: Entity-specific negative modifiers
    entity_modifiers <- sent_tokens |>
      filter(head_token_id == token_id & dep_rel %in% c("amod", "compound"))

    negative_modifiers <- c("hostile", "aggressive", "dangerous", "evil", "corrupt",
                           "threatening", "menacing", "suspicious", "dubious", "rogue",
                           "brutal", "terrorist", "criminal", "oppressive", "belligerent")
    has_negative_modifier <- any(tolower(entity_modifiers$token) %in% negative_modifiers)

    # Check 5: Sentence contains inherently negative entity types
    # If the entity itself is inherently distrusted (enemy, terrorist, etc.)
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

  # ---- Find all codeable entities ----
  # FIX 1: Correct entity regex - spacyr uses TYPE_B/TYPE_I format, not B-TYPE/I-TYPE
  parsed <- parsed |>
    mutate(entity_clean = str_replace(entity, "_[BI]$", ""))

  # FIX 2: Identify self-referential target nouns by checking preceding tokens
  # Words like "our country", "this nation", "my people" refer to SELF, not OTHER
  self_ref_preceding <- c("our", "my", "this", "these")

  # Check if target noun is preceded by self-referential determiner
  parsed <- parsed |>
    group_by(doc_id, sentence_id) |>
    mutate(
      prev_token = lag(token_lower, default = ""),
      is_self_ref_noun = (lemma_lower %in% target_nouns) & (prev_token %in% self_ref_preceding)
    ) |>
    ungroup()

  # FIX 3: BALANCED entity detection
  # Create noun_phrase first for matching
  parsed <- parsed |>
    mutate(noun_phrase_tmp = coalesce(ent_phrase, token_lower))

  # US demonyms/domestic NORP to exclude
  us_norp <- tolower(c("american", "americans", "democratic", "republican",
                        "texan", "texans", "californian", "californians",
                        "new yorker", "new yorkers", "floridian", "floridians"))

  entities <- parsed |>
    filter(
      # GPE entities - ONLY if they match country_list (foreign countries)
      (entity_clean == "GPE" & noun_phrase_tmp %in% all_entities_corpus) |
      # ORG entities - ONLY if they match io list (international organizations)
      (entity_clean == "ORG" & noun_phrase_tmp %in% io_vec) |
      # NORP entities - but exclude US demonyms
      (entity_clean == "NORP" & !(noun_phrase_tmp %in% us_norp) & !(noun_phrase_tmp %in% own_terms)) |
      # Target nouns from codebook - ONLY if NOT self-referential
      (lemma_lower %in% target_nouns & !is_self_ref_noun)
      # PERSON removed - too many US politicians without context
    ) |>
    # Use multi-word phrase if available
    mutate(noun_phrase = noun_phrase_tmp) |>
    # Filter out speaker's own terms
    filter(!(noun_phrase %in% own_terms))

  if (nrow(entities) == 0) {
    if (!bootstrap) return(tibble(S = 0L, OS = 0L))
    return(tibble(meanS = 0, varS = 0, meanOS = 0, varOS = 0))
  }

  # Apply per-entity classification
  entity_classified <- entities |>
    rowwise() |>
    mutate(label = classify_entity(pick(everything()), parsed, sentences)) |>
    ungroup() |>
    select(sentence_id, noun = noun_phrase, label)

  # Count (NO deduplication - count each mention)
  if (!bootstrap) {
    output <- entity_classified |>
      count(label) |>
      tidyr::complete(label = c("S", "OS"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = label, values_from = n) |>
      select(S, OS)

    return(output)

  } else {
    count_by_sentence <- entity_classified |>
      group_by(sentence_id) |>
      count(label) |>
      tidyr::complete(label = c("S", "OS"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = label, values_from = n) |>
      select(sentence_id, S, OS)

    boot_results <- purrr::map_dfr(1:B, ~{
      sampled_ids <- sample(unique(sentences$sentence_id), replace = TRUE)
      sample_df <- tibble(sentence_id = sampled_ids)

      sampled <- sample_df |>
        inner_join(count_by_sentence, by = "sentence_id", relationship = "many-to-many")

      if (nrow(sampled) == 0) {
        tibble(S = 0, OS = 0)
      } else {
        tibble(S = sum(sampled$S, na.rm = TRUE), OS = sum(sampled$OS, na.rm = TRUE))
      }
    })

    btres <- tibble(
      meanS = mean(boot_results$S, na.rm = TRUE),
      varS = var(boot_results$S, na.rm = TRUE),
      meanOS = mean(boot_results$OS, na.rm = TRUE),
      varOS = var(boot_results$OS, na.rm = TRUE)
    )

    return(btres)
  }
}
