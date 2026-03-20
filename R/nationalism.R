# Nationalism - REFERENT-LEVEL VERSION (v3)
# Key changes:
# 1. Groups multi-word entities into single referents
# 2. Only counts GPE entities (countries) - not NORP/ORG
# 3. Very conservative pronoun counting

library(spacyr)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(data.table)

source("code/det_lta/expand_aliases.R")

# Modifier lists for own-country references
favorable_descriptors <- c(
  "great", "good", "peace-loving", "progressive", "successful", "prosperous",
  "democratic", "leading", "innovative", "vibrant", "dynamic", "respected",
  "excellent", "positive", "constructive", "friendly", "free",
  "proud", "diverse", "thriving", "strong", "noble", "beautiful", "resilient",
  "hardworking", "brave", "courageous", "united", "powerful", "mighty",
  "blessed", "fortunate", "exceptional", "magnificent", "glorious")

strength_descriptors <- c(
  "powerful", "capable", "prepared", "strong", "resilient", "resourceful",
  "unyielding", "robust", "effective", "decisive", "leading", "advanced",
  "determined", "steadfast", "unwavering", "formidable", "invincible")

# Phrases indicating national honor/identity (checked in sentence context)
national_honor_phrases <- c(
  "sovereignty", "self-determination", "national interest", "homeland",
  "patriotism", "patriotic", "national pride", "founding principles",
  "heritage", "tradition", "honor", "duty", "sacred", "independence",
  "freedom", "liberty", "destiny", "unity",
  # Additional nationalistic phrases
  "american dream", "american people", "american spirit", "american way",
  "our great nation", "this great nation", "our beloved country",
  "land of the free", "home of the brave", "greatest nation",
  "our founding fathers", "founding principles", "constitution",
  "stars and stripes", "old glory", "star-spangled",
  "god bless america", "god bless the united states",
  "defend our nation", "protect our nation", "serve our nation",
  "proud american", "proud to be american", "american values",
  "national security", "national defense", "national sovereignty",
  "american leadership", "american strength", "american resolve",
  "our troops", "our soldiers", "our armed forces", "our military",
  "brave men and women", "heroes", "veterans",
  # Generic nation-referent phrases (Fix 2: expanded list)
  "our nation", "my nation", "this nation", "the nation",
  "our country", "my country", "this country",
  "our government", "our people", "our citizens",
  "great nation", "proud nation", "best nation",
  "our strength", "our power", "our future",
  "our way of life", "our values", "our children",
  "stand united", "stand together",
  "our allies", "our friends", "our partners",
  "defend our", "protect our", "serve our",
  "fight for", "work for our",
  "democratic party", "republican party", "our party",
  "our state", "our government's"
)

# Unfavorable descriptors for other countries
unfavorable_descriptors_others <- c(
  "weak", "colonialist", "imperialistic", "hostile", "unfriendly", "corrupt", "failing",
  "aggressive", "ineffective", "powerless", "backward", "oppressive", "dangerous",
  "destructive", "imperialist", "authoritarian", "tyrannical", "repressive", "fascist",
  "totalitarian", "genocidal", "terrorist", "pariah", "illegitimate", "extremist",
  "undemocratic", "autocratic", "theocratic", "dictatorial", "belligerent", "evil",
  "rogue", "brutal", "ruthless", "criminal", "barbaric")

# Meddling behavior verbs for other countries
meddling_verbs <- c(
  "interfere", "meddle", "undermine", "destabilize", "violate", "threaten",
  "attack", "invade", "occupy", "exploit", "manipulate", "deceive", "provoke",
  "sabotage", "subvert", "infiltrate", "coerce", "intimidate", "oppress")

# Own-entity pronouns
own_pronouns <- c("we", "us", "our", "ours", "ourselves")
other_pronouns <- c("they", "them", "their", "theirs", "themselves")

# International organizations to count as "Other" entities for nationalism
intl_orgs <- c(
  "united nations", "un", "nato", "who", "world health organization",
  "imf", "international monetary fund", "world bank", "oas",
  "organization of american states", "european union", "eu", "ec",
  "european community", "g7", "g8", "g20", "opec", "wto",
  "world trade organization", "iaea", "international atomic energy agency",
  "security council", "general assembly"
)

get_nat <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  # Define own entity terms
  own_entity_terms <- unique(tolower(c(own_entity, expand_aliases_country(own_entity))))

  # Parse text
  parsed <- spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  parsed <- parsed |> mutate(token_lower = tolower(token))

  # Get sentence text for context checking
  sentences <- parsed |>
    group_by(sentence_id) |>
    summarise(sentence = paste(token, collapse = " "), .groups = "drop")

  # STEP 1: Group multi-word entities into single referents
  # Use run-length encoding to group consecutive entity tokens
  # Only include GPE (countries) - ORG caused massive overcounting
  parsed <- parsed |>
    mutate(
      is_gpe = grepl("GPE", entity),
      ent_group = data.table::rleid(is_gpe & !is.na(entity) & entity != "")
    )

  # Extract GPE entity phrases (grouped)
  gpe_entities <- parsed |>
    filter(is_gpe) |>
    group_by(doc_id, sentence_id, ent_group) |>
    summarise(
      phrase = paste(token_lower, collapse = " "),
      first_token_id = first(token_id),
      .groups = "drop"
    ) |>
    left_join(sentences, by = "sentence_id")

  # STEP 2: Count own-reference pronouns - ONE per sentence (not per pronoun)
  # Pronouns only contribute to N (with nationalistic context), never to ON
  own_pronoun_refs <- parsed |>
    filter(
      token_lower %in% c("we", "our") &
      dep_rel %in% c("nsubj", "poss", "nsubjpass")
    ) |>
    group_by(doc_id, sentence_id) |>
    summarise(
      phrase = "we/our",
      first_token_id = first(token_id),
      .groups = "drop"
    ) |>
    left_join(sentences, by = "sentence_id")

  # STEP 3: Classify referents

  # Classify GPE entities as own or other
  if (nrow(gpe_entities) > 0) {
    gpe_entities <- gpe_entities |>
      mutate(
        entity_type = ifelse(phrase %in% own_entity_terms, "own", "other"),
        is_pronoun = FALSE
      )
  } else {
    gpe_entities <- tibble(
      sentence_id = integer(), phrase = character(), first_token_id = integer(),
      sentence = character(), entity_type = character(), is_pronoun = logical()
    )
  }

  # Own pronouns are always "own" type and marked as pronouns
  if (nrow(own_pronoun_refs) > 0) {
    own_pronoun_refs <- own_pronoun_refs |>
      mutate(entity_type = "own", is_pronoun = TRUE)
  } else {
    own_pronoun_refs <- tibble(
      sentence_id = integer(), phrase = character(), first_token_id = integer(),
      sentence = character(), entity_type = character(), is_pronoun = logical()
    )
  }

  # Combine all referents
  all_referents <- bind_rows(
    gpe_entities |> select(sentence_id, phrase, first_token_id, sentence, entity_type, is_pronoun),
    own_pronoun_refs |> select(sentence_id, phrase, first_token_id, sentence, entity_type, is_pronoun)
  )

  if (nrow(all_referents) == 0) {
    if (bootstrap) {
      return(tibble(meanN = 0, meanON = 0, varN = 0, varON = 0))
    } else {
      return(tibble(N = 0, ON = 0))
    }
  }

  # STEP 4: Apply N/ON classification based on context
  # Key fix: Pronouns only count toward N (with nationalistic context), never toward ON
  # This addresses the +12.88 ON overcounting bias
  classify_referent <- function(token_id, sent_id, ent_type, phrase, sentence_text, parsed_df, is_pron) {

    sent_tokens <- parsed_df |>
      filter(sentence_id == sent_id) |>
      mutate(token_lower = tolower(token))

    # Find modifiers attached to this token via dependency
    modifiers <- sent_tokens |>
      filter(head_token_id == token_id & dep_rel %in% c("amod", "advmod", "compound"))

    modifier_words <- tolower(modifiers$token)

    # Check window around token
    token_idx <- which(sent_tokens$token_id == token_id)
    if (length(token_idx) > 0) {
      window_start <- max(1, token_idx - 3)
      window_end <- min(nrow(sent_tokens), token_idx + 3)
      window_tokens <- tolower(sent_tokens$token[window_start:window_end])
    } else {
      window_tokens <- character(0)
    }

    context_words <- unique(c(modifier_words, window_tokens))
    sentence_lower <- tolower(sentence_text)

    if (ent_type == "own") {
      # Check for favorable/strength modifiers or national honor phrases
      has_favorable <- any(context_words %in% favorable_descriptors)
      has_strength <- any(context_words %in% strength_descriptors)
      has_honor <- any(sapply(national_honor_phrases, function(p) grepl(p, sentence_lower, fixed = TRUE)))

      # If any national indicator present, classify as N
      # NOTE: Removed the overly broad has_nation_noun check as it caused overcounting
      if (has_favorable || has_strength || has_honor) {
        return("N")
      } else {
        # Pronouns without nationalistic context count toward ON (neutral national reference)
        # Per codebook: "we/us/our counted if referring to ALL the people or WHOLE nation"
        # When no nationalistic modifier present, it's still a reference to the nation
        return("ON")
      }
    } else {
      # entity_type == "other"
      has_unfavorable <- any(context_words %in% unfavorable_descriptors_others)
      sent_verbs <- tolower(sent_tokens$lemma[sent_tokens$pos == "VERB"])
      has_meddling <- any(sent_verbs %in% meddling_verbs)

      if (has_unfavorable || has_meddling) {
        return("N")
      } else {
        return("ON")
      }
    }
  }

  # Apply classification
  referent_classified <- all_referents |>
    rowwise() |>
    mutate(
      mod = classify_referent(first_token_id, sentence_id, entity_type, phrase, sentence, parsed, is_pronoun)
    ) |>
    ungroup()

  # STEP 5: Count N and ON
  output <- referent_classified |>
    count(mod) |>
    tidyr::complete(mod = c("N", "ON"), fill = list(n = 0)) |>
    tidyr::pivot_wider(names_from = mod, values_from = n)

  if (bootstrap) {
    sentence_summary <- referent_classified |>
      group_by(sentence_id) |>
      count(mod) |>
      tidyr::complete(mod = c("N", "ON"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = mod, values_from = n)

    boot_results <- purrr::map_dfr(1:B, ~{
      sampled_ids <- sample(unique(sentences$sentence_id), replace = TRUE)
      sample_df <- tibble(sentence_id = sampled_ids)

      sampled <- sample_df |>
        inner_join(sentence_summary, by = "sentence_id", relationship = "many-to-many")

      if (nrow(sampled) == 0) {
        tibble(N = 0, ON = 0)
      } else {
        tibble(N = sum(sampled$N, na.rm = TRUE), ON = sum(sampled$ON, na.rm = TRUE))
      }
    })

    btres <- boot_results |>
      summarise(meanN = mean(N), meanON = mean(ON)) |>
      mutate(varN = var(boot_results$N), varON = var(boot_results$ON))

    return(btres)
  } else {
    return(output)
  }
}
