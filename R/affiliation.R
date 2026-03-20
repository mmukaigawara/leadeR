#' Compute affiliation scores
#'
#' Measures the affiliation orientation of a leader's speech by classifying
#' verb-level actions as affiliative (A) or other-affiliative (OA) based on
#' the speaker's own-entity references and sentence-level context.
#'
#' @param own_entity A character vector of entity names representing the
#'   speaker's own country or group.
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{A} and \code{OA}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanA}, \code{meanOA}, \code{varA}, \code{varOA}.
#' @export
get_aff <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  # Build entities corpus

  all_entities_corpus <- build_entities_corpus()

  # Regex
  regex_a <- stringr::regex(
    "\\b(friendship|friend(s)?|sympathy|support|cooperate|cooperation|unity|peaceful|amicable|cordial|alliance|ally|solidarity|harmony|collaboration|collaborate|partnership|camaraderie|goodwill|forgive|accept|welcome)\\b",
    ignore_case = TRUE)
  regex_b <- stringr::regex(
    "\\b(agree|agreement|resolve|negotiat|dialog(ue)?|talk(s)?|compromise|concile|restore|reconcile|rapprochement|bridge|resume|resum|reunite|settle|mend|regret)\\b",
    ignore_case = TRUE)
  regex_c <- stringr::regex(
    "\\b(summit|conference|forum|visit|meeting|dialogue|dialog|talk(s)?|joint|coalition|collaborate|together|program)\\b",
    ignore_case = TRUE)
  regex_d <- stringr::regex(
    "\\b(help|aid|assist|support|relief|care|comfort|console|concern|concerned|compassion|charit|nurtur|alleviat|suffer|plight|victim|refugee)\\b",
    ignore_case = TRUE)

  ## Define own and other entities
  own_terms <- c(unique(tolower(own_entity)),
                 "we", "our nation", "our country", "us",
                 "our state", "our government", "our institution",
                 "our group", "our party", "our people",
                 "i", "my nation", "my country", "me",
                 "my state", "my government", "my institution",
                 "my group", "my party", "my people")

  own_entity <- unique(c(tolower(own_entity), expand_aliases_country(own_entity)))

  ## Define own and other entities
  other_terms <- c(setdiff(all_entities_corpus, tolower(own_terms)),
                   "they", "their nation", "their country",
                   "their state", "their government", "their institution")

  # Parse text
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)

  sentences <- parsed |>
    dplyr::group_by(sentence_id) |>
    dplyr::summarise(sentence = paste(token, collapse = " "), .groups = "drop")

  # Step 1: Get multiword named entities
  parsed_entity_grouped <- parsed |>
    dplyr::mutate(ent_group = ifelse(!is.na(entity) & entity != "", 1, NA)) |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(entity_group_id = data.table::rleid(ent_group)) |>
    dplyr::ungroup() |>
    dplyr::mutate(entity_group_id = ifelse(is.na(ent_group), NA, entity_group_id))

  multiword_entities <- parsed_entity_grouped |>
    dplyr::filter(!is.na(entity_group_id)) |>
    dplyr::group_by(doc_id, sentence_id, entity_group_id) |>
    dplyr::summarise(entity = paste(token, collapse = " "), .groups = "drop")

  # Step 2: Get subjects and join with named entities (new, to incorporate possessives)
  subjects <- parsed_entity_grouped |>
    dplyr::filter(dep_rel %in% c("poss", "nsubj", "nsubjpass")) |>
    dplyr::select(-entity) |>
    dplyr::left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
    dplyr::mutate(
      poss_pronoun = dplyr::lag(lemma),
      poss_dep = dplyr::lag(dep_rel),
      has_possessive = poss_pronoun %in% c("my", "our", "their") & poss_dep == "poss",
      subject_phrase = dplyr::case_when(
        has_possessive ~ paste(poss_pronoun, lemma),        # e.g., "our concern"
        !is.na(entity) ~ entity,                             # multiword entities
        TRUE ~ token
      )
    ) |>
    dplyr::filter(dep_rel %in% c("nsubj", "nsubjpass") | (dep_rel == "poss" & has_possessive)) |>
    dplyr::select(doc_id, sentence_id, subject_phrase, head_token_id) |>
    dplyr::distinct() |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::summarise(subject = tolower(dplyr::first(subject_phrase)),
              head_token_id = dplyr::first(head_token_id), .groups = "drop")

  sentences <- parsed |>
    dplyr::group_by(sentence_id) |>
    dplyr::summarise(sentence = paste(token, collapse = " "), .groups = "drop") |>
    dplyr::left_join(subjects, by = "sentence_id") |>
    dplyr::mutate(entity_type = ifelse(subject %in% own_terms, "own", NA_character_))

  ## Passive voice: Handle passive voice: "by us" etc. -----
  agent_tokens <- parsed |>
    dplyr::filter(dep_rel == "agent") |>
    dplyr::select(doc_id, sentence_id, agent_token_id = token_id)

  agent_objects <- parsed |>
    dplyr::filter(dep_rel == "pobj" & tolower(token) %in% c("us", "we", "our", "my")) |>
    dplyr::rename(child_token_id = token_id, agent_actor = token)

  agents <- agent_tokens |>
    dplyr::left_join(agent_objects,
              by = c("doc_id", "sentence_id", "agent_token_id" = "head_token_id")) |>
    dplyr::filter(!is.na(agent_actor)) |>
    dplyr::select(doc_id, sentence_id, agent_actor)

  sentences_with_agents <- sentences |>
    dplyr::left_join(agents, by = "sentence_id")

  ## Identify sentences with own subjects or own agents -----
  own_terms_regex <- paste0(stringr::str_replace_all(tolower(own_terms), "([\\W])", "\\\\\\1"),
                            collapse = "|")

  sentences_with_agents <- sentences_with_agents |>
    dplyr::mutate(
      subject_norm   = subject,
      agent_norm     = stringr::str_to_lower(agent_actor),
      starts_with_own_pron = stringr::str_detect(subject_norm, "^(we|us|our|my)\\b"),
      starts_with_own_name = if (nchar(own_terms_regex) > 0)
        stringr::str_detect(subject_norm, paste0("^(?:", own_terms_regex, ")\\b")) else FALSE,
      exact_own_match = subject_norm %in% tolower(own_terms)
    )

  own_sentences <- sentences_with_agents |>
    dplyr::filter(
      exact_own_match | starts_with_own_pron | starts_with_own_name |
        agent_norm %in% tolower(own_terms)
    )

  verbs <- parsed |>
    dplyr::filter(pos == "VERB") |>
    dplyr::inner_join(
      own_sentences |> dplyr::select(sentence_id, head_token_id),
      by = c("sentence_id", "token_id" = "head_token_id")
    )

  classify_aff <- function(lemma, sentence) {
    lemma <- tolower(lemma)
    sentence <- tolower(sentence)

    if (lemma %in% c("will", "would", "shall", "should", "can", "could", "may", "might",
                     "must", "do", "does", "did", "have", "has", "had",
                     "be", "am", "is", "are", "was", "were", "being", "been")) {
      return(NA_character_)
    }

    cond_a <- stringr::str_detect(sentence, regex_a)
    cond_b <- stringr::str_detect(sentence, regex_b)
    cond_c <- stringr::str_detect(sentence, regex_c)
    cond_d <- stringr::str_detect(sentence, regex_d)

    if (cond_a | cond_b | cond_c | cond_d) {
      return("A")
    } else {
      return("OA")
    }
  }

  if (nrow(own_sentences) == 0 | nrow(verbs) == 0) {
    if (bootstrap) {
      return(tibble::tibble(meanA = 0, meanOA = 0, varA = 0, varOA = 0))
    } else {
      return(tibble::tibble(A = 0, OA = 0))
    }
  }

  verbs_joined <- verbs |>
    dplyr::left_join(own_sentences, by = "sentence_id") |>
    dplyr::filter(!is.na(sentence))

  verb_classified <- verbs_joined |>
    dplyr::rowwise() |>
    dplyr::mutate(
      affiliation = classify_aff(lemma, sentence)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(affiliation))

  output <- verb_classified |>
    dplyr::count(affiliation) |>
    tidyr::complete(affiliation = c("A", "OA"), fill = list(n = 0)) |>
    tidyr::pivot_wider(names_from = affiliation, values_from = n)

  if (bootstrap) {

    if (nrow(verb_classified) == 0) {

      btres <- tibble::tibble(meanA = 0, meanOA = 0, varA = 0, varOA = 0)

    } else {

    sentence_summary <- verb_classified |>
      dplyr::group_by(sentence_id) |>
      dplyr::count(affiliation) |>
      tidyr::complete(affiliation = c("A", "OA"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = affiliation, values_from = n)

    boot_results <- purrr::map_dfr(1:B, ~{
      sampled_id <- sentences$sentence_id[sample(nrow(sentences), replace = TRUE)]
      sample_df <- tibble::tibble(sentence_id = sampled_id)

      own_sentences_sampled <- sample_df |>
        dplyr::inner_join(sentence_summary, by = "sentence_id", relationship = "many-to-many")

      if (nrow(own_sentences_sampled) == 0) {

        tibble::tibble(A = 0, OA = 0)

      } else {

        own_sentences_sampled |>
          dplyr::summarise(
            A = sum(A),
            OA = sum(OA)
          )

      }
    })

    btres <- boot_results |>
                   dplyr::summarise(meanA = mean(A), meanOA = mean(OA),
                             varA = stats::var(A), varOA = stats::var(OA))

    }

    return(btres)

  } else {

    return(output)

  }
}
