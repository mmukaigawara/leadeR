# Power, deterministic

library(spacyr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)

source("code/det_lta/expand_aliases.R")
#spacyr::spacy_initialize(model = "en_core_web_sm")

# All entities
country_list <- c(countries::list_countries(nomenclature = "name_en"))
country_list <- unique(tolower(country_list))
country_list <- c(country_list, "ussr", "east germany", 
                  "west germany", "soviet union", "the soviet union") # Soviet abbreviation
country_list <- unlist(lapply(country_list, function(x) expand_aliases_country(x)))
all_entities_corpus <- c(tolower(io), country_list)

# Regex patterns for power conditions
regex_a <- regex("\\b(wipe( them)? out|defeat(ed)?|destroy(ed)?|attack(ed)?|assault(ed)?|threat(en|ened)?|accus(ed|e)?|reprimand(ed)?|retaliat(ed|e)?|blame(d)?)\\b", ignore_case = TRUE)
regex_b <- regex("\\b(help(ed)?|assist(ed)?|support(ed)?|aid(ed)?|encourag(ed|e)?|advise(d)?)\\b.*\\b(them|another|others|recipient|nation(s)?|group(s)?)\\b", ignore_case = TRUE)
regex_c <- regex("\\b(arrang(ed)?|regulat(ed|e)?|control(led)?|determin(e|ed)?|check(ed)?|investigat(ed|e)?|search(ed)?|monitor(ed)?)\\b.*\\b(them|their|another|lives|actions?)\\b", ignore_case = TRUE)
regex_d <- regex("\\b(influenc(e|ed)?|persuad(e|ed)?|convinc(e|ed)?|brib(e|ed)?|argu(e|ed)?|suggest(ed)?|hint(ed)?|pressure(d)?|advocat(e|ed)?|hope(d)?)\\b.*\\b(them|recipient|group|nation|he|she|you)\\b", ignore_case = TRUE)
regex_e <- regex("\\b(impress(ed)?|prestig(e|ious)?|display(ed)?|show(ed|case(d)?)?|notoriety|fame|glory|take|took|taken|honor(ed)?)\\b", ignore_case = TRUE)
regex_f <- regex("\\b(vindicat(ed)?|superior|status|reputation|weak(ness)?|inferior|strength|power(ful)?|prestige|blame(d)?|emphasiz(e|ed)|criticized|guarantee(d)?)\\b", ignore_case = TRUE)

p_keywords <- c(
  # Original codebook list (from LTA codebook line 47)
  "warn", "help", "hint", "stop", "suggest", "impress", "promise", "worried",
  "attack", "demand", "refuse", "accuse", "protect", "defend", "retaliate", "blame",
  "support", "threaten", "guard", "achieve", "instigate", "arrange", "hope",
  # From codebook examples
  "take",       # Example 5: "If we take this action" → P (condition e)
  "emphasize"   # Example 6: "I emphasize the significance" → P (condition f)
)

get_power <- function(own_entity, text, bootstrap = FALSE, B = 1000) {
  
  # ---- Own / other terms ----
  own_terms <- c(
    unique(tolower(own_entity)),
    "we", "our nation", "our country", "us",
    "our state", "our government", "our institution",
    "our group", "our party", "our people",
    "i", "my nation", "my country", "me",
    "my state", "my government", "my institution",
    "my group", "my party", "my people"
  )
  
  # expand aliases if you use your helper
  own_entity <- unique(c(tolower(own_entity), expand_aliases_country(own_entity)))
  own_terms  <- unique(c(tolower(own_terms), own_entity))
  own_terms_norm <- stringr::str_to_lower(stringr::str_replace(own_terms, "^the\\s+", ""))
  
  # ---- Parse ----
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  
  if (nrow(parsed) == 0) {
    if (bootstrap) return(tibble::tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0))
    return(tibble::tibble(P = 0, OP = 0))
  }
  
  # ---- Sentence strings ----
  sentences <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::summarise(sentence = paste(token, collapse = " "), .groups = "drop")
  
  # ---- Multiword entity grouping (contiguous entity tokens) ----
  parsed_entity_grouped <- parsed |>
    dplyr::mutate(ent_group = ifelse(!is.na(entity) & entity != "", 1L, NA_integer_)) |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(entity_group_id = data.table::rleid(ent_group)) |>
    dplyr::ungroup() |>
    dplyr::mutate(entity_group_id = ifelse(is.na(ent_group), NA_integer_, entity_group_id))
  
  entity_groups <- parsed_entity_grouped |>
    dplyr::filter(!is.na(entity_group_id)) |>
    dplyr::group_by(doc_id, sentence_id, entity_group_id) |>
    dplyr::summarise(entity_phrase = paste(token, collapse = " "), .groups = "drop")
  
  # token_id -> entity_group_id map (for quick lookup)
  token_to_entity_group <- parsed_entity_grouped |>
    dplyr::filter(!is.na(entity_group_id)) |>
    dplyr::select(doc_id, sentence_id, token_id, entity_group_id) |>
    dplyr::distinct()
  
  # ---- Possessive map: poss -> head noun (e.g., our -> government) ----
  poss_map <- parsed |>
    dplyr::filter(dep_rel == "poss") |>
    dplyr::transmute(
      doc_id, sentence_id,
      head_noun_id = head_token_id,               # noun token_id
      poss_lemma   = stringr::str_to_lower(lemma) # "our"/"my"/"their"
    ) |>
    dplyr::filter(poss_lemma %in% c("my", "our", "their")) |>
    dplyr::distinct()
  
  # ---- Helper: build phrase for an actor token_id in a sentence ----
  build_actor_phrase <- function(doc_id_, sentence_id_, actor_token_id_, parsed_df) {
    
    # 1) if part of a multiword entity, return the whole entity phrase
    eg <- token_to_entity_group |>
      dplyr::filter(doc_id == doc_id_, sentence_id == sentence_id_, token_id == actor_token_id_) |>
      dplyr::pull(entity_group_id)
    
    if (length(eg) == 1 && !is.na(eg)) {
      ent <- entity_groups |>
        dplyr::filter(doc_id == doc_id_, sentence_id == sentence_id_, entity_group_id == eg) |>
        dplyr::pull(entity_phrase)
      if (length(ent) == 1 && !is.na(ent)) {
        return(stringr::str_to_lower(ent))
      }
    }
    
    # 2) else: base token
    tok <- parsed_df |>
      dplyr::filter(doc_id == doc_id_, sentence_id == sentence_id_, token_id == actor_token_id_) |>
      dplyr::slice(1)
    
    if (nrow(tok) == 0) return(NA_character_)
    base <- stringr::str_to_lower(tok$token[1])
    
    # 3) if noun has possessive ("our X"), prepend it
    poss <- poss_map |>
      dplyr::filter(doc_id == doc_id_, sentence_id == sentence_id_, head_noun_id == actor_token_id_) |>
      dplyr::pull(poss_lemma)
    
    if (length(poss) >= 1 && !is.na(poss[1])) {
      return(paste(poss[1], base))
    }
    
    return(base)
  }
  
  # ---- Build verb-level actor table ----
  # (A) Active + passive subjects: nsubj/nsubjpass point to VERB via head_token_id
  verb_subjects <- parsed |>
    dplyr::filter(dep_rel %in% c("nsubj", "nsubjpass")) |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id  = head_token_id,   # verb token_id
      actor_id = token_id
    ) |>
    dplyr::distinct()
  
  # (B) Passive agents: agent token ("by") attached to VERB; pobj under that agent is the actor
  by_tokens <- parsed |>
    dplyr::filter(dep_rel == "agent") |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id = head_token_id,  # verb token_id
      by_id   = token_id
    ) |>
    dplyr::distinct()
  
  agent_pobj <- parsed |>
    dplyr::filter(dep_rel == "pobj") |>
    dplyr::transmute(
      doc_id, sentence_id,
      by_id     = head_token_id,
      actor_id  = token_id
    ) |>
    dplyr::distinct()
  
  verb_agents <- by_tokens |>
    dplyr::inner_join(agent_pobj, by = c("doc_id", "sentence_id", "by_id")) |>
    dplyr::select(doc_id, sentence_id, verb_id, actor_id) |>
    dplyr::distinct()
  
  verb_actors <- dplyr::bind_rows(verb_subjects, verb_agents) |>
    dplyr::distinct()

  # ---- FIX 1: Propagate subjects through auxiliary chains ----
  # When "We" → "will" → "help", propagate "We" to "help"
  # This addresses the root cause: codebook treats "will help" as ONE verb phrase
  aux_chains <- parsed |>
    dplyr::filter(dep_rel == "aux") |>
    dplyr::transmute(
      doc_id, sentence_id,
      aux_id = token_id,           # the modal/aux token
      main_verb_id = head_token_id  # the main verb it modifies
    )

  # For each main verb, find if its aux has a subject
  aux_subjects <- aux_chains |>
    dplyr::inner_join(
      verb_actors |> dplyr::select(doc_id, sentence_id, verb_id, actor_id),
      by = c("doc_id" = "doc_id", "sentence_id" = "sentence_id", "aux_id" = "verb_id")
    ) |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id = main_verb_id,  # propagate subject to main verb
      actor_id
    )

  # Combine direct subjects and propagated subjects
  verb_actors <- dplyr::bind_rows(verb_actors, aux_subjects) |>
    dplyr::distinct(doc_id, sentence_id, verb_id, .keep_all = TRUE)

  # ---- FIX 2: Propagate subjects through xcomp (infinitive complements) ----
  # "We want to help" → "help" (xcomp) should inherit "We" as actor
  xcomp_chains <- parsed |>
    dplyr::filter(dep_rel == "xcomp") |>
    dplyr::transmute(
      doc_id, sentence_id,
      xcomp_id = token_id,           # the infinitive verb
      parent_verb_id = head_token_id  # the controlling verb
    )

  xcomp_subjects <- xcomp_chains |>
    dplyr::inner_join(
      verb_actors |> dplyr::select(doc_id, sentence_id, verb_id, actor_id),
      by = c("doc_id" = "doc_id", "sentence_id" = "sentence_id", "parent_verb_id" = "verb_id")
    ) |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id = xcomp_id,
      actor_id
    )

  verb_actors <- dplyr::bind_rows(verb_actors, xcomp_subjects) |>
    dplyr::distinct(doc_id, sentence_id, verb_id, .keep_all = TRUE)

  # ---- FIX 3: Propagate subjects through conj (coordinated verbs) ----
  # "We help and protect" → "protect" (conj) should inherit "We" as actor
  conj_chains <- parsed |>
    dplyr::filter(dep_rel == "conj" & pos %in% c("VERB", "AUX")) |>
    dplyr::transmute(
      doc_id, sentence_id,
      conj_id = token_id,           # the conjunct verb
      head_verb_id = head_token_id  # the head verb
    )

  conj_subjects <- conj_chains |>
    dplyr::inner_join(
      verb_actors |> dplyr::select(doc_id, sentence_id, verb_id, actor_id),
      by = c("doc_id" = "doc_id", "sentence_id" = "sentence_id", "head_verb_id" = "verb_id")
    ) |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id = conj_id,
      actor_id
    )

  verb_actors <- dplyr::bind_rows(verb_actors, conj_subjects) |>
    dplyr::distinct(doc_id, sentence_id, verb_id, .keep_all = TRUE)

  # Add actor phrase
  verb_actors <- verb_actors |>
    dplyr::rowwise() |>
    dplyr::mutate(actor = build_actor_phrase(doc_id, sentence_id, actor_id, parsed)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      actor_norm = stringr::str_to_lower(stringr::str_replace(actor, "^the\\s+", "")),
      is_own =
        actor_norm %in% own_terms_norm |
        stringr::str_detect(actor_norm, "^(we|us|our|my)\\b")
    ) |>
    dplyr::filter(is_own) |>
    dplyr::select(doc_id, sentence_id, verb_id, actor, actor_norm)
  
  if (nrow(verb_actors) == 0) {
    if (bootstrap) return(tibble::tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0))
    return(tibble::tibble(P = 0, OP = 0))
  }
  
  # ---- Pull the verbs we will classify (verb_id = token_id) ----
  verbs <- parsed |>
    dplyr::filter(pos %in% c("VERB", "AUX")) |>
    dplyr::transmute(
      doc_id, sentence_id,
      verb_id = token_id,
      lemma   = stringr::str_to_lower(lemma),
      token   = stringr::str_to_lower(token),
      tag, dep_rel
    ) |>
    dplyr::inner_join(verb_actors, by = c("doc_id", "sentence_id", "verb_id")) |>
    dplyr::inner_join(sentences, by = c("doc_id", "sentence_id"))
  
  if (nrow(verbs) == 0) {
    if (bootstrap) return(tibble::tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0))
    return(tibble::tibble(P = 0, OP = 0))
  }
  
  # ---- Classifier (your logic, but no sentence-level dropping) ----
  classify_power <- function(lemma, tag, sentence) {
    if (is.na(sentence) || is.na(lemma)) return(NA_character_)
    
    # drop auxiliaries/modals (keep classification focused on action verbs)
    if (lemma %in% c("will","would","shall","should","can","could","may","might",
                     "must","do","does","did","have","has","had",
                     "be","am","is","are","was","were","being","been")) {
      return(NA_character_)
    }
    
    # optional: drop bare infinitives like "to help" when it’s just complement of "want/seek"
    # (kept lightweight: your previous check was fragile; this avoids false drops)
    # If you want to keep it, do it robustly via dependency patterns later.
    
    a <- stringr::str_detect(sentence, regex_a)
    b <- stringr::str_detect(sentence, regex_b)
    c <- stringr::str_detect(sentence, regex_c)
    d <- stringr::str_detect(sentence, regex_d)
    e <- stringr::str_detect(sentence, regex_e)
    f <- stringr::str_detect(sentence, regex_f)
    
    if (a | b | c | d | e | f | lemma %in% p_keywords) "P" else "OP"
  }
  
  verb_classified <- verbs |>
    dplyr::rowwise() |>
    dplyr::mutate(power = classify_power(lemma, tag, sentence)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(power))
  
  # ---- Output ----
  if (nrow(verb_classified) == 0) {
    
    if (bootstrap) return(tibble::tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0))
    return(tibble::tibble(P = 0, OP = 0))
    
  } else {
    
    if (!bootstrap) {
      
      output <- verb_classified |>
        dplyr::count(power) |>
        tidyr::complete(power = c("P","OP"), fill = list(n = 0)) |>
        tidyr::pivot_wider(names_from = power, values_from = n) |>
        dplyr::select(P, OP)
      
      return(output)
      
    } else {
      
      # ---- Bootstrap by sentence resampling ----
      sentence_summary <- verb_classified |>
        dplyr::group_by(doc_id, sentence_id) |>
        dplyr::count(power) |>
        tidyr::complete(power = c("P","OP"), fill = list(n = 0)) |>
        tidyr::pivot_wider(names_from = power, values_from = n) |>
        dplyr::ungroup() |>
        dplyr::select(doc_id, sentence_id, P, OP)
      
      all_sentence_ids <- sentences |>
        dplyr::distinct(doc_id, sentence_id)
      
      boot_results <- purrr::map_dfr(1:B, ~{
        sampled <- all_sentence_ids |>
          dplyr::slice_sample(n = nrow(all_sentence_ids), replace = TRUE)
        
        sampled |>
          dplyr::inner_join(sentence_summary, by = c("doc_id","sentence_id")) |>
          dplyr::summarise(P = sum(P), OP = sum(OP))
      })
      
      tibble::tibble(
        meanP  = mean(boot_results$P),
        meanOP = mean(boot_results$OP),
        varP   = stats::var(boot_results$P),
        varOP  = stats::var(boot_results$OP)
      )
    }
  }
}

# get_power <- function(own_entity, text, bootstrap = FALSE, B = 1000) {
#   
#   ## Define own and other entities
#   own_terms <- c(unique(tolower(own_entity)), 
#                  "we", "our nation", "our country", "us",
#                  "our state", "our government", "our institution", 
#                  "our group", "our party", "our people",
#                  "i", "my nation", "my country", "me",
#                  "my state", "my government", "my institution",
#                  "my group", "my party", "my people")
#   
#   own_entity <- unique(c(tolower(own_entity), expand_aliases_country(own_entity)))
#   
#   ## Define own and other entities
#   other_terms <- c(setdiff(all_entities_corpus, tolower(own_terms)),
#                    "they", "their nation", "their country", 
#                    "their state", "their government", "their institution")
#   
#   # Parse text
#   parsed <- spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
#   
#   sentences <- parsed |> 
#     group_by(sentence_id) |> 
#     summarise(sentence = paste(token, collapse = " "), .groups = "drop")
#   
#   # Step 1: Get multiword named entities
#   parsed_entity_grouped <- parsed |>
#     mutate(ent_group = ifelse(!is.na(entity) & entity != "", 1, NA)) |>
#     group_by(doc_id, sentence_id) |>
#     mutate(entity_group_id = data.table::rleid(ent_group)) |>
#     ungroup() |>
#     mutate(entity_group_id = ifelse(is.na(ent_group), NA, entity_group_id))
#   
#   multiword_entities <- parsed_entity_grouped |>
#     filter(!is.na(entity_group_id)) |>
#     group_by(doc_id, sentence_id, entity_group_id) |>
#     summarise(entity = paste(token, collapse = " "), .groups = "drop")
#   
#   # Step 2: Get subjects and join with named entities (new, to incorporate possessives)  
#   subjects <- parsed_entity_grouped |>
#     filter(dep_rel %in% c("poss", "nsubj", "nsubjpass")) |>
#     select(-entity) |>
#     left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
#     mutate(
#       poss_pronoun = lag(lemma),
#       poss_dep = lag(dep_rel),
#       has_possessive = poss_pronoun %in% c("my", "our", "their") & poss_dep == "poss",
#       subject_phrase = case_when(
#         has_possessive ~ paste(poss_pronoun, lemma),        # e.g., "our concern"
#         !is.na(entity) ~ entity,                             # multiword entities
#         TRUE ~ token
#       )
#     ) |>
#     filter(dep_rel %in% c("nsubj", "nsubjpass") | (dep_rel == "poss" & has_possessive)) |>
#     select(doc_id, sentence_id, subject_phrase, head_token_id) |>
#     distinct() |>
#     group_by(doc_id, sentence_id) |>
#     summarise(subject = tolower(first(subject_phrase)),
#               head_token_id = first(head_token_id), .groups = "drop")
#   
#   sentences <- parsed |>
#     group_by(sentence_id) |>
#     summarise(sentence = paste(token, collapse = " "), .groups = "drop") |>
#     left_join(subjects, by = "sentence_id") |>
#     mutate(entity_type = ifelse(subject %in% own_terms, "own", NA_character_))
#   
#   ## Passive voice: Handle passive voice: "by us" etc. -----
#   agent_tokens <- parsed |>
#     filter(dep_rel == "agent") |>
#     select(doc_id, sentence_id, agent_token_id = token_id)
#   
#   agent_objects <- parsed |>
#     filter(dep_rel == "pobj" & tolower(token) %in% c("us", "we", "our", "my")) |>
#     rename(child_token_id = token_id, agent_actor = token)
#   
#   agents <- agent_tokens |>
#     left_join(agent_objects, 
#               by = c("doc_id", "sentence_id", "agent_token_id" = "head_token_id")) |>
#     filter(!is.na(agent_actor)) |>
#     select(doc_id, sentence_id, agent_actor)
#   
#   sentences_with_agents <- sentences |>
#     left_join(agents, by = "sentence_id")
#   
#   ## Identify sentences with own subjects or own agents -----
#   # own_terms <- c(own_terms, "me", "my", "us", "our") # Extend the terms to incorporate us, me etc
#   
#   own_terms_regex <- paste0(stringr::str_replace_all(tolower(own_terms), "([\\W])", "\\\\\\1"),
#                             collapse = "|")
#   
#   sentences_with_agents <- sentences_with_agents |>
#     mutate(
#       subject_norm   = subject,
#       agent_norm     = str_to_lower(agent_actor),
#       starts_with_own_pron = str_detect(subject_norm, "^(we|us|our|my)\\b"),
#       starts_with_own_name = if (nchar(own_terms_regex) > 0)
#         str_detect(subject_norm, paste0("^(?:", own_terms_regex, ")\\b")) else FALSE,
#       exact_own_match = subject_norm %in% tolower(own_terms)
#     )
#   
#   own_sentences <- sentences_with_agents |>
#     filter(
#       exact_own_match | starts_with_own_pron | starts_with_own_name |
#         agent_norm %in% tolower(own_terms)
#     )
#   
#   # own_sentences <- sentences_with_agents |>
#   #   filter(
#   #     str_remove(subject, "^the\\s+") %in% own_terms |
#   #       tolower(agent_actor) %in% own_terms
#   #   )
#   
#   verbs <- parsed |>
#     filter(pos == "VERB") |>
#     inner_join(
#       own_sentences |> select(sentence_id, head_token_id),
#       by = c("sentence_id", "token_id" = "head_token_id")
#     )
#   
#   if (nrow(own_sentences) == 0 | nrow(verbs) == 0) {
#     if (bootstrap) {
#       return(tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0))
#     } else {
#       return(tibble(P = 0, OP = 0))
#     }
#   } else {
#     
#     classify_power <- function(lemma, token, token_id, sentence, tag, dep_rel, sentence_id, parsed_df) {
#       if (is.na(sentence_id) || is.na(sentence)) return(NA_character_)
#       
#       lemma <- tolower(lemma)
#       token <- tolower(token)
#       
#       sentence_tokens <- parsed_df %>% filter(sentence_id == !!sentence_id)
#       idx_all <- which(sentence_tokens$token == token & sentence_tokens$tag == "VB")
#       idx <- which(sentence_tokens$token_id == token_id)
#       if (length(idx) != 1 && length(idx_all) == 1) idx <- idx_all
#       if (length(idx) != 1) return(NA_character_)
#       
#       if (lemma %in% c("will", "would", "shall", "should", "can", "could", "may", "might", 
#                        "must", "do", "does", "did", "have", "has", "had", 
#                        "be", "am", "is", "are", "was", "were", "being", "been")) {
#         return(NA_character_)
#       }
#       
#       if (tag == "VB" && idx > 2) {
#         prev1 <- tolower(sentence_tokens$lemma[idx - 1])
#         prev2 <- tolower(sentence_tokens$lemma[idx - 2])
#         if (prev1 == "to" && prev2 %in% c("help", "support", "assist", "aid", "encourage", "allow", "enable", "want", "seek")) {
#           return(NA_character_)
#         }
#       }
#       
#       subject_tokens <- parsed_df |>
#         filter(sentence_id == !!sentence_id,
#                dep_rel %in% c("nsubj", "nsubjpass"),
#                head_token_id == !!token_id)
#       
#       if (nrow(subject_tokens) > 0) {
#         subj_lemma <- tolower(subject_tokens$lemma[1])
#         if (subj_lemma %in% c("they", "them", "their", "he", "she", "it")) {
#           return(NA_character_)
#         }
#       }
#       
#       a <- str_detect(sentence, regex_a)
#       b <- str_detect(sentence, regex_b)
#       c <- str_detect(sentence, regex_c)
#       d <- str_detect(sentence, regex_d)
#       e <- str_detect(sentence, regex_e)
#       f <- str_detect(sentence, regex_f)
#       
#       if (a | b | c | d | e | f | lemma %in% p_keywords) {
#         return("P")
#       } else {
#         return("OP")
#       }
#     }
#     
#     verbs_joined <- verbs |>
#       left_join(own_sentences, by = "sentence_id") |>
#       filter(!is.na(sentence))
#     
#     verb_classified <- verbs_joined |>
#       rowwise() |>
#       mutate(
#         power = classify_power(lemma, token, token_id, sentence, tag, dep_rel, sentence_id, parsed)
#       ) |>
#       ungroup() |>
#       filter(!is.na(power))
#     
#     output <- verb_classified |>
#       count(power) |>
#       tidyr::complete(power = c("P", "OP"), fill = list(n = 0)) |>
#       tidyr::pivot_wider(names_from = power, values_from = n) |>
#       dplyr::select(P, OP)
#     
#     if (bootstrap) {
#       
#       if (nrow(verb_classified) == 0) {
#         
#         btres <- tibble(meanP = 0, meanOP = 0, varP = 0, varOP = 0)
#         
#       } else {
#       
#         sentence_summary <- verb_classified |>
#           group_by(sentence_id) |>
#           count(power) |>
#           tidyr::complete(power = c("P", "OP"), fill = list(n = 0)) |>
#           tidyr::pivot_wider(names_from = power, values_from = n) |>
#           dplyr::select(sentence_id, P, OP)
#         
#         boot_results <- purrr::map_dfr(1:B, ~{
#           sampled_id <- sentences$sentence_id[sample(nrow(sentences), replace = TRUE)]
#           sample_df <- tibble(sentence_id = sampled_id)
#           
#           own_sentences_sampled <- sample_df |> 
#             inner_join(sentence_summary, by = "sentence_id", relationship = "many-to-many")
#           
#           if (nrow(own_sentences_sampled) == 0) {
#             tibble(P = 0, OP = 0)
#           } else {
#             
#             own_sentences_sampled |> 
#               summarise(
#                 P = sum(P), 
#                 OP = sum(OP)
#               )
#           }
#         })
#         
#         btres <- boot_results |>
#           summarize(meanP = mean(P), meanOP = mean(OP)) |>
#           mutate(varP = var(boot_results$P), varOP = var(boot_results$OP))
#       
#       }
#       
#       return(btres)
#       
#     } else {
#       
#       return(output)
#       
#     }
#   }
# }

# Check with test cases ------

# # P
# test_text1 <- "We will help to improve relations between South Africa and Black Africa."
# test_own_entities1 <- c("Malawi")
# result1 <- get_power(test_own_entities1, test_text1)
# result1
# 
# # P
# test_text2 <- "We certainly hope that they will come to their senses."
# test_own_entities2 <- c("Malawi")
# result2 <- get_power(test_own_entities2, test_text2)
# result2
# 
# # P
# test_text3 <- "Our government threatened to act roughly because of their earlier behavior."
# test_own_entities3 <- c("Malawi")
# result3 <- get_power(test_own_entities3, test_text3)
# result3
# 
# # P
# test_text4 <- "Our government has arranged some relief for them."
# test_own_entities4 <- c("Malawi")
# result4 <- get_power(test_own_entities4, test_text4)
# result4
# 
# # P
# test_text5 <- "If we take this action, African unity will be raised to a new high."
# test_own_entities5 <- c("Malawi")
# result5 <- get_power(test_own_entities5, test_text5)
# result5
# 
# # P
# test_text6 <- "I emphasize the significance of our act."
# test_own_entities6 <- c("Malawi")
# result6 <- get_power(test_own_entities6, test_text6)
# result6
# 
# # OP
# test_text7 <- "We live with that problem all the time."
# test_own_entities7 <- c("Malawi")
# result7 <- get_power(test_own_entities7, test_text7)
# result7
# 
# # P
# test_text8 <- "The nature of their relationship was arranged by us."
# test_own_entities8 <- c("Malawi")
# result8 <- get_power(test_own_entities8, test_text8)
# result8
