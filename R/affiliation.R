# Affiliation, deterministic

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

# Regex
regex_a <- regex(
  "\\b(friendship|friend(s)?|sympathy|support|cooperate|cooperation|unity|peaceful|amicable|cordial|alliance|ally|solidarity|harmony|collaboration|collaborate|partnership|camaraderie|goodwill|forgive|accept|welcome)\\b",
  ignore_case = TRUE)
regex_b <- regex(
  "\\b(agree|agreement|resolve|negotiat|dialog(ue)?|talk(s)?|compromise|concile|restore|reconcile|rapprochement|bridge|resume|resum|reunite|settle|mend|regret)\\b",
  ignore_case = TRUE)
regex_c <- regex(
  "\\b(summit|conference|forum|visit|meeting|dialogue|dialog|talk(s)?|joint|coalition|collaborate|together|program)\\b",
  ignore_case = TRUE)
regex_d <- regex(
  "\\b(help|aid|assist|support|relief|care|comfort|console|concern|concerned|compassion|charit|nurtur|alleviat|suffer|plight|victim|refugee)\\b",
  ignore_case = TRUE)

get_aff <- function(own_entity, text, bootstrap = FALSE, B = 1000) {
  
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
  parsed <- spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  
  sentences <- parsed |> 
    group_by(sentence_id) |> 
    summarise(sentence = paste(token, collapse = " "), .groups = "drop")
  
  # Step 1: Get multiword named entities
  parsed_entity_grouped <- parsed |>
    mutate(ent_group = ifelse(!is.na(entity) & entity != "", 1, NA)) |>
    group_by(doc_id, sentence_id) |>
    mutate(entity_group_id = data.table::rleid(ent_group)) |>
    ungroup() |>
    mutate(entity_group_id = ifelse(is.na(ent_group), NA, entity_group_id))
  
  multiword_entities <- parsed_entity_grouped |>
    filter(!is.na(entity_group_id)) |>
    group_by(doc_id, sentence_id, entity_group_id) |>
    summarise(entity = paste(token, collapse = " "), .groups = "drop")
  
  # Step 2: Get subjects and join with named entities (new, to incorporate possessives)  
  subjects <- parsed_entity_grouped |>
    filter(dep_rel %in% c("poss", "nsubj", "nsubjpass")) |>
    select(-entity) |>
    left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
    mutate(
      poss_pronoun = lag(lemma),
      poss_dep = lag(dep_rel),
      has_possessive = poss_pronoun %in% c("my", "our", "their") & poss_dep == "poss",
      subject_phrase = case_when(
        has_possessive ~ paste(poss_pronoun, lemma),        # e.g., "our concern"
        !is.na(entity) ~ entity,                             # multiword entities
        TRUE ~ token
      )
    ) |>
    filter(dep_rel %in% c("nsubj", "nsubjpass") | (dep_rel == "poss" & has_possessive)) |>
    select(doc_id, sentence_id, subject_phrase, head_token_id) |>
    distinct() |>
    group_by(doc_id, sentence_id) |>
    summarise(subject = tolower(first(subject_phrase)),
              head_token_id = first(head_token_id), .groups = "drop")
  
  sentences <- parsed |>
    group_by(sentence_id) |>
    summarise(sentence = paste(token, collapse = " "), .groups = "drop") |>
    left_join(subjects, by = "sentence_id") |>
    mutate(entity_type = ifelse(subject %in% own_terms, "own", NA_character_))
  
  ## Passive voice: Handle passive voice: "by us" etc. -----
  agent_tokens <- parsed |>
    filter(dep_rel == "agent") |>
    select(doc_id, sentence_id, agent_token_id = token_id)
  
  agent_objects <- parsed |>
    filter(dep_rel == "pobj" & tolower(token) %in% c("us", "we", "our", "my")) |>
    rename(child_token_id = token_id, agent_actor = token)
  
  agents <- agent_tokens |>
    left_join(agent_objects, 
              by = c("doc_id", "sentence_id", "agent_token_id" = "head_token_id")) |>
    filter(!is.na(agent_actor)) |>
    select(doc_id, sentence_id, agent_actor)
  
  sentences_with_agents <- sentences |>
    left_join(agents, by = "sentence_id")
  
  ## Identify sentences with own subjects or own agents -----
  # own_terms <- c(own_terms, "me", "my", "us", "our") # Extend the terms to incorporate us, me etc
  
  own_terms_regex <- paste0(stringr::str_replace_all(tolower(own_terms), "([\\W])", "\\\\\\1"),
                            collapse = "|")
  
  sentences_with_agents <- sentences_with_agents |>
    mutate(
      subject_norm   = subject,
      agent_norm     = str_to_lower(agent_actor),
      starts_with_own_pron = str_detect(subject_norm, "^(we|us|our|my)\\b"),
      starts_with_own_name = if (nchar(own_terms_regex) > 0)
        str_detect(subject_norm, paste0("^(?:", own_terms_regex, ")\\b")) else FALSE,
      exact_own_match = subject_norm %in% tolower(own_terms)
    )
  
  own_sentences <- sentences_with_agents |>
    filter(
      exact_own_match | starts_with_own_pron | starts_with_own_name |
        agent_norm %in% tolower(own_terms)
    )
  
  # own_sentences <- sentences_with_agents |>
  #   filter(
  #     str_remove(subject, "^the\\s+") %in% own_terms |
  #       tolower(agent_actor) %in% own_terms
  #   )
  
  verbs <- parsed |>
    filter(pos == "VERB") |>
    inner_join(
      own_sentences |> select(sentence_id, head_token_id),
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
    
    cond_a <- str_detect(sentence, regex_a)
    cond_b <- str_detect(sentence, regex_b)
    cond_c <- str_detect(sentence, regex_c)
    cond_d <- str_detect(sentence, regex_d)
    
    if (cond_a | cond_b | cond_c | cond_d) {
      return("A")
    } else {
      return("OA")
    }
  }
  
  if (nrow(own_sentences) == 0 | nrow(verbs) == 0) {
    if (bootstrap) {
      return(tibble(meanA = 0, meanOA = 0, varA = 0, varOA = 0))
    } else {
      return(tibble(A = 0, OA = 0))
    }
  }
  
  verbs_joined <- verbs |>
    left_join(own_sentences, by = "sentence_id") |>
    filter(!is.na(sentence))
  
  verb_classified <- verbs_joined |>
    rowwise() |>
    mutate(
      affiliation = classify_aff(lemma, sentence)
    ) |>
    ungroup() |>
    filter(!is.na(affiliation))
  
  output <- verb_classified |>
    count(affiliation) |>
    tidyr::complete(affiliation = c("A", "OA"), fill = list(n = 0)) |>
    tidyr::pivot_wider(names_from = affiliation, values_from = n)
  
  if (bootstrap) {
    
    if (nrow(verb_classified) == 0) {
      
      btres <- tibble(meanA = 0, meanOA = 0, varA = 0, varOA = 0)
      
    } else {
    
    sentence_summary <- verb_classified |>
      group_by(sentence_id) |>
      count(affiliation) |>
      tidyr::complete(affiliation = c("A", "OA"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = affiliation, values_from = n)
    
    boot_results <- purrr::map_dfr(1:B, ~{
      sampled_id <- sentences$sentence_id[sample(nrow(sentences), replace = TRUE)]
      sample_df <- tibble(sentence_id = sampled_id)
      
      own_sentences_sampled <- sample_df |> 
        inner_join(sentence_summary, by = "sentence_id", relationship = "many-to-many")
      
      if (nrow(own_sentences_sampled) == 0) {
        
        tibble(A = 0, OA = 0)
        
      } else {
        
        own_sentences_sampled |> 
          summarise(
            A = sum(A), 
            OA = sum(OA)
          )
        
      }
    })
    
    btres <- boot_results |>
                   summarise(meanA = mean(A), meanOA = mean(OA),
                             varA = var(A), varOA = var(OA))
    
    }
    
    return(btres)
    
  } else {
    
    return(output)
    
  }
}

# Check with test cases ------

# # A
# test_text1 <- "If our concern for a settlement were only shared by the Zambians."
# test_own_entities1 <- c("Malawi")
# result1 <- get_aff(test_own_entities1, test_text1)
# result1
# 
# # A
# test_text2 <- "Our two groups will always live together in friendship."
# test_own_entities2 <- c("Malawi")
# result2 <- get_aff(test_own_entities2, test_text2)
# result2
# 
# # A
# test_text3 <- "I would hope that South Africa will join us in sponsoring the program."
# test_own_entities3 <- c("Malawi")
# result3 <- get_aff(test_own_entities3, test_text3)
# result3
# 
# # A
# test_text4 <- "We have offered our help to the drought victims in their time of need."
# test_own_entities4 <- c("Malawi")
# result4 <- get_aff(test_own_entities4, test_text4)
# result4
# 
# # OA
# test_text5 <- "May we offer congratulations to Ghana for reaching its objective."
# test_own_entities5 <- c("Malawi")
# result5 <- get_aff(test_own_entities5, test_text5)
# result5
# 
# # OA
# test_text6 <- "We seek peace in Africa."
# test_own_entities6 <- c("Malawi")
# result6 <- get_aff(test_own_entities6, test_text6)
# result6
# 
# # A
# test_text7 <- "We will remain friends hopefully regardless of South Africa's behavior."
# test_own_entities7 <- c("Malawi")
# result7 <- get_aff(test_own_entities7, test_text7)
# result7
