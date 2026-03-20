# Task orientation, deterministic

library(spacyr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)

#spacyr::spacy_initialize(model = "en_core_web_sm")

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

get_task <- function(text, bootstrap = FALSE, B = 1000) {

  parsed <- spacy_parse(text, dependency = TRUE, lemma = TRUE)

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
      group_by(sentence_id) |>
      summarise(
        tags = list(classify_sentence(pick(everything()))),
        TI = sum(unlist(tags) == "TI"),
        IP = sum(unlist(tags) == "IP")
      ) |>
      dplyr::select(-tags)
    
  }
  
  results <- classify_all(parsed)
  
  if (length(results) == 0) {
    
    if (bootstrap) {
      
      return(tibble(meanTI = 0, meanIP = 0, varTI = 0, varIP = 0))
      
    } else {
      
      return(tibble(TI = 0, IP = 0))
      
    }
    
  } else {
    
    if (bootstrap) {
      
      boot_counts <- replicate(B, {
        
        sampled_id <- unique(parsed$sentence_id)[sample(length(unique(parsed$sentence_id)), replace = TRUE)]
        sample_df <- tibble(sentence_id = sampled_id)
        
        own_sentences_sampled <- sample_df |> 
          inner_join(results, by = "sentence_id", relationship = "many-to-many")
        
        c(TI = sum(own_sentences_sampled$TI), 
          IP = sum(own_sentences_sampled$IP))
        
      }, simplify = TRUE)
      
      boot_df <- as.data.frame(t(boot_counts))
      return(tibble(
        meanTI = mean(boot_df$TI), varTI = var(boot_df$TI),
        meanIP = mean(boot_df$IP), varIP = var(boot_df$IP)
      ))
      
    } else {
      
      return(tibble(TI = sum(results$TI), IP = sum(results$IP)))
      
    }
    
  }
  
}

# Task orientation test cases ------

# # TI x 2
# test_text1 <- "We must develop a better plan to solve this issue."
# result1 <- get_task(test_text1)
# result1
# 
# # IP x 3
# test_text2 <- "We appreciate your cooperation and support."
# result2 <- get_task(test_text2)
# result2
# 
# # Mixed: TI x 2, IP x 2
# test_text3 <- "We initiated a new program and hope to support those in need."
# result3 <- get_task(test_text3)
# result3
# 
# # Negated task (not coded)
# test_text4 <- "We will not attempt to solve this problem."
# result4 <- get_task(test_text4)
# result4
# 
# # Quoted (not coded)
# test_text5 <- "He said, 'We must work together and hope for peace.'"
# result5 <- get_task(test_text5)
# result5
