#' leadeR: Profiling Leaders at a Distance
#'
#' The leadeR package profiles political leaders using text analysis,
#' implementing Leadership Trait Analysis (LTA) and Operational Code
#' Analysis (OCA). You provide text data and the package performs the
#' analyses.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @importFrom spacyr spacy_parse
#' @importFrom data.table rleid
#' @importFrom tibble tibble
#' @importFrom countries list_countries
#' @importFrom countrycode countryname
#' @importFrom purrr map_dfr
#' @importFrom stringi stri_trans_general
#' @importFrom stats var na.omit
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTEs for dplyr/tidyr NSE column references
utils::globalVariables(c(
  "A", "IC", "N", "OA", "OC", "ON", "OP", "OS", "OSC", "P", "S", "SC",
  "actor", "actor_id", "actor_norm", "affiliation", "agent_actor",
  "agent_norm", "agent_token", "amod_ner", "amod_phrase", "by_ner",
  "by_phrase", "conf", "conj_head", "conj_id", "country.name.en",
  "counts", "ctrl", "dep_rel", "doc_id", "ent_flag", "ent_group",
  "ent_phrase", "entity", "entity.x", "entity.y", "entity_clean",
  "entity_group_id", "entity_type", "exact_own_match", "first_token_id",
  "future_aux", "has_against", "has_conditional", "has_modal",
  "has_possessive", "head_ner", "head_noun", "head_token_id", "in_quote",
  "inf_lemma", "inf_tag", "is_conditional", "is_future", "is_gpe",
  "is_let_us", "is_own", "is_pronoun", "is_quote_char", "is_self_ref_noun",
  "label", "lemma", "lemma_full", "lemma_lower", "main_verb_id", "mod",
  "modal_aux", "ner_type_clean", "nom_head", "noun_lemma", "noun_phrase",
  "noun_phrase_tmp", "obj_head", "object_phrase", "opc_score", "particle",
  "phrase", "pos", "poss_dep", "poss_lemma", "poss_ner", "poss_phrase",
  "poss_pronoun", "power", "prep_head", "prep_token", "prev_token",
  "quote_cumsum", "quote_open", "sent_ner", "sent_subject", "sentence",
  "sentence_id", "sentence_text", "starts_with_own_name",
  "starts_with_own_pron", "subject", "subject_clean", "subject_ner_type",
  "subject_no_article", "subject_norm", "subject_phrase", "tag", "tags",
  "token", "token_id", "token_lower", "verb_dep", "verb_head", "verb_id",
  "verb_lemma", "verb_tag", "verb_token", "xcomp_id"
))
