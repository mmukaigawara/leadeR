# Regex patterns for power conditions
regex_a <- stringr::regex("\\b(wipe( them)? out|defeat(ed)?|destroy(ed)?|attack(ed)?|assault(ed)?|threat(en|ened)?|accus(ed|e)?|reprimand(ed)?|retaliat(ed|e)?|blame(d)?)\\b", ignore_case = TRUE)
regex_b <- stringr::regex("\\b(help(ed)?|assist(ed)?|support(ed)?|aid(ed)?|encourag(ed|e)?|advise(d)?)\\b.*\\b(them|another|others|recipient|nation(s)?|group(s)?)\\b", ignore_case = TRUE)
regex_c <- stringr::regex("\\b(arrang(ed)?|regulat(ed|e)?|control(led)?|determin(e|ed)?|check(ed)?|investigat(ed|e)?|search(ed)?|monitor(ed)?)\\b.*\\b(them|their|another|lives|actions?)\\b", ignore_case = TRUE)
regex_d <- stringr::regex("\\b(influenc(e|ed)?|persuad(e|ed)?|convinc(e|ed)?|brib(e|ed)?|argu(e|ed)?|suggest(ed)?|hint(ed)?|pressure(d)?|advocat(e|ed)?|hope(d)?)\\b.*\\b(them|recipient|group|nation|he|she|you)\\b", ignore_case = TRUE)
regex_e <- stringr::regex("\\b(impress(ed)?|prestig(e|ious)?|display(ed)?|show(ed|case(d)?)?|notoriety|fame|glory|take|took|taken|honor(ed)?)\\b", ignore_case = TRUE)
regex_f <- stringr::regex("\\b(vindicat(ed)?|superior|status|reputation|weak(ness)?|inferior|strength|power(ful)?|prestige|blame(d)?|emphasiz(e|ed)|criticized|guarantee(d)?)\\b", ignore_case = TRUE)

p_keywords <- c(
  # Original codebook list (from LTA codebook line 47)
  "warn", "help", "hint", "stop", "suggest", "impress", "promise", "worried",
  "attack", "demand", "refuse", "accuse", "protect", "defend", "retaliate", "blame",
  "support", "threaten", "guard", "achieve", "instigate", "arrange", "hope",
  # From codebook examples
  "take",       # Example 5: "If we take this action" → P (condition e)
  "emphasize"   # Example 6: "I emphasize the significance" → P (condition f)
)

#' Compute Power Score (LTA)
#'
#' Classifies verbs in text as reflecting Power (P) or Other Power (OP)
#' based on the Leadership Trait Analysis codebook.
#'
#' @param own_entity Character vector of the speaker's country/entity name(s).
#' @param text Character string of the speech text to analyse.
#' @param bootstrap Logical; if TRUE, return bootstrap means and variances.
#' @param B Number of bootstrap replications (default 1000).
#' @return A tibble with columns P and OP (or meanP, meanOP, varP, varOP if
#'   bootstrap = TRUE).
#' @export
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
    # Force evaluation of parameters
    doc_id_ <- doc_id_

    sentence_id_ <- sentence_id_
    actor_token_id_ <- actor_token_id_

    # 1) if part of a multiword entity, return the whole entity phrase
    eg_rows <- token_to_entity_group[
      token_to_entity_group$doc_id == doc_id_ &
      token_to_entity_group$sentence_id == sentence_id_ &
      token_to_entity_group$token_id == actor_token_id_, ]

    if (nrow(eg_rows) == 1) {
      eg <- eg_rows$entity_group_id[1]
      if (!is.na(eg)) {
        ent_rows <- entity_groups[
          entity_groups$doc_id == doc_id_ &
          entity_groups$sentence_id == sentence_id_ &
          entity_groups$entity_group_id == eg, ]
        if (nrow(ent_rows) == 1 && !is.na(ent_rows$entity_phrase[1])) {
          return(stringr::str_to_lower(ent_rows$entity_phrase[1]))
        }
      }
    }

    # 2) else: base token
    tok <- parsed_df[
      parsed_df$doc_id == doc_id_ &
      parsed_df$sentence_id == sentence_id_ &
      parsed_df$token_id == actor_token_id_, ]

    if (nrow(tok) == 0) return(NA_character_)
    base <- stringr::str_to_lower(tok$token[1])

    # 3) if noun has possessive ("our X"), prepend it
    poss_rows <- poss_map[
      poss_map$doc_id == doc_id_ &
      poss_map$sentence_id == sentence_id_ &
      poss_map$head_noun_id == actor_token_id_, ]

    if (nrow(poss_rows) >= 1 && !is.na(poss_rows$poss_lemma[1])) {
      return(paste(poss_rows$poss_lemma[1], base))
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

    # optional: drop bare infinitives like "to help" when it's just complement of "want/seek"
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
