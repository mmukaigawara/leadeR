# Self-confidence - IMPROVED VERSION
# Key changes:
# 1. Significantly expanded SC detection patterns for all three conditions
# 2. Added verb-based detection for condition (a) - self as instigator
# 3. Better authority/leadership phrase detection

pronouns <- c("i", "me", "my", "mine", "myself")

#' Compute self-confidence scores
#'
#' Classifies first-person pronoun occurrences in the text as self-confident
#' (SC) or other-self-confident (OSC) based on sentence-level context patterns
#' covering three conditions: self as instigator, self as authority, and self
#' as recipient of positive recognition.
#'
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical; if \code{TRUE}, return bootstrapped mean and
#'   variance estimates. Default is \code{FALSE}.
#' @param B Integer; number of bootstrap replicates. Default is 1000.
#' @return A one-row \code{\link[tibble]{tibble}}. When \code{bootstrap = FALSE},
#'   columns are \code{SC} and \code{OSC}. When \code{bootstrap = TRUE}, columns
#'   are \code{meanSC}, \code{meanOSC}, \code{varSC}, \code{varOSC}.
#' @export
get_conf <- function(text, bootstrap = FALSE, B = 1000) {

  # Parse text to tokens with spaCy
  parsed <- spacyr::spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)

  # Identify quoted spans to skip pronouns inside quotes
  parsed <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::mutate(
      is_quote = FALSE,
      quote_open = token %in% c('"', "'", "\u201C", "\u201D", "\u2018", "\u2019"),
      quote_cumsum = cumsum(quote_open)) |>
    dplyr::mutate(in_quote = (quote_cumsum %% 2 == 1)) |>
    dplyr::ungroup()

  # Reconstruct sentences
  sentences <- parsed |>
    dplyr::group_by(doc_id, sentence_id) |>
    dplyr::summarise(sentence = paste(token, collapse = " "), .groups = "drop")

  # Filter for first-person pronouns outside quotes
  pronoun_df <- parsed |>
    dplyr::filter(tolower(token) %in% pronouns, !in_quote) |>
    dplyr::left_join(sentences, by = c("doc_id", "sentence_id"))

  if (nrow(pronoun_df) == 0) {
    if (bootstrap) {
      return(tibble::tibble(meanSC = 0, meanOSC = 0, varSC = 0, varOSC = 0))
    } else {
      return(tibble::tibble(SC = 0, OSC = 0))
    }
  } else {

    classify_conf <- function(sentence) {
      s <- tolower(sentence)

      # Condition A: Self as instigator of activity
      a_patterns <- c(
        "\\bi (will|shall|am going to|plan to|intend to|propose to|am prepared to|am ready to|am determined to|have decided to)\\b",
        "\\bi (think|believe|feel|am convinced|am confident|am certain|am sure) (we|that we|that this|that the|this|it)\\b",
        "\\bi (want|would like|hope|expect|anticipate) (to|us to|that we|that this)\\b",
        "\\bi (have|'ve) (decided|determined|resolved|concluded|made up my mind)\\b",
        "\\bi (am|'m) (planning|proposing|recommending|suggesting|urging|calling for)\\b",
        "\\bi (recommend|suggest|propose|urge|call for|advocate|favor|support)\\b",
        "\\bmy (plan|proposal|recommendation|suggestion|initiative|goal|objective|vision|agenda|strategy)\\b",
        "\\bthis is my (plan|proposal|initiative|goal|vision)\\b",
        "\\bunder my (leadership|direction|guidance|administration)\\b",
        "\\bi (will|shall) (do|make|take|give|provide|ensure|see to it)\\b",
        "\\bi (have|'ve) (taken|made|given|done|initiated|started|begun|launched)\\b",
        "\\bi (addressed|visited|met|spoke|talked|presented|announced|signed|called|ordered|directed|led|conducted|organized|hosted|convened|chaired|launched|initiated|established|created|built|developed|achieved|accomplished|completed|finished|delivered|sent|submitted|issued)\\b",
        "\\bi (address|visit|meet|speak|talk|present|announce|sign|call|order|direct|lead|conduct|organize|host|convene|chair|launch|initiate|establish|create|build|develop|achieve|accomplish|complete|finish|deliver|send|submit|issue)\\b",
        "\\bi call (on|upon)\\b",
        "\\bi (salute|welcome|greet|congratulate|commend|thank|acknowledge)\\b",
        "\\blet me (salute|welcome|greet|congratulate|commend|thank|acknowledge|address|explain|clarify|begin|start|say|tell|remind|emphasize|stress|point out|note|add|turn)\\b",
        "\\bi know (that|what|how|we|the)\\b",
        "\\bi see (this|that|it|an|a|the)\\b",
        "\\bi understand (that|the|this)\\b",
        "\\bi (commit|pledge|promise|vow|dedicate)\\b",
        "\\bi can (assure|tell|say)\\b",
        "\\bi ask (you|that|congress|the)\\b",
        "\\bi look forward\\b",
        "\\bi (come|stand|speak) (here|before|to)\\b",
        "\\bi join\\b",
        "\\bi share\\b",
        "\\btoday i\\b",
        "\\bi (am|'m) (here|pleased|delighted|happy) to\\b"
      )

      # Condition B: Self as authority figure
      b_patterns <- c(
        "\\bif it were up to me\\b",
        "\\bif i had my way\\b",
        "\\bif it was up to me\\b",
        "\\blet me\\b",
        "\\ballow me to\\b",
        "\\bpermit me to\\b",
        "\\bi (must|have to|need to|should) (explain|clarify|point out|emphasize|stress|note|add|say)\\b",
        "\\bmy (position|decision|opinion|view|judgment|assessment|conclusion|recommendation) (was|is|has been) (accepted|adopted|approved|endorsed|followed|implemented|recognized)\\b",
        "\\bi (have|hold|exercise|maintain|possess) (authority|power|control|responsibility|oversight)\\b",
        "\\bi (am|was) (in charge|in control|responsible|accountable|the one)\\b",
        "\\bunder my (command|control|authority|supervision|watch)\\b",
        "\\bi (direct|order|command|instruct|authorize|approve|decide|determine)\\b",
        "\\bi (made|make) the (decision|call|choice|determination)\\b",
        "\\bthe (decision|choice|call) (is|was|will be) mine\\b",
        "\\bi (take|accept|bear|assume) (responsibility|credit|blame)\\b",
        "\\bas (president|leader|commander|head|chairman|director|secretary)\\s+i\\b",
        "\\bi\\s+as\\s+(president|leader|commander|head|chairman|director|secretary)\\b"
      )

      # Condition C: Self as recipient of positive reward/recognition
      c_patterns <- c(
        "\\bi (am|'m|was|feel|felt) (honored|honoured|privileged|proud|grateful|thankful|blessed|humbled|flattered|pleased|delighted|thrilled)\\b",
        "\\bhonor(ed|s)? me\\b",
        "\\bthank(s|ful|ed|ing)? (to )?(me|myself)\\b",
        "\\byou (flatter|honor|compliment|praise) me\\b",
        "\\bpraise(d|s)? (for )?me\\b",
        "\\bcompliment(s|ed)? (me|my)\\b",
        "\\baward(ed)? (to )?(me|myself)\\b",
        "\\bprivileg(e|ed) (of|to)\\b",
        "\\bi (was|am|have been|'ve been) (chosen|selected|elected|appointed|named|designated|honored|recognized|rewarded|praised|commended|acknowledged|thanked)\\b",
        "\\bchose me\\b",
        "\\bselect(ed)? me\\b",
        "\\brecogniz(e|ed|es|ing) (me|my)\\b",
        "\\bcommend(s|ed|ing)? (me|my)\\b",
        "\\backnowledg(e|ed|es|ing) (me|my)\\b",
        "\\bit (is|was) (a|an|my) (honor|privilege|pleasure)\\b",
        "\\bi (am|'m) (deeply|truly|very|most) (honored|grateful|thankful|privileged)\\b"
      )

      # Check all patterns
      a_match <- any(sapply(a_patterns, function(p) stringr::str_detect(s, p)))
      b_match <- any(sapply(b_patterns, function(p) stringr::str_detect(s, p)))
      c_match <- any(sapply(c_patterns, function(p) stringr::str_detect(s, p)))

      if (a_match || b_match || c_match) "SC" else "OSC"
    }

    # Classify each pronoun by sentence context
    pronoun_classified <- pronoun_df |>
      dplyr::rowwise() |>
      dplyr::mutate(conf = classify_conf(sentence)) |>
      dplyr::ungroup()

    # Count SC vs OSC labels
    output <- pronoun_classified |>
      dplyr::count(conf) |>
      tidyr::complete(conf = c("SC", "OSC"), fill = list(n = 0)) |>
      tidyr::pivot_wider(names_from = conf, values_from = n) |>
      dplyr::select(SC, OSC)

    if (!bootstrap) {
      return(output)
    } else {
      count_by_sentence <- pronoun_classified |>
        dplyr::group_by(sentence_id) |>
        dplyr::count(conf) |>
        tidyr::complete(conf = c("SC", "OSC"), fill = list(n = 0)) |>
        tidyr::pivot_wider(names_from = conf, values_from = n) |>
        dplyr::select(sentence_id, SC, OSC)

      boot_results <- purrr::map_dfr(1:B, ~{
        sampled_id <- sentences$sentence_id[sample(nrow(sentences), replace = TRUE)]
        sample_df <- tibble::tibble(sentence_id = sampled_id)

        own_sentences_sampled <- sample_df |>
          dplyr::inner_join(count_by_sentence, by = "sentence_id", relationship = "many-to-many")

        if (nrow(own_sentences_sampled) == 0) {
          tibble::tibble(SC = 0, OSC = 0)
        } else {
          own_sentences_sampled |>
            dplyr::summarise(SC = sum(SC), OSC = sum(OSC))
        }
      })

      boot_results |>
        dplyr::summarise(meanSC = mean(SC), meanOSC = mean(OSC),
                        varSC = stats::var(SC), varOSC = stats::var(OSC))
    }
  }
}
