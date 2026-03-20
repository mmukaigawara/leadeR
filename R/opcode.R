# Operational code

library(tidyverse)
library(spacyr)
library(countries)

source("code/det_lta/expand_aliases.R")

# All entities
country_list <- c(countries::list_countries(nomenclature = "name_en"))
country_list <- unique(tolower(country_list))
country_list <- c(country_list, "ussr", "east germany", 
                  "west germany", "soviet union", "the soviet union") # Soviet abbreviation
country_list <- unlist(lapply(country_list, function(x) expand_aliases_country(x)))
all_entities_corpus <- c(tolower(io), country_list)

# Internal function

get_scores <- function(dat) {

  iqv <- function(x) {
    if (length(x) < 2) return(0)
    p <- prop.table(table(x))
    if (length(p) < 2) return(0)
    (6 / (6 - 1)) * (1 - sum(p^2))
  }

  n_other <- sum(dat$entity_type == "Other")
  n_self <- sum(dat$entity_type == "Self")

  # P metrics (require Other > 0)
  if (n_other > 0) {
    P1 = (sum(dat$opc_score > 0 & dat$entity_type == "Other") -
            sum(dat$opc_score < 0 & dat$entity_type == "Other")) / n_other
    P2 = sum(dat$opc_score[which(dat$entity_type == "Other")]) / (n_other * 3)
    P3 = 1 - iqv(dat$opc_score[which(dat$entity_type == "Other")])

    other_reward = sum(dat$opc_score == 3 & dat$entity_type == "Other")
    other_promise = sum(dat$opc_score == 2 & dat$entity_type == "Other")
    other_support = sum(dat$opc_score == 1 & dat$entity_type == "Other")
    other_oppose = sum(dat$opc_score == -1 & dat$entity_type == "Other")
    other_threat = sum(dat$opc_score == -2 & dat$entity_type == "Other")
    other_punish = sum(dat$opc_score == -3 & dat$entity_type == "Other")
  } else {
    P1 = NA_real_
    P2 = NA_real_
    P3 = NA_real_
    other_reward = 0
    other_promise = 0
    other_support = 0
    other_oppose = 0
    other_threat = 0
    other_punish = 0
  }

  # I metrics (require Self > 0)
  if (n_self > 0) {
    I1 = (sum(dat$opc_score > 0 & dat$entity_type == "Self") -
            sum(dat$opc_score < 0 & dat$entity_type == "Self")) / n_self
    I2 = sum(dat$opc_score[which(dat$entity_type == "Self")]) / (n_self * 3)
    I3 = 1 - iqv(dat$opc_score[which(dat$entity_type == "Self")])
    I4a = 1 - abs(I1)
    I4b = 1 - abs(
      (sum(dat$opc_score %in% c(-2, -1, 1, 2) & dat$entity_type == "Self") -
         sum(dat$opc_score %in% c(-3, 3) & dat$entity_type == "Self")) / n_self
    )
    I5_punish = sum(dat$opc_score == -3 & dat$entity_type == "Self") / n_self
    I5_threat = sum(dat$opc_score == -2 & dat$entity_type == "Self") / n_self
    I5_oppose = sum(dat$opc_score == -1 & dat$entity_type == "Self") / n_self
    I5_support = sum(dat$opc_score == 1 & dat$entity_type == "Self") / n_self
    I5_promise = sum(dat$opc_score == 2 & dat$entity_type == "Self") / n_self
    I5_reward = sum(dat$opc_score == 3 & dat$entity_type == "Self") / n_self

    self_reward = sum(dat$opc_score == 3 & dat$entity_type == "Self")
    self_promise = sum(dat$opc_score == 2 & dat$entity_type == "Self")
    self_support = sum(dat$opc_score == 1 & dat$entity_type == "Self")
    self_oppose = sum(dat$opc_score == -1 & dat$entity_type == "Self")
    self_threat = sum(dat$opc_score == -2 & dat$entity_type == "Self")
    self_punish = sum(dat$opc_score == -3 & dat$entity_type == "Self")
  } else {
    I1 = NA_real_
    I2 = NA_real_
    I3 = NA_real_
    I4a = NA_real_
    I4b = NA_real_
    I5_punish = NA_real_
    I5_threat = NA_real_
    I5_oppose = NA_real_
    I5_support = NA_real_
    I5_promise = NA_real_
    I5_reward = NA_real_
    self_reward = 0
    self_promise = 0
    self_support = 0
    self_oppose = 0
    self_threat = 0
    self_punish = 0
  }

  # P4 and P5 require both
  P4 = if (nrow(dat) > 0) n_self / nrow(dat) else NA_real_
  P5 = if (!is.na(P3) && !is.na(P4)) 1 - P3 * P4 else NA_real_

  return(tibble(P1 = P1, P2 = P2, P3 = P3, P4 = P4, P5 = P5,
                I1 = I1, I2 = I2, I3 = I3, I4a = I4a, I4b = I4b,
                I5_punish = I5_punish, I5_threat = I5_threat, I5_oppose = I5_oppose,
                I5_support = I5_support, I5_promise = I5_promise, I5_reward = I5_reward,
                self_reward = self_reward, self_promise = self_promise, self_support = self_support,
                self_oppose = self_oppose, self_threat = self_threat, self_punish = self_punish,
                other_reward = other_reward, other_promise = other_promise, other_support = other_support,
                other_oppose = other_oppose, other_threat = other_threat, other_punish = other_punish
                ))

}

# --- MASSIVELY EXTENDED VICS VERB DICTIONARIES ---

# --- 1. PUNISH (-3) ---
verbs_punish <- c(
  "attack", "bomb", "destroy", "invade", "punish", "expel", "assault", "strike", 
  "bombard", "shell", "raid", "ambush", "fire", "shoot", "massacre", "slaughter", 
  "decimate", "eradicate", "annihilate", "obliterate", "devastate", "ravage", 
  "pulverize", "flatten", "crush", "smash", "wreck", "liquidate", "neutralize", 
  "conquer", "subjugate", "vanquish", "overrun", "besiege", "encircle", 
  "mobilize", "deploy", "disarm", "demilitarize", "occupy", "annex", "seize", 
  "capture", "arrest", "detain", "jail", "imprison", "incarcerate", "abduct", 
  "kidnap", "torture", "execute", "assassinate", "murder", "kill", "purge", 
  "eliminate", "exterminate", "genocide", "sanction", "embargo", "blockade", 
  "boycott", "ban", "bar", "blacklist", "censor", "suppress", "repress", "quell", 
  "silence", "gag", "confiscate", "expropriate", "sequestrate", "freeze", "appropriate", 
  "nationalize", "revoke", "rescind", "cancel", "terminate", "suspend", "sever", 
  "cut", "break", "rupture", "halt", "stop", "impede", "obstruct", "disrupt", 
  "sabotage", "subvert", "undermine", "destabilize", "topple", "overthrow", 
  "oust", "depose", "remove", "dismiss", "sack", "deport", "extradite", "exile", 
  "banish", "marginalize", "isolate", "alienate", "coerce", "force", "compel", 
  "constrain", "impose", "dictate", "mandate", "enforce", "prosecute", "indict", 
  "convict", "sentence", "penalize", "fine", "levy", "tax", "burden", "encumber", 
  "bankrupt", "impoverish", "starve", "strangle", "choke", "suffocate",
  
  "blow up", "wipe out", "take over", "crack down", "lock up", "shut down", 
  "cut off", "kick out", "push out", "drive out", "beat back", "hunt down", 
  "round up", "shoot down", "take out", "clamp down", "close down",
  
  "hack", "infiltrate", "breach", "compromise", "surveil", "spy", "wiretap",
  "dox", "jam", "scramble", "intercept", "nuke", "gas", "drone",

  # NEW: Political/legislative obstruction verbs (human coders count these as punish)
  # "voted against X", "defeated the bill", "killed the legislation"
  "defeat", "kill", "gut", "slash", "strip", "deny", "deprive",
  "defund", "decimate", "dismantle", "abolish", "eliminate",
  "discriminate", "persecute", "oppress", "exploit",
  "violate", "breach", "infringe", "transgress",
  "betray", "abandon", "forsake", "desert"
)

# --- 2. THREATEN (-2) ---
# REFINED: Only warnings/threats of future negative action, not demands/pressure
verbs_threaten <- c(
  # Core threat/warning verbs
  "threaten", "warn", "alert", "intimidate", "menace", "blackmail",
  "bully", "cow", "browbeat",

  # Fear-inducing verbs
  "scare", "frighten", "terrify", "alarm", "horrify", "appall",
  "daunt", "unnerve",

  # Escalation verbs
  "provoke", "incite", "instigate", "foment", "inflame", "escalate",

  # Danger/risk verbs
  "imperil", "jeopardize", "endanger",

  # Warning/omen verbs
  "forewarn", "admonish", "caution",

  # Display of force
  "posture", "bluster", "brandish", "flaunt", "flex", "bluff",

  # Ultimatum (true threat)
  "ultimatum"
)

# --- 3. OPPOSE (-1) ---
verbs_oppose <- c(
  "oppose", "condemn", "reject", "refuse", "criticize", "deny", "object", 
  "protest", "blame", "accuse", "charge", "allege", "impute", "attribute", 
  "ascribe", "indict", "impeach", "arraign", "denounce", "decry", "deplore", 
  "lament", "mourn", "grieve", "regret", "rue", "disapprove", "disagree", 
  "dissent", "differ", "clash", "conflict", "collide", "contest", "dispute", 
  "question", "query", "doubt", "suspect", "distrust", "mistrust", "skeptical", 
  "cynical", "dismiss", "discard", "ignore", "disregard", "neglect", "overlook", 
  "shun", "snub", "spurn", "rebuff", "repulse", "alienate", "estrange", 
  "divorce", "separate", "split", "divide", "fragment", "splinter", "polarize", 
  "radicalize", "marginalize", "exclude", "omit", "bar", "block", "hinder", 
  "hamper", "impede", "thwart", "stymie", "foil", "frustrate", "check", "curb", 
  "restrain", "restrict", "limit", "confine", "constrain", "contain", 
  "encircle", "surround", "envelop", "besieged", "withhold", "retain", "keep", 
  "reserve", "preserve", "maintain", "uphold", "defend", "protect", "guard", 
  "shield", "screen", "cover", "hide", "conceal", "mask", "veil", "cloak", 
  "disguise", "camouflage", "obscure", "cloud", "blur", "confuse", "muddle", 
  "complicate", "obfuscate", "distort", "twist", "warp", "pervert", "manipulate", 
  "exploit", "abuse", "misuse", "mistreat", "maltreat", "neglect",
  
  "turn down", "vote down", "rule out", "push back", "break off", "call off",
  "walk out", "opt out", "back out", "stand against", "shut out", "fend off",

  "veto", "filibuster", "abstain", "defect", "stonewall", "stall", "table",

  # Assertion/criticism verbs (added to improve Other detection)
  "assert", "declare", "proclaim", "claim", "contend", "maintain",
  "point out", "highlight", "emphasize", "stress", "underscore",
  "argue", "debate", "dispute", "counter", "rebut", "refute",
  "call out", "take issue", "take exception",

  # Demand/pressure verbs (moved from Threaten - these are opposition, not threats)
  "demand", "insist", "require", "pressure", "push",
  "order", "command", "direct", "instruct", "leverage"
)

# --- 4. SUPPORT (+1) ---
verbs_support <- c(
  "support", "endorse", "approve", "agree", "praise", "accept", "welcome", 
  "aid", "help", "assist", "benefit", "favor", "prefer", "choose", "select", 
  "elect", "vote", "nominate", "appoint", "designate", "name", "authorize", 
  "empower", "entitle", "enable", "facilitate", "promote", "advance", 
  "further", "foster", "cultivate", "nurture", "nurse", "cherish", "treasure", 
  "value", "prize", "esteem", "respect", "admire", "honor", "revere", 
  "venerate", "worship", "idolize", "adore", "love", "like", "enjoy", 
  "appreciate", "grateful", "thank", "acknowledge", "recognize", "admit", 
  "concede", "confess", "grant", "allow", "permit", "let", "suffer", 
  "tolerate", "bear", "endure", "stand", "abide", "condone", "excuse", 
  "pardon", "forgive", "absolve", "acquit", "exonerate", "vindicate", "justify", 
  "warrant", "defend", "uphold", "sustain", "maintain", "preserve", "keep", 
  "save", "rescue", "liberate", "free", "emancipate", "release", "discharge", 
  "dismiss", "exempt", "spare", "relieve", "ease", "alleviate", "mitigate", 
  "ameliorate", "improve", "better", "enhance", "enrich", "upgrade", "update", 
  "modernize", "reform", "renovate", "restore", "revive", "resurrect", 
  "renew", "regenerate", "rejuvenate", "invigorate", "refresh", "revitalize", 
  "stimulate", "encourage", "inspire", "motivate", "embolden", "hearten", 
  "comfort", "console", "soothe", "calm", "quiet", "pacify", "placate", 
  "appease", "mollify", "conciliate", "reconcile", "mediate", "arbitrate", 
  "negotiate", "bargain", "haggle", "deal", "trade", "exchange", "swap", 
  "switch", "substitute", "replace", "alternate", "rotate", "cycle", 
  "circulate", "distribute", "dispense", "allocate", "allot", "assign", 
  "apportion", "share", "partake", "participate", "cooperate", "collaborate", 
  "coordinate", "associate", "ally", "unite", "join", "combine", "merge", 
  "fuse", "blend", "mix", "mingle", "integrate", "incorporate", "consolidate", 
  "unify", "solidify", "strengthen", "fortify", "reinforce", "buttress", 
  "bolster", "prop", "shore", "brace",
  
  "back up", "stand by", "stick by", "root for", "cheer on", "vote for",
  "side with", "team up", "band together", "hear out",

  "partner", "align", "caucus",

  # Appeal/request verbs (added to improve Other detection)
  "call", "call for", "call upon", "call on",
  "appeal", "appeal to", "appeal for",
  "urge", "exhort", "implore", "beseech", "entreat",
  "request", "ask", "seek", "petition", "solicit",
  "invite", "summon", "beckon", "convene",
  "plead", "beg", "pray",

  # Additional appeal/exhortation verbs (critical for political speech)
  "challenge", "congratulate", "salute", "honor", "celebrate",
  "recommend", "suggest", "propose", "advise",
  "let", "allow",  # for "let us" / "let's" constructions
  "need",  # prescriptive "we need to..."
  "wish", "hope"
)

# --- 5. PROMISE (+2) ---
# Cooperative Words: Future commitments, treaty signing, and assurances.
# REFINED: Removed generic intent/desire verbs that caused +4.94 overcounting
# Focus on ACTUAL COMMITMENTS, not just expressions of intent or desire
verbs_promise <- c(
  # Core commitment verbs (VICS Promise = binding commitments)
  "promise", "pledge", "assure", "guarantee", "warrant", "certify", "attest",
  "swear", "vow", "commit", "bind", "obligate", "undertake", "covenant",

  # Agreement/consent verbs (formal acceptance)
  "agree", "consent", "assent", "accede", "concur", "comply",

  # Formal offers and proposals (binding)
  "propose", "offer", "tender", "bid",

  # Financial commitments
  "finance", "fund", "subsidize", "underwrite",

  # Advocacy verbs (committed support)
  "advocate", "champion", "sponsor",

  # Phrasal commitment verbs
  "enter into", "sign on", "swear in",

  # Treaty/formal agreement verbs
  "ratify", "sign", "ink", "finalise", "formalize",

  # RESTORED: Intent verbs that indicate commitment (human coders count these)
  # Context: When spoken by political leader, these ARE commitments
  "intend", "plan", "aim",
  "expect", "anticipate",
  "declare", "announce", "proclaim",
  "affirm", "reaffirm", "confirm"

  # REMOVED to fix overcounting:
  # - suggest, recommend, advise, urge, counsel (generic discussion, not commitment)
  # - hope, wish, desire, want, need, expect (desire, not commitment)
  # - plan, intend, aim (intent but not binding commitment)
  # - ask, request, demand, beg, plead (requests, not promises)
  # - call, invite, summon (convening, not promising)
  # - anticipate, predict, forecast (prediction, not commitment)
)

# --- 6. REWARD (+3) ---
# Cooperative Deeds: Material aid, de-escalation actions, and granting rights.
# BALANCED: Restored high-frequency material action verbs to fix Other Reward bias (0.39→target 0.7)
verbs_reward <- c(
  # Core reward verbs (actual positive deeds)
  "reward", "award", "grant", "give", "donate", "contribute",
  "provide", "supply", "furnish", "deliver",

  # Material aid verbs (completed actions)
  "aid", "assist", "rescue", "liberate", "free", "save",

  # Concession/de-escalation verbs
  "release", "withdraw", "relinquish", "cede", "surrender",
  "lift", "waive", "ease", "relieve",

  # Financial/material transfer verbs
  "fund", "finance", "invest", "loan", "lend",
  "compensate", "reimburse", "repay", "refund",

  # RESTORED: High-frequency material action verbs (to fix Other Reward undercounting)
  "build", "construct", "create", "establish", "found",
  "open", "launch", "initiate", "start",
  "restore", "rebuild", "repair", "fix",
  "send", "ship", "transport", "dispatch",
  "allocate", "distribute", "share", "transfer",

  # Treaty/agreement completion verbs
  "sign", "ratify", "conclude", "finalize", "seal",

  # Phrasal reward verbs
  "give up", "hand over", "turn over", "pull out", "stand down",
  "pay back", "give back", "bail out",

  # De-escalation
  "repatriate", "pardon", "amnesty",

  # NEW: Achievement/accomplishment verbs (human coders count past accomplishments as reward)
  # "passed the legislation", "achieved peace", "won the war"
  "pass", "enact", "adopt", "implement", "execute",
  "achieve", "accomplish", "attain", "realize", "fulfill",
  "secure", "win", "gain", "obtain", "acquire",
  "complete", "finish", "succeed", "triumph", "prevail",
  "improve", "strengthen", "enhance", "boost", "increase",
  "expand", "extend", "broaden", "widen", "deepen",
  "develop", "advance", "progress", "promote", "elevate",
  "protect", "defend", "safeguard", "preserve", "maintain",
  "uphold", "sustain", "ensure", "guarantee"
)

# --- 7. VERBS THAT BECOME NEGATIVE WITH "AGAINST" (NEW) ---
# These neutral/positive verbs become Oppose (-1) or Punish (-3) when + "against"
# "voted against X" = Oppose, "fought against X" = Punish
verbs_flip_with_against <- c(
  # Voting/political action verbs -> Oppose (-1)
  "vote", "speak", "argue", "testify", "campaign", "rally",
  "lobby", "advocate", "push", "work", "act", "move",
  "decide", "rule", "stand", "come out", "go",
  # Combat/struggle verbs -> Punish (-3)
  "fight", "battle", "struggle", "war", "rebel", "revolt",
  "strike", "march", "protest", "demonstrate", "riot"
)

# Subset that becomes Punish (-3) when + against (vs Oppose -1)
verbs_punish_with_against <- c(
  "fight", "battle", "struggle", "war", "rebel", "revolt",
  "strike", "march", "riot"
)

# --- NOMINALIZATION DICTIONARIES ---
# Nouns that represent actions (counted by human coders but invisible to verb-only parsing)

nouns_reward <- c(
  "aid", "assistance", "support", "help", "relief", "provision", "provisions",
  "liberation", "freedom", "grant", "grants", "gift", "gifts", "donation", "donations",
  "contribution", "contributions", "cooperation", "collaboration", "partnership",
  "alliance", "alliances", "treaty", "treaties", "agreement", "agreements",
  "rescue", "deployment", "delivery", "investment", "investments",

  # NEW: Policy/legislative achievement nouns
  "achievement", "achievements", "accomplishment", "accomplishments",
  "success", "successes", "victory", "victories", "triumph", "triumphs",
  "progress", "improvement", "improvements", "advancement", "advancements",
  "reform", "reforms", "legislation", "law", "laws", "act", "acts",
  "program", "programs", "initiative", "initiatives", "policy", "policies",
  "benefit", "benefits", "protection", "protections", "security",
  "development", "developments", "growth", "expansion", "increase"
)

nouns_punish <- c(
  "attack", "attacks", "assault", "assaults", "invasion", "invasions",
  "destruction", "devastation", "sanction", "sanctions", "embargo", "embargoes",
  "blockade", "blockades", "punishment", "punishments", "retaliation",
  "strike", "strikes", "bombing", "bombings", "occupation", "seizure",
  "expulsion", "execution", "executions", "massacre", "massacres",

  # Punitive administrative actions (human coders count these)
  "withdrawal", "withdrawals",
  "suspension", "suspensions",
  "termination", "terminations",
  "revocation", "revocations",
  "cutoff", "cutoffs",
  "cancellation", "cancellations",
  "abrogation", "abrogations",
  "severance",
  "crackdown", "crackdowns",
  "clampdown", "clampdowns",

  # NEW: Political/policy harm nouns
  "discrimination", "persecution", "oppression", "exploitation",
  "violation", "violations", "breach", "breaches", "infringement",
  "defeat", "defeats", "rejection", "rejections", "denial", "denials",
  "obstruction", "obstructions", "veto", "vetoes",
  "cut", "cuts", "slash", "slashes", "reduction", "reductions",
  "prosecution", "prosecutions", "indictment", "indictments",
  "conviction", "convictions", "sentence", "sentences"
)

nouns_support <- c(
  "endorsement", "endorsements", "approval", "recognition", "acknowledgment",
  "praise", "commendation", "appreciation", "gratitude", "thanks",
  "appeal", "appeals", "request", "requests", "call", "calls",
  "petition", "petitions", "recommendation", "recommendations",
  "tribute", "tributes", "honor", "honors", "salute", "salutes"
)

nouns_threaten <- c(
  "threat", "threats", "warning", "warnings", "ultimatum", "ultimatums",
  "intimidation", "coercion", "blackmail", "pressure", "pressures",

  # Military/conflict threat nouns
  "aggression", "aggressions",
  "escalation", "escalations",
  "hostility", "hostilities",
  "confrontation", "confrontations",
  "provocation", "provocations",
  "menace", "menaces",

  # Implicit threat nouns
  "buildup", "buildups",
  "mobilization", "mobilizations",
  "brinkmanship",

  # Diplomatic threat nouns
  "deadline", "deadlines",
  "consequence", "consequences"
)

nouns_oppose <- c(
  "opposition", "objection", "objections", "protest", "protests",
  "criticism", "criticisms", "condemnation", "rejection", "denial",
  "veto", "vetoes", "refusal", "dispute", "disputes"
)

# NEW: Promise nominalizations (previously missing)
nouns_promise <- c(
  "promise", "promises", "pledge", "pledges",
  "commitment", "commitments", "assurance", "assurances",
  "guarantee", "guarantees", "vow", "vows",
  "oath", "oaths", "covenant", "covenants",
  "undertaking", "undertakings", "obligation", "obligations",
  "agreement", "agreements", "treaty", "treaties",
  "pact", "pacts", "accord", "accords",
  "declaration", "declarations",
  "intention", "intentions",
  "plan", "plans", "proposal", "proposals"
)

get_opc <- function(own_entity, text, bootstrap = FALSE, B = 1000) {
  
  # --- A. Setup Entity Lists ---
  own_entity_clean <- unique(c(tolower(own_entity), expand_aliases_country(own_entity)))
  
  own_terms <- c(own_entity_clean, 
                 "we", "our nation", "our country",
                 "our state", "our government", "our institution", 
                 "our group", "our party", "our people",
                 "i", "my nation", "my country",
                 "my state", "my government", "my institution",
                 "my group", "my party", "my people")
  
  other_terms <- c(setdiff(all_entities_corpus, tolower(own_terms)),
                   "they", "their nation", "their country", 
                   "their state", "their government", "their institution")
  
  # --- B. Parse Text ---
  # Parse text with dependency parsing enabled
  parsed <- spacy_parse(text, dependency = TRUE, entity = TRUE, tag = TRUE)
  
  # --- C. Identify Multi-word Entities ---
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
  
  # --- D. Link Subjects to Verbs ---

  # 1. Isolate Subjects (with NER entity type info)
  subjects <- parsed_entity_grouped |>
    filter(dep_rel %in% c("poss", "nsubj", "nsubjpass")) |>
    mutate(
      subject_ner_type = entity  # Keep the NER entity type
    ) |>
    select(-entity) |>
    left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
    mutate(
      poss_pronoun = lag(lemma),
      poss_dep = lag(dep_rel),
      has_possessive = poss_pronoun %in% c("my", "our", "their") & poss_dep == "poss",
      subject_phrase = case_when(
        has_possessive ~ paste(poss_pronoun, lemma),
        !is.na(entity) ~ entity,
        TRUE ~ token
      )
    ) |>
    filter(dep_rel %in% c("nsubj", "nsubjpass")) |>
    select(doc_id, sentence_id, head_token_id, subject_phrase, subject_ner_type) |>
    distinct()

  # 1b. ENHANCED passive voice agent detection
  # Improved: Uses dependency tree structure instead of fragile lag() check
  # Captures: "invaded by Soviet forces", "attacked by the enemy", etc.

  # Step 1: Find all "by" prepositions and trace to their verb heads
  by_prep_tokens <- parsed |>
    filter(dep_rel == "prep" & tolower(lemma) == "by") |>
    select(doc_id, sentence_id, by_token_id = token_id, verb_head = head_token_id)

  # Step 2: Find pobj children of "by" prepositions (the actual agents)
  by_prep_objects <- parsed |>
    filter(dep_rel == "pobj") |>
    inner_join(by_prep_tokens,
               by = c("doc_id", "sentence_id", "head_token_id" = "by_token_id")) |>
    select(doc_id, sentence_id, head_token_id = verb_head, agent_token_id = token_id, agent_token = token)

  # Step 3: Get entity info for by-phrase agents
  by_agents_with_entities <- by_prep_objects |>
    left_join(
      parsed_entity_grouped |> select(doc_id, sentence_id, token_id, entity_group_id, entity),
      by = c("doc_id", "sentence_id", "agent_token_id" = "token_id")
    ) |>
    left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
    mutate(
      subject_phrase = coalesce(entity.y, agent_token),
      subject_ner_type = coalesce(entity.x, "")
    ) |>
    select(doc_id, sentence_id, head_token_id, subject_phrase, subject_ner_type) |>
    distinct()

  # Step 4: Also capture direct agent relations and obl relations (modern spaCy)
  direct_agents <- parsed_entity_grouped |>
    filter(dep_rel %in% c("agent", "obl")) |>
    mutate(subject_ner_type = entity) |>
    select(-entity) |>
    left_join(multiword_entities, by = c("doc_id", "sentence_id", "entity_group_id")) |>
    mutate(
      subject_phrase = case_when(
        !is.na(entity) ~ entity,
        TRUE ~ token
      )
    ) |>
    select(doc_id, sentence_id, head_token_id, subject_phrase, subject_ner_type) |>
    distinct()

  # Step 5: Combine all agent sources
  agents <- bind_rows(direct_agents, by_agents_with_entities) |> distinct()

  # Combine standard subjects with agent phrases
  subjects <- bind_rows(subjects, agents) |> distinct()

  # 1c. RELATIVE CLAUSE HANDLING (NEW)
  # In "forces that attacked", "forces" is the logical subject of "attacked"
  # relcl = relative clause modifier
  relcl_verbs <- parsed |>
    filter(dep_rel == "relcl" & pos == "VERB") |>
    select(doc_id, sentence_id, token_id, relcl_head = head_token_id,
           verb_lemma = lemma, verb_tag = tag)

  if (nrow(relcl_verbs) > 0) {
    # Get the head nouns of relative clauses
    relcl_head_nouns <- parsed |>
      filter(pos %in% c("NOUN", "PROPN", "PRON")) |>
      select(doc_id, sentence_id, token_id, head_noun = token, head_ner = entity)

    # Link head nouns to relative clause verbs
    relcl_subjects <- relcl_verbs |>
      inner_join(relcl_head_nouns,
                 by = c("doc_id", "sentence_id", "relcl_head" = "token_id")) |>
      mutate(
        subject_phrase = head_noun,
        subject_ner_type = coalesce(head_ner, "")
      ) |>
      select(doc_id, sentence_id, head_token_id = token_id, subject_phrase, subject_ner_type) |>
      distinct()

    # Add relative clause subjects to subjects
    subjects <- bind_rows(subjects, relcl_subjects) |> distinct()
  }

  # 1d. ADJECTIVAL CLAUSE HANDLING (NEW)
  # In "forces attacking the city", "forces" is the subject of "attacking"
  # In "measures designed to prevent", "measures" is the subject of "designed"
  # acl = adjectival clause
  acl_verbs <- parsed |>
    filter(dep_rel == "acl" & pos == "VERB") |>
    select(doc_id, sentence_id, token_id, acl_head = head_token_id,
           verb_lemma = lemma, verb_tag = tag)

  if (nrow(acl_verbs) > 0) {
    # Get head nouns of adjectival clauses
    acl_head_nouns <- parsed |>
      filter(pos %in% c("NOUN", "PROPN", "PRON")) |>
      select(doc_id, sentence_id, token_id, head_noun = token, head_ner = entity)

    # Link head nouns to acl verbs
    acl_subjects <- acl_verbs |>
      inner_join(acl_head_nouns,
                 by = c("doc_id", "sentence_id", "acl_head" = "token_id")) |>
      mutate(
        subject_phrase = head_noun,
        subject_ner_type = coalesce(head_ner, "")
      ) |>
      select(doc_id, sentence_id, head_token_id = token_id, subject_phrase, subject_ner_type) |>
      distinct()

    # Add adjectival clause subjects
    subjects <- bind_rows(subjects, acl_subjects) |> distinct()
  }

  # 2. Identify Particles
  # Find tokens that are particles (e.g., "up", "down", "out") and link them to their head verb
  particles <- parsed |>
    filter(dep_rel == "prt") |>
    select(doc_id, sentence_id, head_token_id, particle = lemma)
  
  future_markers <- parsed |>
    filter(dep_rel == "aux", lemma %in% c("will", "shall", "wo", "'ll", "gon")) |>
    select(doc_id, sentence_id, head_token_id, future_aux = lemma)

  # 2b. Detect modal verbs as implicit appeals/commitments
  # "should", "must", "need to" = calls to action, coded as Support (+1) by human coders
  modal_markers <- parsed |>
    filter(dep_rel == "aux" & lemma %in% c("should", "must", "ought", "need")) |>
    select(doc_id, sentence_id, head_token_id, modal_aux = lemma)

  # 2c. CONDITIONAL THREAT DETECTION (NEW)
  # Detect "if X then we will Y" patterns where Y is punish action -> codes as Threat
  # spaCy dependency: advcl (adverbial clause modifier) with "mark" child containing if/unless/should
  conditional_markers <- parsed |>
    filter(dep_rel == "mark" & tolower(lemma) %in% c("if", "unless", "should", "whether")) |>
    select(doc_id, sentence_id, mark_head = head_token_id, condition_type = lemma)

  # Find advcl verbs (the condition clause)
  advcl_verbs <- parsed |>
    filter(dep_rel == "advcl") |>
    select(doc_id, sentence_id, advcl_token = token_id, main_verb_id = head_token_id)

  # Link conditional markers to their main clause verbs via the advcl
  conditional_consequents <- advcl_verbs |>
    inner_join(conditional_markers,
               by = c("doc_id", "sentence_id", "advcl_token" = "mark_head")) |>
    select(doc_id, sentence_id, head_token_id = main_verb_id) |>
    mutate(has_conditional = TRUE) |>
    distinct()

  # 2d. "AGAINST" MODIFIER DETECTION (NEW)
  # Detect "voted against", "fought against", "worked against" patterns
  # These neutral/positive verbs become negative (Oppose/Punish) when + "against"
  against_prep <- parsed |>
    filter(dep_rel == "prep" & tolower(lemma) == "against") |>
    select(doc_id, sentence_id, against_head = head_token_id) |>
    mutate(has_against = TRUE) |>
    distinct()

  # Also detect "against" as agent (some parses)
  against_agent <- parsed |>
    filter(dep_rel == "agent" & tolower(token) == "against") |>
    select(doc_id, sentence_id, against_head = head_token_id) |>
    mutate(has_against = TRUE) |>
    distinct()

  # Combine both patterns
  against_markers <- bind_rows(against_prep, against_agent) |> distinct()

  # 3. Isolate Verbs and Merge Particles
  verbs <- parsed |>
    filter(pos == "VERB") |>
    left_join(particles, by = c("doc_id", "sentence_id", "token_id" = "head_token_id")) |>
    left_join(future_markers, by = c("doc_id", "sentence_id", "token_id" = "head_token_id")) |>
    left_join(modal_markers, by = c("doc_id", "sentence_id", "token_id" = "head_token_id")) |>
    left_join(conditional_consequents, by = c("doc_id", "sentence_id", "token_id" = "head_token_id")) |>
    left_join(against_markers, by = c("doc_id", "sentence_id", "token_id" = "against_head")) |>
    mutate(
      lemma_full = ifelse(!is.na(particle), paste(lemma, particle), lemma),
      is_future = !is.na(future_aux), # Boolean flag for future tense
      has_modal = !is.na(modal_aux),  # Boolean flag for modal (should/must/ought/need)
      is_conditional = coalesce(has_conditional, FALSE),  # NEW: "if X then Y" consequent
      has_against = coalesce(has_against, FALSE)  # NEW: "voted against", "fought against"
    ) |>
    select(doc_id, sentence_id, token_id, lemma_full, is_future, has_modal, is_conditional, has_against, tag)

  # 4. Join Subjects to Verbs (including NER type)
  utterances <- verbs |>
    inner_join(subjects, by = c("doc_id", "sentence_id", "token_id" = "head_token_id")) |>
    mutate(subject_ner_type = coalesce(subject_ner_type, ""))

  # 4a. COORDINATION HANDLING
  # Handle "We X and Y" - conjunct verbs should inherit subject from head verb
  # This captures cases like "We held the line AND eliminated waste"
  conj_verbs <- parsed |>
    filter(dep_rel == "conj" & pos == "VERB") |>
    select(doc_id, sentence_id, token_id, conj_head = head_token_id)

  if (nrow(conj_verbs) > 0) {
    # Find subjects linked to the head verbs
    head_verb_subjects <- utterances |>
      select(doc_id, sentence_id, token_id, subject_phrase, subject_ner_type)

    # Link conjunct verbs to their head verb's subject
    utterances_from_conj <- verbs |>
      inner_join(conj_verbs, by = c("doc_id", "sentence_id", "token_id")) |>
      inner_join(head_verb_subjects,
                 by = c("doc_id", "sentence_id", "conj_head" = "token_id")) |>
      select(-conj_head)

    # Combine with existing utterances
    utterances <- bind_rows(utterances, utterances_from_conj) |> distinct()
  }

  # 4a2. IMPERATIVE DETECTION
  # Imperatives like "Vote!", "Support our troops!", "Send him back!" have no grammatical subject
  # but are appeals to the audience (Other). Detect verbs in base form (VB) that lack subjects.
  # EXCEPTION: "Let us X" / "Let's X" constructions are Self (inclusive we), not Other
  verbs_with_subjects <- utterances |>
    select(doc_id, sentence_id, token_id) |>
    distinct()

  # Detect "let us" / "let's" patterns - these are Self (inclusive we)
  let_us_verbs <- parsed |>
    filter(dep_rel == "ccomp" | dep_rel == "xcomp") |>
    left_join(
      parsed |>
        filter(tolower(lemma) == "let" & pos == "VERB") |>
        select(doc_id, sentence_id, let_token = token_id),
      by = c("doc_id", "sentence_id", "head_token_id" = "let_token")
    ) |>
    filter(!is.na(head_token_id)) |>
    # Check if "us" or "'s" is the object of "let"
    left_join(
      parsed |>
        filter(dep_rel %in% c("dobj", "nsubj") & tolower(lemma) %in% c("us", "'s", "we")) |>
        select(doc_id, sentence_id, obj_head = head_token_id) |>
        mutate(is_let_us = TRUE),
      by = c("doc_id", "sentence_id", "head_token_id" = "obj_head")
    ) |>
    filter(coalesce(is_let_us, FALSE)) |>
    select(doc_id, sentence_id, token_id) |>
    distinct()

  imperatives <- verbs |>
    filter(tag == "VB") |>  # Base form verb (imperative mood)
    anti_join(verbs_with_subjects, by = c("doc_id", "sentence_id", "token_id")) |>
    # Check if this verb is part of a "let us" construction
    left_join(let_us_verbs |> mutate(is_let_us = TRUE),
              by = c("doc_id", "sentence_id", "token_id")) |>
    mutate(
      # "Let us X" = Self (inclusive we), other imperatives = Other (audience)
      subject_phrase = ifelse(coalesce(is_let_us, FALSE), "we", "you"),
      subject_ner_type = ""
    ) |>
    select(-is_let_us)

  if (nrow(imperatives) > 0) {
    utterances <- bind_rows(utterances, imperatives) |> distinct()
  }

  # 4b. OBJECT PRONOUNS AS IMPLIED "OTHER" AGENTS
  # For verbs like "help them", "support them", "attack them" - the object pronoun indicates
  # an action directed at "Other". This captures many missing "Other" actions.
  # CRITICAL: Include "you/your" - "I urge you", "I challenge you" are extremely common appeals
  object_pronouns <- parsed |>
    filter(dep_rel %in% c("dobj", "iobj") &
           tolower(lemma) %in% c("they", "them", "he", "she", "it", "him", "her", "you", "your")) |>
    select(doc_id, sentence_id, head_token_id, object_phrase = token) |>
    mutate(subject_phrase = object_phrase, subject_ner_type = "") |>
    distinct()

  # Create utterances from object pronouns (these represent actions TOWARD Others)
  if (nrow(object_pronouns) > 0) {
    utterances_from_objects <- verbs |>
      inner_join(object_pronouns,
                 by = c("doc_id", "sentence_id", "token_id" = "head_token_id"))

    # Combine with regular utterances
    utterances <- bind_rows(utterances, utterances_from_objects) |> distinct()
  }

  # 4c. NOMINALIZATION DETECTION WITH PROPER AGENT EXTRACTION (IMPROVED)
  # Human coders count action nouns like "liberation", "sanctions", "aid"
  # CRITICAL: Must extract the actual AGENT of the nominalization, not just sentence subject
  # Priority: possessive ("their attack") > by-phrase ("attack by X") > amod ("Soviet invasion") > sentence subject
  all_action_nouns <- c(nouns_reward, nouns_punish, nouns_support, nouns_threaten, nouns_oppose, nouns_promise)

  nominalizations <- parsed |>
    filter(pos == "NOUN" & tolower(lemma) %in% all_action_nouns) |>
    select(doc_id, sentence_id, token_id, noun_lemma = lemma)

  if (nrow(nominalizations) > 0) {

    # Step 1: Find possessive modifiers of nominalizations ("their attack", "Soviet's invasion")
    nom_poss_agents <- parsed |>
      filter(dep_rel %in% c("poss", "nmod:poss")) |>
      select(doc_id, sentence_id, poss_head = head_token_id, poss_phrase = token, poss_ner = entity)

    # Step 2: Find adjectival/compound modifiers ("Soviet invasion", "American aid", "military strike")
    nom_amod_agents <- parsed |>
      filter(dep_rel %in% c("amod", "compound")) |>
      select(doc_id, sentence_id, amod_head = head_token_id, amod_phrase = token, amod_ner = entity)

    # Step 3: Find "by X" agents of nominalizations ("attack by Soviet forces")
    nom_prep <- parsed |>
      filter(dep_rel == "prep" & tolower(lemma) == "by") |>
      select(doc_id, sentence_id, by_token_id = token_id, nom_head = head_token_id)

    nom_by_objects <- parsed |>
      filter(dep_rel == "pobj") |>
      inner_join(nom_prep, by = c("doc_id", "sentence_id", "head_token_id" = "by_token_id")) |>
      select(doc_id, sentence_id, by_head = nom_head, by_phrase = token, by_ner = entity)

    # Step 4: Get sentence subjects as fallback
    sentence_subjects <- subjects |>
      group_by(doc_id, sentence_id) |>
      slice(1) |>
      ungroup() |>
      select(doc_id, sentence_id, sent_subject = subject_phrase, sent_ner = subject_ner_type)

    # Step 5: Link nominalizations to their agents (priority: poss > by-phrase > amod > sentence subject)
    utterances_from_nouns <- nominalizations |>
      # Try possessive first
      left_join(nom_poss_agents, by = c("doc_id", "sentence_id", "token_id" = "poss_head")) |>
      # Then try "by X" phrases
      left_join(nom_by_objects, by = c("doc_id", "sentence_id", "token_id" = "by_head")) |>
      # Then try adjectival modifiers
      left_join(nom_amod_agents, by = c("doc_id", "sentence_id", "token_id" = "amod_head")) |>
      # Finally fall back to sentence subject
      left_join(sentence_subjects, by = c("doc_id", "sentence_id")) |>
      mutate(
        # Priority: possessive > by-phrase > amod > sentence subject
        subject_phrase = coalesce(poss_phrase, by_phrase, amod_phrase, sent_subject),
        subject_ner_type = coalesce(poss_ner, by_ner, amod_ner, sent_ner, ""),
        lemma_full = tolower(noun_lemma),
        is_future = FALSE,
        has_modal = FALSE,
        tag = "NN"
      ) |>
      filter(!is.na(subject_phrase)) |>
      select(doc_id, sentence_id, token_id, lemma_full, is_future, has_modal, tag,
             subject_phrase, subject_ner_type)

    # Combine with verb-based utterances
    utterances <- bind_rows(utterances, utterances_from_nouns) |> distinct()
  }

  # 4d. INFINITIVE CLAUSE EXTRACTION (xcomp)
  # CRITICAL for Appeal/Support detection
  # In "I urge you to vote", "I call upon all nations to unite":
  #   - The infinitive verb (vote, unite) is the ACTUAL action coded by human coders
  #   - The object of the main verb (you, nations) is the subject of the infinitive
  # This captures a major source of missing Appeal/Support utterances

  # Find xcomp relations (infinitive verbs linked to main verbs)
  xcomp_verbs <- parsed |>
    filter(dep_rel == "xcomp" & pos == "VERB") |>
    select(doc_id, sentence_id, token_id, xcomp_head = head_token_id,
           inf_lemma = lemma, inf_tag = tag)

  if (nrow(xcomp_verbs) > 0) {
    # Find the object of the main verb - this is the logical subject of the infinitive
    # In "urge you to vote", "you" is dobj of "urge" and logical subject of "vote"
    main_verb_objects <- parsed |>
      filter(dep_rel %in% c("dobj", "iobj", "pobj")) |>
      select(doc_id, sentence_id, obj_head = head_token_id,
             object_phrase = token, obj_ner = entity)

    # Also find prepositional objects for "call upon X" patterns
    pobj_after_upon <- parsed |>
      filter(dep_rel == "pobj") |>
      left_join(
        parsed |> select(doc_id, sentence_id, token_id, prep_token = token),
        by = c("doc_id", "sentence_id", "head_token_id" = "token_id")
      ) |>
      filter(tolower(prep_token) %in% c("upon", "on", "to", "for")) |>
      # Get the verb that governs the preposition
      left_join(
        parsed |> select(doc_id, sentence_id, token_id, prep_head = head_token_id),
        by = c("doc_id", "sentence_id", "head_token_id" = "token_id")
      ) |>
      select(doc_id, sentence_id, obj_head = prep_head,
             object_phrase = token, obj_ner = entity) |>
      filter(!is.na(obj_head))

    all_objects <- bind_rows(main_verb_objects, pobj_after_upon) |> distinct()

    # IMPROVED: Get main verb subjects for reflexive xcomp (e.g., "We intend to X")
    main_verb_subjects <- subjects |>
      select(doc_id, sentence_id, head_token_id, main_subject = subject_phrase,
             main_ner = subject_ner_type)

    # Create utterances from infinitive clauses
    # CRITICAL FIX: Distinguish between transitive xcomp (subject = object)
    # and reflexive xcomp (subject = main verb subject)
    utterances_from_xcomp <- xcomp_verbs |>
      # First, get the main verb's subject for reflexive cases
      left_join(main_verb_subjects,
                by = c("doc_id", "sentence_id", "xcomp_head" = "head_token_id")) |>
      # Then get objects for transitive cases
      left_join(all_objects, by = c("doc_id", "sentence_id", "xcomp_head" = "obj_head"),
                relationship = "many-to-many") |>
      # Take only the first match
      group_by(doc_id, sentence_id, token_id) |>
      slice(1) |>
      ungroup() |>
      mutate(
        lemma_full = tolower(inf_lemma),
        is_future = FALSE,
        has_modal = FALSE,
        is_conditional = FALSE,
        tag = inf_tag,
        # CRITICAL FIX: If object is explicit Other pronoun, use object
        # Otherwise, inherit main verb subject (for "We intend to X")
        subject_phrase = case_when(
          !is.na(object_phrase) & tolower(object_phrase) %in% c("them", "him", "her", "it", "you") ~ object_phrase,
          !is.na(main_subject) ~ main_subject,
          !is.na(object_phrase) ~ object_phrase,
          TRUE ~ NA_character_
        ),
        subject_ner_type = case_when(
          !is.na(object_phrase) & tolower(object_phrase) %in% c("them", "him", "her", "it", "you") ~ coalesce(obj_ner, ""),
          !is.na(main_ner) ~ main_ner,
          TRUE ~ coalesce(obj_ner, "")
        )
      ) |>
      filter(!is.na(subject_phrase)) |>
      select(doc_id, sentence_id, token_id, lemma_full, is_future, has_modal, is_conditional, tag,
             subject_phrase, subject_ner_type)

    # Combine with existing utterances
    utterances <- bind_rows(utterances, utterances_from_xcomp) |> distinct()
  }

  # --- E. Coding the Verb ---

  # Note: Ensure your dictionary includes phrasal verbs (e.g., "give up", "stand down")
  scored_utterances <- utterances |>
    mutate(lemma_lower = tolower(lemma_full)) |>
    mutate(
      opc_score = case_when(
        # 0. NEW: "Verb + against" pattern detection (must come first)
        # "voted against X" = Oppose, "fought against X" = Punish
        has_against & lemma_lower %in% verbs_punish_with_against ~ -3,
        has_against & lemma_lower %in% verbs_flip_with_against ~ -1,

        # 1. Check Punish (-3) - Future or Conditional Punish -> Threat (-2)
        lemma_lower %in% verbs_punish ~ ifelse(is_future | is_conditional, -2, -3),
        # 2. Check Reward (+3) - Future or Conditional Reward -> Promise (+2)
        lemma_lower %in% verbs_reward ~ ifelse(is_future | is_conditional, 2, 3),
        # 3. Words (Unchanged by future tense, they remain words)
        lemma_lower %in% verbs_threaten ~ -2,
        lemma_lower %in% verbs_promise ~ 2,
        lemma_lower %in% verbs_oppose ~ -1,
        lemma_lower %in% verbs_support ~ 1,

        # 4. Modal constructions ("should/must/need to + verb") = implicit appeal/support
        # "We should support..." "We must remember..." are coded as Support (+1) by human coders
        # Only apply to verbs NOT already in negative categories
        has_modal & !(lemma_lower %in% c(verbs_punish, verbs_threaten, verbs_oppose)) ~ 1,

        # 5. Nominalizations (action nouns like "liberation", "sanctions", "aid")
        # Human coders count these as actions even when expressed as nouns
        lemma_lower %in% nouns_punish ~ -3,
        lemma_lower %in% nouns_reward ~ 3,
        lemma_lower %in% nouns_threaten ~ -2,
        lemma_lower %in% nouns_oppose ~ -1,
        lemma_lower %in% nouns_support ~ 1,
        lemma_lower %in% nouns_promise ~ 2,  # NEW: Promise nominalizations

        TRUE ~ NA_real_
      )
    ) |>
    filter(!is.na(opc_score))
  
  # --- F. Specify Subject (Self vs Other) ---
  # IMPROVED: More flexible matching using NER + patterns
  # CRITICAL FIX: Handle "inclusive we" problem
  # When speakers say "We must support...", "We should remember...", human coders
  # recognize this as an APPEAL TO THE AUDIENCE (Other), not a Self action.
  # The presence of modal + positive verb with "we" = inclusive "we" = appeal to Other

  final_coding <- scored_utterances |>
    mutate(
      subject_clean = tolower(subject_phrase),
      subject_no_article = str_remove(subject_clean, "^(the|a|an)\\s+"),
      # Clean up NER type - extract just the entity type
      ner_type_clean = str_replace(subject_ner_type, "^[BI]-", "")

      # REMOVED: "inclusive we" rule was incorrectly converting Self to Other
      # Human coders treat "We should support..." as Self, not appeal to Other
    ) |>
    mutate(
      # Step 1: Check for Self entities
      is_self = (subject_no_article %in% own_terms | subject_clean %in% own_terms),

      # Step 2: Check for Other entities (exact match with country/org list)
      is_other_exact = subject_no_article %in% other_terms | subject_clean %in% other_terms,

      # Step 3: Check for common other pronouns
      # CRITICAL: Include "you" - when speaker addresses audience, this is Other
      # "You can help", "You must act" = appeal TO audience (Other)
      is_other_pronoun = subject_clean %in% c("they", "them", "he", "she", "it", "him", "her", "you"),

      # Step 4: Check for other indicators by name patterns (expanded)
      is_other_pattern = grepl(paste0("(",
        # Countries (expanded list)
        "iraq|iran|soviet|russia|china|north korea|syria|libya|cuba|vietnam|afghanistan|panama|",
        "japan|germany|france|britain|british|uk|england|poland|hungary|czechoslovakia|",
        "israel|egypt|saudi|jordan|lebanon|kuwait|pakistan|india|mexico|canada|brazil|",
        # Leaders (expanded)
        "saddam|hussein|gorbachev|noriega|castro|khomeini|assad|qaddafi|kim|kohl|mitterrand|",
        "thatcher|major|yeltsin|deng|xiaoping|arafat|rabin|mubarak|shamir|",
        # Generic other terms
        "communist|regime|enemy|terrorist|aggressor|dictator|tyrant|invader|occupier|",
        "ally|allies|partner|partners|friend|friends|neighbor|neighbors|neighbour|neighbours|",
        # Government institutions (very important - often subjects in political speech)
        "congress|senate|house|parliament|legislature|court|courts|supreme court|",
        "administration|cabinet|department|agency|agencies|",
        # Plural references
        "iraqis|iranians|soviets|russians|chinese|japanese|germans|french|british|",
        "forces|troops|army|military|authorities|officials|leaders|members|delegates|",
        "representatives|senators|congressmen|lawmakers|politicians|diplomats|ambassadors|",
        # Organizations (expanded)
        "un|united nations|nato|opec|kremlin|politburo|ec|european community|",
        "world bank|imf|oas|organization|coalition|alliance|bloc|pact|league|",
        # Generic institutional actors
        "government|governments|president|prime minister|chancellor|king|queen|",
        "secretary|minister|ministers|spokesman|spokesperson|official|director|chief|",
        # Media and other actors
        "media|press|newspaper|reporter|journalist|critic|critics|opposition|",
        # Groups
        "people|peoples|citizens|nation|nations|society|public|",
        "group|groups|party|parties|faction|movement|rebels|revolutionaries|insurgents",
      ")"), subject_clean, ignore.case = TRUE),

      # Step 5: Check NER entity type - GPE/ORG/PERSON/NORP/FAC/LOC that isn't Self -> Other
      is_ner_entity = ner_type_clean %in% c("GPE", "ORG", "PERSON", "NORP", "FAC", "LOC"),

      # Step 6: Check for common "Other" agent nouns (expanded list)
      is_other_agent_noun = grepl(paste0("(",
        # Collective and institutional actors
        "authority|authorities|delegation|delegations|leadership|",
        "adversary|adversaries|enemy|enemies|opponent|opponents|",
        "community|communities|population|populations|masses|",
        # International and generic terms
        "world|globe|humanity|mankind|international|foreign|",
        # Demonstrative references
        "those|these|others|other|another|",
        # Generic action agents
        "actor|actors|player|players|participant|participants|",
        "side|sides|counterpart|counterparts|",
        # Additional collective terms
        "body|bodies|council|councils|committee|committees|",
        "assembly|assemblies|delegation|delegations|mission|missions",
      ")"), subject_clean, ignore.case = TRUE),

      # Step 7: Default - assume non-Self subjects with country-like words are Other
      has_country_word = grepl("(nation|country|state|republic|kingdom|empire|federation|union)",
                               subject_clean, ignore.case = TRUE),

      # Step 8: Detect abstract collective entities ("all nations", "all peoples", "every citizen")
      # Human coders recognize these as Other even without explicit NER detection
      is_abstract_collective = grepl(paste0("(",
        # "All X" patterns
        "all nations|all peoples|all countries|all citizens|all people|",
        "all men|all women|all americans|all humanity|all mankind|",
        # "Every X" patterns
        "every nation|every people|every country|every citizen|every person|",
        "every man|every woman|every american|",
        # "The X of the world" patterns
        "nations of the world|peoples of the world|countries of the world|",
        "people of the world|citizens of the world|",
        # Collective humanity
        "humanity|mankind|humankind|civilized world|free world|",
        "civilized nations|free peoples|democratic nations|",
        # Generic audience terms
        "audience|listeners|viewers|readers|voters|electorate|",
        "fellow americans|fellow citizens|countrymen|compatriots",
      ")"), subject_clean, ignore.case = TRUE)
    ) |>
    mutate(
      entity_type = case_when(
        # PRIORITY 1: Standard Self check
        is_self ~ "Self",
        # PRIORITY 2: Explicit Other matches
        is_other_exact ~ "Other",
        is_other_pronoun ~ "Other",
        is_other_pattern ~ "Other",
        is_other_agent_noun ~ "Other",  # Generic agent nouns -> Other
        is_abstract_collective ~ "Other",  # NEW: "all nations", "humanity", etc. -> Other
        is_ner_entity & !is_self ~ "Other",  # NER-detected entities that aren't Self -> Other
        has_country_word & !is_self ~ "Other",  # Generic country references -> Other
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(entity_type)) |>
    dplyr::select(opc_score, entity_type)
  
  if (nrow(final_coding) == 0) {
    
    if(!bootstrap) {
      
      return(tibble(P1 = 0, P2 = 0, P3 = 0, P4 = 0, P5 = 0,
                    I1 = 0, I2 = 0, I3 = 0, I4a = 0, I4b = 0,
                    I5_punish = 0, I5_threat = 0, I5_oppose = 0, 
                    I5_support = 0, I5_promise = 0, I5_reward = 0,
                    self_reward = 0, self_promise = 0, self_support = 0,
                    self_oppose = 0, self_threat = 0, self_punish = 0,
                    other_reward = 0, other_promise = 0, other_support = 0,
                    other_oppose = 0, other_threat = 0, other_punish = 0))
      
    } else {
      
      return(tibble(meanP1 = 0, meanP2 = 0, meanP3 = 0, meanP4 = 0, meanP5 = 0,
                    meanI1 = 0, meanI2 = 0, meanI3 = 0, meanI4a = 0, meanI4b = 0,
                    meanI5_punish = 0, meanI5_threat = 0, meanI5_oppose = 0, 
                    meanI5_support = 0, meanI5_promise = 0, meanI5_reward = 0,
                    varP1 = 0, varP2 = 0, varP3 = 0, varP4 = 0, varP5 = 0,
                    varI1 = 0, varI2 = 0, varI3 = 0, varI4a = 0, varI4b = 0,
                    varI5_punish = 0, varI5_threat = 0, varI5_oppose = 0, 
                    varI5_support = 0, varI5_promise = 0, varI5_reward = 0,
                    meanself_reward = 0, meanself_promise = 0, meanself_support = 0,
                    meanself_oppose = 0, meanself_threat = 0, meanself_punish = 0,
                    meanother_reward = 0, meanother_promise = 0, meanother_support = 0,
                    meanother_oppose = 0, meanother_threat = 0, meanother_punish = 0,
                    varself_reward = 0, varself_promise = 0, varself_support = 0,
                    varself_oppose = 0, varself_threat = 0, varself_punish = 0,
                    varother_reward = 0, varother_promise = 0, varother_support = 0,
                    varother_oppose = 0, varother_threat = 0, varother_punish = 0,
                    ))
      
    }
    
  } else {
    
    if(!bootstrap) {
      
      res <- get_scores(final_coding)
      
      return(res)
      
    } else {
      
      boot_counts <- replicate(B, {
        
        final_coding$rowid <- 1:nrow(final_coding)
        sampled_id <- final_coding$rowid[sample(nrow(final_coding), replace = TRUE)]
        sample_df <- tibble(rowid = sampled_id)
        final_coding_sampled <- sample_df |> 
          inner_join(final_coding, by = "rowid", relationship = "many-to-many")
        
        get_scores(final_coding_sampled)
        
      }, simplify = TRUE)
      
      boot_counts_num <- apply(boot_counts, 2, as.numeric)
      rownames(boot_counts_num) <- rownames(boot_counts)
      row_means <- rowMeans(boot_counts_num, na.rm = TRUE)
      row_vars  <- apply(boot_counts_num, 1, var, na.rm = TRUE)
      
      stats_vec <- c(row_means, row_vars)
      stats_names <- c(paste0("mean", names(row_means)), 
                       paste0("var",  names(row_vars)))
      names(stats_vec) <- stats_names
      
      res <- bind_rows(stats_vec)
      
      return(res)
      
    }
    
  }
  
}
