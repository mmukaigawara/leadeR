#' Classify LTA Traits into Hermann's Leadership Typology
#'
#' Takes per-speech LTA output (from [get_lta()]) and aggregates trait scores
#' across speeches, then classifies the leader along three dimensions
#' (constraint, openness, motivation toward world) and maps the first two plus
#' task orientation to one of eight leadership styles.
#'
#' @param lta A data frame with one row per speech, as returned by
#'   [get_lta()]. Must contain columns `Pp`, `B`, `C`, `Ss`, `Ta`, `D`, `Na`.
#'   When `precision_weighted = TRUE`, must also contain `varPp`, `varB`,
#'   `varC`, `varSs`, `varTa`, `varD`, `varNa`.
#' @param precision_weighted Logical. If `FALSE` (default), aggregate traits
#'   with a simple mean. If `TRUE`, use inverse-variance (precision) weighting
#'   via random-effects meta-analysis (`metafor::rma()`).
#' @param need_for_power Threshold for the need-for-power trait (`Pp`).
#'   Default `0.50`.
#' @param control Threshold for belief in ability to control events (`B`).
#'   Default `0.44`.
#' @param complex_high Threshold for high conceptual complexity (`C`).
#'   Default `0.56`.
#' @param confidence_high Threshold for high self-confidence (`Ss`).
#'   Default `0.81`.
#' @param task Threshold for task orientation (`Ta`). Default `0.59`.
#' @param distrust Threshold for distrust (`D`). Default `0.41`.
#' @param ingroup Threshold for in-group bias / nationalism (`Na`).
#'   Default `0.42`.
#' @return A one-row [tibble::tibble] with aggregated trait values, standard
#'   errors (when `precision_weighted = TRUE`), and classification columns:
#'   `constraint`, `openness`, `motivation_toward_world`, `task_orientation`,
#'   `typology`, and `method`.
#' @export
#' @examples
#' \dontrun{
#' spacyr::spacy_initialize()
#' res <- data.table::rbindlist(
#'   lapply(c(jfk19560921, jfk19570702, jfk19571101), function(x)
#'     get_lta(own_entity = "United States", text = clean_text(x),
#'             bootstrap = TRUE, B = 1000))
#' )
#' # Simple mean aggregation
#' type_lta(res)
#' # Precision-weighted aggregation
#' type_lta(res, precision_weighted = TRUE)
#' }
type_lta <- function(lta,
                     precision_weighted = FALSE,
                     need_for_power = 0.50,
                     control = 0.44,
                     complex_high = 0.56,
                     confidence_high = 0.81,
                     task = 0.59,
                     distrust = 0.41,
                     ingroup = 0.42) {

  traits <- c("Pp", "B", "C", "Ss", "Ta", "D", "Na")

  ## --- Aggregation ---
  if (precision_weighted) {
    if (!requireNamespace("metafor", quietly = TRUE)) {
      stop("Package 'metafor' is required for precision weighting. ",
           "Install it with install.packages('metafor').", call. = FALSE)
    }

    var_traits <- paste0("var", traits)
    missing_vars <- setdiff(var_traits, names(lta))
    if (length(missing_vars) > 0) {
      stop("Columns missing for precision weighting: ",
           paste(missing_vars, collapse = ", "),
           ". Run get_lta() with bootstrap = TRUE.", call. = FALSE)
    }

    estimates <- vapply(seq_along(traits), function(i) {
      yi <- lta[[traits[i]]]
      vi <- lta[[var_traits[i]]]
      vi <- pmax(vi, 1e-8, na.rm = TRUE)
      fit <- metafor::rma(yi = yi, vi = vi, method = "REML")
      c(mu = as.numeric(fit$b), se = as.numeric(fit$se))
    }, numeric(2))

    vals <- stats::setNames(estimates[1, ], traits)
    ses  <- stats::setNames(estimates[2, ], paste0("se_", traits))
    out  <- tibble::as_tibble(c(as.list(vals), as.list(ses)))

  } else {
    vals <- vapply(traits, function(tr) mean(lta[[tr]], na.rm = TRUE), numeric(1))
    out  <- tibble::as_tibble(as.list(vals))
  }

  ## --- Classification ---
  out$constraint <- ifelse(
    vals["Pp"] < need_for_power & vals["B"] < control,
    "Respect", "Challenge"
  )

  out$openness <- ifelse(
    vals["C"] > vals["Ss"] |
      (vals["C"] > complex_high & vals["Ss"] > confidence_high),
    "Open", "Closed"
  )

  out$motivation_toward_world <- ifelse(
    vals["D"] < distrust & vals["Na"] < ingroup, "Cooperative",
    ifelse(vals["D"] < distrust & vals["Na"] >= ingroup,
           "Cooperative (in-group bias)",
           ifelse(vals["D"] >= distrust & vals["Na"] < ingroup,
                  "Competitive (out-group focus)", "Competitive"))
  )

  out$task_orientation <- ifelse(vals["Ta"] > task, "Problem", "Relationship")

  ## --- Typology mapping ---
  typology_map <- list(
    Challenge = list(
      Closed = list(Problem = "Expansionistic", Relationship = "Evangelistic"),
      Open   = list(Problem = "Incremental",    Relationship = "Charismatic")
    ),
    Respect = list(
      Closed = list(Problem = "Directive",  Relationship = "Consultative"),
      Open   = list(Problem = "Reactive",   Relationship = "Accommodative")
    )
  )

  out$typology <- typology_map[[out$constraint]][[out$openness]][[out$task_orientation]]
  out$method <- ifelse(precision_weighted, "precision_weighted", "simple_mean")

  out
}
