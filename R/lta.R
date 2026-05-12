#' Leadership Trait Analysis (LTA)
#'
#' Runs all eight LTA trait functions at once and returns a single combined
#' tibble. The eight traits are: power, affiliation, distrust, conceptual
#' complexity, task orientation, self-confidence, nationalism, and control.
#'
#' @param own_entity A character string identifying the speaker's country or
#'   entity (e.g., `"United States"`).
#' @param text A character string containing the speech text to analyse.
#' @param bootstrap Logical. If `TRUE`, returns bootstrap means and variances
#'   instead of raw counts. Default is `FALSE`.
#' @param B Number of bootstrap iterations. Default is `1000`.
#' @return A one-row [tibble::tibble].
#'
#'   When `bootstrap = FALSE`, columns include raw counts (`P`, `OP`, `A`,
#'   `OA`, `S`, `OS`, `HC`, `LC`, `TI`, `IP`, `SC`, `OSC`, `N`, `ON`, `IC`,
#'   `OC`) plus trait proportions (`Pp`, `D`, `C`, `Ta`, `Ss`, `Na`, `B`).
#'
#'   When `bootstrap = TRUE`, columns include bootstrap means and variances
#'   for the raw counts (`meanP`, `varP`, `meanOP`, `varOP`, ...) plus trait
#'   proportions and their delta-method variances (`Pp`, `varPp`, `D`, `varD`,
#'   `C`, `varC`, `Ta`, `varTa`, `Ss`, `varSs`, `Na`, `varNa`, `B`, `varB`).
#' @export
#' @examples
#' \donttest{
#' # Requires spaCy to be installed; see spacyr::spacy_install().
#' spacyr::spacy_initialize()
#' get_lta(own_entity = "United States", text = "We will defend our nation.")
#' get_lta(own_entity = "United States", text = "We will defend our nation.",
#'         bootstrap = TRUE, B = 500)
#' }
get_lta <- function(own_entity, text, bootstrap = FALSE, B = 1000) {

  res_power   <- get_power(own_entity = own_entity, text = text,
                           bootstrap = bootstrap, B = B)
  res_aff     <- get_aff(own_entity = own_entity, text = text,
                         bootstrap = bootstrap, B = B)
  res_dist    <- get_dist(own_entity = own_entity, text = text,
                          bootstrap = bootstrap, B = B)
  res_complex <- get_complex(text = text, bootstrap = bootstrap, B = B)
  res_task    <- get_task(text = text, bootstrap = bootstrap, B = B)
  res_conf    <- get_conf(text = text, bootstrap = bootstrap, B = B)
  res_nat     <- get_nat(own_entity = own_entity, text = text,
                         bootstrap = bootstrap, B = B)
  res_ctrl    <- get_ctrl(own_entity = own_entity, text = text,
                          bootstrap = bootstrap, B = B)

  dat_out <- dplyr::bind_cols(
    res_power, res_aff, res_dist, res_complex, res_task, res_conf, res_nat,
    res_ctrl
  )

  if (bootstrap) {
    dat_out <- dat_out |>
      mutate(
        Pp = if_else((meanP + meanOP) == 0, 0,
                     meanP / (meanP + meanOP)),
        varPp = if_else((meanP + meanOP) == 0, 0,
                        ((meanOP / (meanP + meanOP)^2)^2) * varP +
                          ((-meanP / (meanP + meanOP)^2)^2) * varOP),

        D = if_else((meanS + meanOS) == 0, 0,
                    meanS / (meanS + meanOS)),
        varD = if_else((meanS + meanOS) == 0, 0,
                       ((meanOS / (meanS + meanOS)^2)^2) * varS +
                         ((-meanS / (meanS + meanOS)^2)^2) * varOS),

        C = if_else((meanHC + meanLC) == 0, 0,
                    meanHC / (meanHC + meanLC)),
        varC = if_else((meanHC + meanLC) == 0, 0,
                       ((meanLC / (meanHC + meanLC)^2)^2) * varHC +
                         ((-meanHC / (meanHC + meanLC)^2)^2) * varLC),

        Ta = if_else((meanTI + meanIP) == 0, 0,
                     meanTI / (meanTI + meanIP)),
        varTa = if_else((meanTI + meanIP) == 0, 0,
                        ((meanIP / (meanTI + meanIP)^2)^2) * varTI +
                          ((-meanTI / (meanTI + meanIP)^2)^2) * varIP),

        Ss = if_else((meanSC + meanOSC) == 0, 0,
                     meanSC / (meanSC + meanOSC)),
        varSs = if_else((meanSC + meanOSC) == 0, 0,
                        ((meanOSC / (meanSC + meanOSC)^2)^2) * varSC +
                          ((-meanSC / (meanSC + meanOSC)^2)^2) * varOSC),

        Na = if_else((meanN + meanON) == 0, 0,
                     meanN / (meanN + meanON)),
        varNa = if_else((meanN + meanON) == 0, 0,
                        ((meanON / (meanN + meanON)^2)^2) * varN +
                          ((-meanN / (meanN + meanON)^2)^2) * varON),

        B = if_else((meanIC + meanOC) == 0, 0,
                    meanIC / (meanIC + meanOC)),
        varB = if_else((meanIC + meanOC) == 0, 0,
                       ((meanOC / (meanIC + meanOC)^2)^2) * varIC +
                         ((-meanIC / (meanIC + meanOC)^2)^2) * varOC)
      )
  } else {
    dat_out <- dat_out |>
      mutate(
        Pp = if_else((P + OP) == 0, 0, P / (P + OP)),
        D  = if_else((S + OS) == 0, 0, S / (S + OS)),
        C  = if_else((HC + LC) == 0, 0, HC / (HC + LC)),
        Ta = if_else((TI + IP) == 0, 0, TI / (TI + IP)),
        Ss = if_else((SC + OSC) == 0, 0, SC / (SC + OSC)),
        Na = if_else((N + ON) == 0, 0, N / (N + ON)),
        B  = if_else((IC + OC) == 0, 0, IC / (IC + OC))
      )
  }

  dat_out
}
