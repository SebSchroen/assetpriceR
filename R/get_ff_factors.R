#' Download the asset pricing factors of Fama & French.
#' @param freq  Either "monthly" (default), "daily", or "annual".
#' @return A tibble with the six factors MKTRF, SMB, HML, RMW, CMA, and MOM as documented by Fama & French (1993, 2015, 2018)
#' @import dplyr frenchdata lubridate
#' @export
#' @description Downloads the factors of the Fama & French six factor model from the data library of Prof. Kenneth R. French.
#' @references Nelson Areal: `frenchdata`; Fama & French (1993, 2015, 2018)
#' @examples
#' \dontrun{
#' ff_factors_monthly <- get_ff_factors(freq = "monthly")
#' ff_factors_annual <- get_ff_factors(freq = "annual")
#' ff_factors_daily <- get_ff_factors(freq = "daily")
#' }

get_ff_factors <- function(freq = "monthly") {

if (freq == "daily") {
  factors <- frenchdata::download_french_data("Fama/French 5 Factors (2x3) [Daily]")$subsets$data[[1]] %>%
    left_join(
      frenchdata::download_french_data("Momentum Factor (Mom) [Daily]")$subsets$data[[1]],
      by = "date") %>%
    mutate_at(vars(-date), function(x) x/100) %>%
    mutate(CALDT = lubridate::ymd(date)) %>%
    rename(MKTRF = `Mkt-RF`,
           MOM = Mom) %>%
    select(-date)

    } else if (freq == "annual") {
      factors <- frenchdata::download_french_data("Fama/French 5 Factors (2x3)")$subsets$data[[2]] %>%
        left_join(
          frenchdata::download_french_data("Momentum Factor (Mom)")$subsets$data[[2]],
          by = "date") %>%
        mutate_at(vars(-date), function(x) x/100) %>%
        rename(YYYY = date,
               MKTRF = `Mkt-RF`,
               MOM = Mom)

    } else {
      factors <- frenchdata::download_french_data("Fama/French 5 Factors (2x3)")$subsets$data[[1]] %>%
        left_join(
          frenchdata::download_french_data("Momentum Factor (Mom)")$subsets$data[[1]],
          by = "date") %>%
        mutate_at(vars(-date), function(x) x/100) %>%
        rename(YYYYMM = date,
               MKTRF = `Mkt-RF`,
               MOM = Mom)

      }
return(factors)

}


