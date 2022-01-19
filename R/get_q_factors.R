#' Download the asset pricing factors of Hou et al. (2019)
#' @param freq  Either "monthly" (default), "daily", or "annual".
#' @return A tibble with the five factors MKTRF, ME, IA, ROE, EG as documented by Hou et al. (2019)
#' @import dplyr readr lubridate
#' @export
#' @description Downloads the factors of the Hou et al. (2019) q5 model from the data library q-global.org
#' @references  Hou et al. (2019)
#' @examples
#' \dontrun{
#' q_factors_monthly <- get_q_factors(freq = "monthly")
#' q_factors_annual <- get_q_factors(freq = "annual")
#' q_factors_daily <- get_q_factors(freq = "daily")
#' }

get_q_factors <- function(freq = "monthly") {

if (freq == "daily") {
  factors <- readr::read_csv("http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_daily_2020.csv",
                                              show_col_types = FALSE) %>%
    mutate_at(vars(-DATE), function(x) x/100) %>%
    mutate(CALDT = lubridate::ymd(DATE)) %>%
    dplyr::select(-DATE) %>%
    rename_at(vars(-CALDT,-R_F), ~str_remove(.,"R_")) %>%
    rename(RF = R_F) %>%
    dplyr::select(-RF)

    } else if (freq == "annual") {
      factors <- readr::read_csv("http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_annual_2020.csv",
                                 show_col_types = FALSE) %>%
        mutate(YYYY =year) %>%
        dplyr::select(-c(year)) %>%
        mutate_at(vars(-YYYY), function(x) x/100) %>%
        rename_at(vars(-YYYY,-R_F), ~str_remove(.,"R_")) %>%
        rename(RF = R_F) %>%
        dplyr::select(YYYY, MKT, ME, IA, ROE, EG)
    }
  else if (freq == "quarterly") {
    factors <- readr::read_csv("http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_quarterly_2020.csv",
                               show_col_types = FALSE) %>%
      mutate(YYYYQ = 100*year + quarter) %>%
      dplyr::select(-c(year, quarter)) %>%
      mutate_at(vars(-YYYYQ), function(x) x/100) %>%
      rename_at(vars(-YYYYQ,-R_F), ~str_remove(.,"R_")) %>%
      rename(RF = R_F) %>%
      dplyr::select(YYYYQ, MKT, ME, IA, ROE, EG)



  }  else {
    factors <- readr::read_csv("http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2020.csv",
                               show_col_types = FALSE) %>%
      mutate(YYYYMM = 100*year + month) %>%
      dplyr::select(-c(year, month)) %>%
      mutate_at(vars(-YYYYMM), function(x) x/100) %>%
      rename_at(vars(-YYYYMM,-R_F), ~str_remove(.,"R_")) %>%
      rename(RF = R_F) %>%
      dplyr::select(YYYYMM, MKT, ME, IA, ROE, EG)




      }
return(factors)

}


