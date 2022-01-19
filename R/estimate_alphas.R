#' Estimates factor model alphas on sorted portfolios.
#' @param data  A data frame with returns of sorted portfolios, e.g. the output of the `univariate_sort` function. The data must contain the FF6 factors as downloaded by the `get_ff_factors` function.
#' @param ret  Return variable, e.g. `RET_VW` for value-weighted returns, as returned by `univariate_sort`.
#' @param n_portfolios       The number of portfolios in the input data, default is 10.
#' @param lag      Lag for the Newey West adjustment of the standard errors.
#' @param datevar  Date variable, usually MCALDT with monthly data, use YYYYQ for quarterly data.
#' @return A tibble with returns and alphas for the sorted portfolios, ready to print with `kable`.
#' @import dplyr lubridate broom sandwich
#' @export
#' @description Produce a table with portfolio excess returns and alphas for the factor models of Fama & French (1993, 2015, 2018).
#' @examples
#' \dontrun{
#' size_deciles <- estimate_alphas(sfz_mon, MTCAP, MRETRF)
#' }



estimate_alphas <- function(data, ret, n_portfolios = 10, lag = 6, datevar = MCALDT ) {



    compute_alpha <- function(x, formula) {


      x %>% select(portfolio, {{datevar}}, ret = {{ret}}, MKTRF, SMB, HML, RMW, CMA, MOM,
                   MKT, ME, IA, ROE, EG) %>% group_by(portfolio)%>%
        nest()  %>%
        mutate(model = map(data, ~lm(formula, data = .)), nw_stderror = map_dbl(model, ~sqrt(diag(sandwich::NeweyWest(., lag = lag, prewhite = FALSE))[1])),
               model = map(model, broom::tidy)) %>% unnest(model) %>%
        ungroup() %>% filter(term == "(Intercept)") %>%
        mutate(nw_tstat = estimate/nw_stderror) %>% select(estimate,
                                                           nw_tstat) %>% t()
    }

    average_ret <- data %>% select(portfolio, {{datevar}}, ret = {{ret}} ) %>% group_by(portfolio) %>% nest(data = c({{datevar}}, ret)) %>%
      mutate(model = map(data, ~lm("ret ~ 1", data = .)),
             nw_stderror = map_dbl(model, ~sqrt(diag(sandwich::NeweyWest(., lag = lag, prewhite = FALSE)))), model = map(model, broom::tidy)) %>%
      unnest(model) %>% ungroup() %>% mutate(nw_tstat = estimate/nw_stderror) %>%
      select(estimate, nw_tstat) %>% t()


    capm_alpha <- compute_alpha(data, "ret ~ 1 + MKTRF")
    ff3_alpha <- compute_alpha(data, "ret ~ 1 + MKTRF + SMB + HML")
    ff5_alpha <- compute_alpha(data, "ret ~ 1 + MKTRF + SMB + HML + RMW + CMA")
    ff6_alpha <- compute_alpha(data, "ret ~ 1 + MKTRF + SMB + HML + RMW + CMA + MOM")
    q_alpha <-   compute_alpha(data, "ret ~ 1 + MKT + ME + IA + ROE + EG")
    out <- rbind(average_ret, capm_alpha, ff3_alpha, ff5_alpha,
                 ff6_alpha, q_alpha)
    colnames(out) <- c(as.character(seq(1, n_portfolios, 1)),
                       str_c(n_portfolios, "-1"))
    rownames(out) <- c("Excess Return", "t-Stat", "CAPM Alpha",
                       "t-Stat", "FF3 Alpha", "t-Stat", "FF5 Alpha", "t-Stat",
                       "FF6 Alpha", "t-Stat", "q-5 Alpha, ", "t-Stat")
    return(out)

}
