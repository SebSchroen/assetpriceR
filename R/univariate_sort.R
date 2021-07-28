#' Perform univariate sorts and compute returns for each portfolio.
#' @param data              A data frame with monthly excess returns for each KYPERMNO and the sorting variable `var`.
#' @param var               A variable to sort by.
#' @param ret               The (excess) return variable, default is MRETRF as MRET - RF.
#' @param n_portfolios       The number of portfolios to form, default is 10.
#' @param nyse_breakpoints  Boolean variable for NYSE-based breakpoints, default = `FALSE`.
#' @param price_filter      Boolean variable for applying a screening based on the formation price, default = `FALSE`.
#' @param min_price         `dbl` value for the minimum price used in the price screening, default is $5.
#' @return                  A tibble with portfolio returns, both equal- and value-weighted on a monthly basis.
#' @import                  dplyr lubridate
#' @export
#' @description Sort stocks into `n_portfolios` portfolios and compute value-weighted and equal-weighted monthly excess returns.
#' @examples
#' \dontrun{
#' size_deciles <- univariate_sort(sfz_mon, MTCAP, MRETRF)
#' }



univariate_sort <- function(data, var, ret = MRETRF,
                                            n_portfolios = 10,
                                            nyse_breakpoints = FALSE,
                                            price_filter = FALSE,
                                            min_price = 5){

##########Aux functions:
  get_breakpoints_functions <- function(var, n_portfolios = 10) {
    # Get relevant percentiles
    percentiles <- seq(0, 1, length.out = (n_portfolios + 1))
    percentiles <- percentiles[percentiles > 0 & percentiles < 1]

    # Construct set of named quantile functions
    percentiles_names <- map_chr(percentiles, ~str_c(rlang::quo_text(enquo(var)), "_q", .x*100))
    percentiles_funs <- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
      set_names(nm = percentiles_names)

    return(percentiles_funs)

  }


  get_portfolio <- function(x, breakpoints) {
    portfolio <- as.integer(1 + findInterval(x, unlist(breakpoints)))

    return(portfolio)
  }

  weighted_mean <- function(x, w, ..., na.rm = FALSE){
    if(na.rm){
      x1 <- x[!is.na(x) & !is.na(w)]
      w <- w[!is.na(x) & !is.na(w)]
      x <- x1
    }
    weighted.mean(x, w, ..., na.rm = FALSE)
  }
##########################

  #browser()

  # Keep only observations where sorting variable is defined, apply price filter

  if (price_filter == TRUE) {
  data <- data %>%
    filter(!is.na({{var}})) %>%
    group_by(KYPERMNO) %>%
    mutate(LAG_MPRC = lag(MPRC)) %>%
    filter(abs(LAG_MPRC) >= min_price) %>%
    select(-LAG_MPRC, MPRC) %>%
    ungroup()
  } else {
  data <- data %>%
      filter(!is.na({{var}})) %>%
      select(-MPRC)
  }



  # Determine breakpoints based on NYSE stocks only or on all stocks
  if (nyse_breakpoints == TRUE) {
    data_quantiles <- data %>%
      filter(PRIMEXCHG == "N") %>%
      select(MCALDT, {{var}})
  } else {
    data_quantiles <- data %>%
      select(MCALDT, {{var}})
  }

  # Compute quantiles




  var_funs <- get_breakpoints_functions({{var}}, n_portfolios)


  var_funs <- get_breakpoints_functions({{var}}, n_portfolios)
  quantiles <- data_quantiles %>%
    group_by(MCALDT) %>%
    summarize_at(vars({{var}}), lst(!!!var_funs)) %>%
    group_by(MCALDT) %>%
    nest(quantiles = -MCALDT)

  # Independently sort all stocks into portfolios based on breakpoints
  portfolios <- data %>%
    left_join(quantiles, by = "MCALDT") %>%
    mutate(portfolio = map2_dbl({{var}}, quantiles, get_portfolio)) %>%
    select(-quantiles)


  # Compute average portfolio characteristics
  portfolios_ts <- portfolios %>%
    group_by(KYPERMNO) %>%
    mutate(LAG_MTCAP = lag(MTCAP),
           LAG_RANK = lag(portfolio)) %>%
    drop_na(LAG_RANK, LAG_MTCAP) %>%
    group_by(LAG_RANK, MCALDT) %>%
    summarize(RET_EW = mean({{ret}}, na.rm = TRUE),
              RET_VW = weighted_mean({{ret}}, LAG_MTCAP, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(portfolio = LAG_RANK)


  # Construct long-short portfolio
  portfolios_ts_ls <- portfolios_ts %>%
    select(portfolio, MCALDT, RET_EW, RET_VW) %>%
    filter(portfolio %in% c(max(portfolio), min(portfolio))) %>%
    pivot_wider(names_from = portfolio, values_from = c(RET_EW, RET_VW))  %>%
    mutate(RET_EW = .[[3]] - .[[2]],
           RET_VW = .[[5]] - .[[4]],
           portfolio = paste0(n_portfolios, "-1"),
           YYYYMM = 100*lubridate::year(MCALDT) + lubridate::month(MCALDT)) %>%
    select(portfolio, MCALDT, RET_EW, RET_VW)

  # Combine everything
  out <- portfolios_ts %>%
    mutate(portfolio = as.character(portfolio)) %>%
    bind_rows(portfolios_ts_ls) %>%
    mutate(portfolio = factor(portfolio, levels = c(as.character(seq(1, n_portfolios, 1)), str_c(n_portfolios, "-1"))))

  return(out)
}
