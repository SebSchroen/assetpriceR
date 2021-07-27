#' Estimate rolling window betas in the cross section of stocks.
#' @param data  A data frame with daily or monthly returns for each KYPERMNO and at least one factor to estimate OLS betas.
#' @param regmodel  A regression formula `y~x` to estimate factor betas.
#' @param window    A `dbl` value for the estimation window in months in {1,3,6,12,24,...,60}.
#' @param freq      Either "monthly" (default) or "daily" for the input frequency of the data.
#' @return A tibble with the estimated betas for all input factors on a monthly basis
#' @import dplyr lubridate roll
#' @export
#' @description Estimate rolling window betas in the cross section of stocks.
#' @examples
#' \dontrun{
#' capm_beta_monthly <- estimate_rolling_betas(sfz_mon, "RETRF~MKTRF", 60, "monthly")
#' }


estimate_rolling_betas <- function(data,
                                   regmodel,
                                   window,
                                   freq = "monthly") {




# Thats not very smart, fix that:
  ret <- colnames(model.frame(formula = regmodel, data = data))[1]

 auxroll <- function(x) {
   #x <- na.omit(data)
   mf <- model.frame(formula = regmodel, data = x)
   tt <- terms(mf)
   tt <- update(tt, ~ . -1)
   X <- model.matrix(tt, mf)
   y <- model.response(mf)
   # Index -1 makes sure to drop the intercept efficiently


   out <- as_tibble(roll_lm(X, y, width = window*k, min_obs = min_obs)$coefficients) %>%
     # Takes 20% more time with select:
     select(-"(Intercept)") %>%
     setNames(paste0('BETA_', names(.), "_", window,"M_",toupper(freq)))

   out <-cbind(x, out)



   return(out)

 }





  if (freq == "monthly") {
    k <- 1
    if (window == 12) {min_obs <- 10}
    if (window == 24) {min_obs <- 20}
    if (window == 36) {min_obs <- 24}
    if (window == 60) {min_obs <- 24}


    output <- data %>%
      drop_na(ret) %>%
      arrange(KYPERMNO, MCALDT) %>%
      group_by(KYPERMNO) %>%
      nest()%>%
      mutate(data = map(data, auxroll)) %>%
      unnest(data) %>%
      mutate(YYYYMM = 100*lubridate::year(MCALDT)+ lubridate::month(MCALDT)) %>%
      select(KYPERMNO, YYYYMM, starts_with("BETA")) %>%
      drop_na()






  }
  if (freq == "daily") {
    k <- 21
    if (window == 1) {min_obs <- 15}
    if (window == 3) {min_obs <- 50}
    if (window == 6) {min_obs <- 100}
    if (window == 12) {min_obs <- 200}
    if (window == 24) {min_obs <- 450}
    if (window == 36) {min_obs <- 600}
    if (window == 48) {min_obs <- 900}
    if (window == 60) {min_obs <- 1000}



    output <- data %>%
      drop_na(ret) %>%
      arrange(KYPERMNO, CALDT) %>%
      group_by(KYPERMNO) %>%
      nest()%>%
      mutate(data = map(data, auxroll)) %>%
      unnest(data) %>%
      mutate(YYYYMM = 100*lubridate::year(CALDT)+ lubridate::month(CALDT)) %>%
      group_by(YYYYMM) %>%
      mutate(MAX_DATE = max(CALDT)) %>%
      filter(CALDT == MAX_DATE) %>%
      ungroup() %>%
      select(KYPERMNO, YYYYMM, starts_with("BETA")) %>%
      drop_na()







  }


  return(output)









}
