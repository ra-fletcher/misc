# Right-truncated Cox regression ------------------------------------------

# Cuts follow-up to a specific day, and runs a Cox model

run_cox = function(data, day, ep_var, ep_date_var) {
  # Arguments
  # ---------
  # data : tibble
  # day : integer, day at which to truncate follow-up
  # ep_var : binary endpoint variable (`1` == event, `0` == no event)
  # ep_date_var : endpoint date variable (date of event or censoring)
  #
  # Returns
  # -------
  # tibble (model results)
  
  model_data = data %>%
    mutate(
      trunc_date = start_date + days({{ day }}),
      new_ep = case_when(
        {{ ep_var }} == 1 & {{ ep_date_var }} <= trunc_date ~ 1,
        TRUE ~ 0
      ),
      new_date = case_when(
        new_ep == 1 ~ {{ ep_date_var }},
        new_ep == 0 & {{ ep_date_var }} < trunc_date ~ {{ ep_date_var }},
        TRUE ~ trunc_date
      ),
      new_time_to_event = as.numeric(new_date - start_date)
    ) 
  
  model_results = suppressWarnings(
    model_data %>%
      coxph(
        Surv(new_time_to_event, new_ep) ~ treatment_control + strata(strata), 
        data = .
      ) %>%
      broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
      select(
        term,
        hr = estimate,
        l_ci = conf.low,
        u_ci = conf.high,
        se = std.error,
        statistic,
        p_value = p.value
      ) %>%
      mutate(days = day)
  ) %>% 
    bind_cols(
      model_data %>% summarise(events = sum(new_ep[new_ep == 1]))
    )
}

# Loop function to run function for each day of follow-up
model_results = 
  purrr::map(
    .x = c(1:1653), 
    ~run_cox(data, .x, cardiorenal_ep, cardiorenal_ep_date)
  ) %>%
  bind_rows()


# Similar function but for mixed effects Cox regression -------------------

run_coxme  = function(day, ep_var, ep_date_var) {
  # Arguments
  # ---------
  # day : integer, day at which to truncate follow-up
  # ep_var : binary endpoint variable (`1` == event, `0` == no event)
  # ep_date_var : endpoint date variable (date of event or censoring)
  #
  # Returns
  # -------
  # tibble (model results (fixed effects))
  
  extract_coxme_tbl = function(model) {
    beta = model$coefficients
    nvar = length(beta)
    nfrail = nrow(model$variance) - nvar
    se = sqrt(diag(model$var)[nfrail + 1:nvar])
    z = round(beta/se, 5)
    p = signif(1 - pchisq((beta/se)^2, 1), 5)
    tbl = 
      suppressMessages(
        tibble(bind_cols(beta, se, z, p)) %>%
          rename(
            coef = `...1`,
            se = `...2`,
            z = `...3`,
            p_value = `...4`
          ) %>%
          mutate(
            hr = exp(coef),
            l_ci = exp(coef - 1.96 * se),
            u_ci = exp(coef + 1.96 * se)
          ) %>%
          add_column(var = "cana vs. placebo", .before = 1)
      )
    return(tbl)
  }
  
  model_data = comb_analysis_data %>%
    mutate(
      trunc_date = start_date + days({{ day }}),
      new_ep = case_when(
        {{ ep_var }} == 1 & {{ ep_date_var }} <= trunc_date ~ 1,
        TRUE ~ 0
      ),
      new_date = case_when(
        new_ep == 1 ~ {{ ep_date_var }},
        new_ep == 0 & {{ ep_date_var }} < trunc_date ~ {{ ep_date_var }},
        TRUE ~ trunc_date
      ),
      new_time_to_event = as.numeric(new_date - start_date)
    ) 
  
  model_results = 
    suppressWarnings(
      extract_coxme_tbl(
        model_data %>%
          coxme(
            Surv(new_time_to_event, new_ep) ~ treatment_control + (1 | trial), 
            data = .
          )
      )
    ) %>% 
    mutate(days = day) %>%
    bind_cols(
      model_data %>% summarise(events = sum(new_ep[new_ep == 1]))
    )
  return(model_results)
}