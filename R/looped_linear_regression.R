# Looped linear regression ------------------------------------------------

library(tidyverse)

# Approach 1
run_multiple_lm_1 = function(df, outcomes) {
  
  `%>%` = magrittr::`%>%`
  
  run_lm = function(df, y) {
    model_results = 
      df %>%
      dplyr::mutate(.y = {{ y }}) %>%
      lm(.y ~ indep_var, .) %>%
      broom::tidy() %>% 
      dplyr::filter(!str_detect(term, "Intercept")) %>%
      dplyr::select(variable = term, p_value = p.value) %>% 
      dplyr::mutate(
        variable = df %>% select({{ y }}) %>% names(),
        p_value = case_when(
          p_value < 0.001 ~ "0.001",
          p_value > 0.001 ~ as.character(round(p_value, digits = 3))
        )
      )
    return(model_results)
  }
  
  outcomes = rlang::enquo(outcomes)
  selected = tidyselect::eval_select(outcomes, df)
  
  names(selected) %>%
    purrr::map( ~run_lm(df, !!sym(.))) %>%
    dplyr::bind_rows()
}

# Approach 2 (full NSE solution)
run_multiple_lm_2 = function(df, outcomes) {
  
  `%>%` = magrittr::`%>%`
  
  run_lm = function(df, y) {
    response_var = enquo(y)
    
    model_results = 
      lm(expr(!!sym(quo_name(response_var)) ~ indep_var), data = df) %>%
      broom::tidy() %>% 
      filter(!str_detect(term, "Intercept")) %>%
      select(variable = term, p_value = p.value) %>% 
      mutate(
        variable = df %>% select({{ y }}) %>% names(),
        p_value = case_when(
          p_value < 0.001 ~ "0.001",
          p_value > 0.001 ~ as.character(round(p_value, digits = 3))
        )
      )
    return(model_results)
  }
  
  outcomes = rlang::enquo(outcomes)
  selected = tidyselect::eval_select(outcomes, df)
  
  names(selected) %>%
    purrr::map( ~run_lm(df, !!sym(.))) %>%
    dplyr::bind_rows()
}
