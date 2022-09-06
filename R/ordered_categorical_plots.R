#*******************************************************************************
#
# Date:    06-Sep-2022
# Author:  Rob Fletcher
# Purpose: Create ordered categorical plots
#
#*******************************************************************************


# Notes -------------------------------------------------------------------

# The below code allows for the creation of ordered categorical plots from my
# blood pressure variability paper, looking at the shape of association between
# quintiles of systolic blood pressure variability and clinical outcomes

# Below is a description of each variable included in the model results that is
# required for creation of these plots

# `grp` : group number (in the example below quintiles are used so there are
#         five groups)
# `hr` : group hazard ratios from Cox regression
# `lci` : group hazard ratio lower 95% confidence interval (NB variance of the 
#         log risk in each group are used to calculate group-specific 95% 
#         confidence intervals)
# `uci` : group hazard ratio upper 95% confidence interval
# `se` : group-specific standard errors used for scaling of the hazard ratio 
#        markers on the plots
# `events` : number of events in each group (this is printed below each marker
#            on the plots)
# `median` : median standard deviation of systolic blood pressure variability 
#            value for each group (used for placement of square markers on the
#            x axis)
# `hr_cont` : inverse-variance weighted mean change in the hazard ratio for the 
#             three trials used in the analysis per increase in 5 mm Hg of 
#             standard deviation of systolic blood pressure variability
# `lci_cont` : lower 95% confidence interval for the hazard ratio per increase 
#              in 5 mm Hg of standard deviation of systolic blood pressure 
#              variability
# `uci_cont` : upper 95% confidence interval for the hazard ratio per increase 
#              in 5 mm Hg of standard deviation of systolic blood pressure 
#              variability
# `p_trend` : p-value for the the hazard ratio per increase in 5 mm Hg of 
#             standard deviation of systolic blood pressure variability
# `unit` : character string ("per 5 mm Hg") printed on the plot alongside the
#          model results

# Please make sure to install all dependencies before running this code


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Define variables --------------------------------------------------------

# Define plot main colour (axes, text, etc.)
off_black = "#2F3A44"

# Define plot square marker colour
ocean_blue = "#0065C4"

# Define plot font
font = "Arial"


# Define functions --------------------------------------------------------

process_data = function(.data, ...) {
  # Process model data for plotting
  #
  # Arguments
  # ---------
  # .data : tibble (model data)
  # ... : additional arguments passed on from other functions (in this instance
  #       it is used to select the variables required for processing)
  #
  # Returns
  # -------
  # tibble
  
  .data = .data %>% 
    mutate(
      across(
        c(...), 
        ~ case_when(
          str_detect(
            ., "^[0-9]{1,2}[.][0-9]$|^[-][0-9]{1,2}[.][0-9]$"
          ) ~ paste0(., "0"), 
          str_detect(., "^1$") ~ paste0(., ".00"),
          TRUE ~ as.character(.)
        ),
        .names = "{col}_char" 
      ),
      results = case_when(
        grp == 1 ~
          paste0(
            hr_cont_char, " (95% CI ", lci_cont_char, " to ", uci_cont_char, 
            ")\n", unit, "; p", p_trend
          )
      )
    )
}

plot_ordered_sd_plot = function(.data, 
                                .panel_label, 
                                .outcome_label, 
                                .label_length = "short", 
                                .variance = "low", 
                                .outcome_type = "cv",
                                .eskd = FALSE,
                                .main_colour = "black",
                                .square_colour = "black",
                                .font_family = "Arial") {
  # Create ordered categorical plot showing the association of groups of
  # standard deviation of systolic blood pressure variability with clinical
  # outcomes
  #
  # Arguments
  # ---------
  # .data : tibble (model data)
  # .panel_label : character string (the panel label for your figure, i.e. "A", 
  #                "B", "C", etc.)
  # .outcome_label : character string (the outcome label for your figure, i.e.
  #                  "All-cause mortality" or "Hospitalisation for heart 
  #                  failure)
  # .label_length : character string (options include: "short" (default) and 
  #                 "long"; if "long" specified, the positioning of 
  #                 `.outcome_label` will be adjusted to allow for proper 
  #                 alignment of multiple lines)
  # .variance : character string (options include: "low" (default) and 
  #             "high"; controls the area scaling the square markers, which are
  #             inversely-proportional to the variance of the group)
  # .outcome_type : character string (options include: "cv" (default) and 
  #                 "kidney"; controls where the results of the dashed line are
  #                 printed on the plot. If "cv" is specified, then the results 
  #                 are printed on the bottom right of the plot. If "kidney" is
  #                 specified, the results are printed on the top right where 
  #                 they don't get in the way of the confidence intervals
  # .eskd : Boolean (adjusts the scaling of the y-axis for the kidney failure
  #         outcome to accommodate the very wide confidence intervals)
  # .main_colour : character string (controls the colour of the axes, text, 
  #                etc., default setting is "black")
  # .square_colour : character string (controls the colour of the square markers
  #                  on the plot, default setting is "black")
  # .font_family : character string (controls the font used in the plot, default
  #                setting is "Arial")
  #
  # Returns
  # -------
  # ggplot object
  
  # Coordinates for the line based on the gradient of the regression
  
  # Gradient
  grad = .data %>% dplyr::slice_head() %>% dplyr::pull(hr_cont)
  grad_per1 = (grad - 1) / 5
  
  # Beginning  
  x1 = .data %>% dplyr::slice_head() %>% dplyr::pull(median)
  x1 = x1 - 1.1
  y1 = (grad_per1 * x1) + 1.0
  
  # End
  x2 = .data %>% dplyr::slice_tail() %>% dplyr::pull(median)
  x2 = x2 + 1.1
  y2 = (grad_per1 * x2) + 1.0
  
  plot = 
    ggplot2::ggplot(data = {{ .data }}, aes(x = median, y = hr)) + 
    ggplot2::geom_errorbar(
      aes(ymin = lci, ymax = uci, group = grp), width = 0, size = 0.8, 
      colour = {{ .main_colour }}
    ) + 
    ggplot2::geom_segment(
      aes(x = -2, y = 0.5, xend = -2, yend = 4.0), size = 0.8,
      colour = {{ .main_colour }}
    ) +
    # Regression line
    ggplot2::geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2), size = 0.8, 
      colour = {{ .main_colour }}, linetype = "dashed"
    )
  
  # Add squares for hazard ratio estimates
  if (.variance == "low") {
    plot = plot + 
      ggplot2::geom_point(
        colour = {{ .square_colour }}, size =  0.075/((.data$se)^2), shape = 15
      )
  } else if (.variance == "high") {
    plot = plot + 
      ggplot2::geom_point(
        colour = {{ .square_colour }}, size =  0.1/((.data$se)^2), shape = 15
      )
  }
  
  if (.outcome_type == "cv") {
    plot = plot + 
      ggplot2::geom_text(
        aes(y = 0.5, x = 12, label = results), hjust = 0, vjust = 0, size = 3,
        colour = {{ .main_colour }}, family = {{ .font_family }}
      )
  } else if (.outcome_type == "kidney") {
    plot = plot + 
      ggplot2::geom_text(
        aes(y = 3.5, x = 12, label = results), hjust = 0, vjust = 1, size = 3,
        colour = {{ .main_colour }}, family = {{ .font_family }}
      )
  }
  
  plot = plot +
    ggplot2::geom_text(
      aes(y = 2^(log2(uci) + 0.1), label = hr_char), hjust = 0.5, size = 3, 
      colour = {{ .main_colour }}, family = {{ .font_family }}
    ) +
    ggplot2::geom_text(
      aes(y = 2^(log2(lci) - 0.1), label = events), hjust = 0.5, size = 3, 
      colour = {{ .main_colour }}, family = {{ .font_family }}
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0), 
      limits = c(-2, 20),
      breaks = c(0, 5, 10, 15, 20), 
      labels = c("0", "5", "10", "15", "20")
    ) 
  
  if (.eskd == FALSE) {
    plot = plot +
      ggplot2::scale_y_log10(
        expand = c(0, 0),
        limits = c(0.4, 6.0),
        breaks = c(0.5, 0.75, 1.0, 2.0, 4.0), 
        labels = c("0.5", "0.75", "1.0", "2.0", "4.0")
      ) +  
      ggplot2::geom_segment(
        aes(x = 0, y = 0.4, xend = 20, yend = 0.4), size = 0.8,
        colour = {{ .main_colour }}
      )
  } else if (.eskd == TRUE) {
    plot = plot +
      ggplot2::scale_y_log10(
        expand = c(0, 0),
        limits = c(0.35, 6.0),
        breaks = c(0.5, 0.75, 1.0, 2.0, 4.0),
        labels = c("0.5", "0.75", "1.0", "2.0", "4.0")
      )+
      ggplot2::geom_segment(
        aes(x = 0, y = 0.35, xend = 20, yend = 0.35), size = 0.8, 
        colour = {{ .main_colour }}
      )
  }
  
  plot = plot +
    ggplot2::labs(
      x = "         Systolic blood pressure standard deviation (mm Hg)",
      y = "Hazard ratio (95% CI)"
    ) +
    ggplot2::theme(
      text = element_text(family = {{ .font_family }}),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks.x = element_line(size = 0.4, colour = {{ .main_colour }}),
      axis.ticks.y =  element_line(size = 0.4, colour = {{ .main_colour }}),
      axis.ticks.length = unit(0.125, "cm"),
      axis.text.x = element_text(
        size = 9, color = {{ .main_colour }}, margin = margin(4,0,0,0,"pt")
      ),
      axis.text.y = element_text(size = 9, colour = {{ .main_colour }}),
      axis.title.x = element_text(
        size = 11, colour = {{ .main_colour }}, vjust = -2
      ),
      axis.title.y = element_text(
        size = 11, colour = {{ .main_colour }}, vjust = 1
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    ggplot2::geom_text(
      aes(x = 0, y = 5, label = {{ .panel_label }}, fontface = "bold"), 
      size = 7, hjust = 0, colour = {{ .main_colour }}, family = {{ .font_family }}
    )
  
  if (.label_length == "short") {
    plot = plot +
      ggplot2::geom_text(
        aes(x = 1.8, y = 5, label = {{ .outcome_label }}), size = 3.5, 
        vjust = 0, hjust = 0, colour = {{ .main_colour }}, 
        family = {{ .font_family }}
      )
  } else if (.label_length == "long") {
    plot = plot + 
      ggplot2::geom_text(
        aes(x = 1.8, y = 5, label = {{ .outcome_label }}), size = 3.5, 
        hjust = 0, colour = {{ .main_colour }}, family = {{ .font_family }}
      )
  }
  
  return(plot)
}


# Define reprex data ------------------------------------------------------

# Data for all-cause mortality
dth = 
  tibble::tibble(
    grp = c(1, 2, 3, 4, 5),
    hr = c(1.00, 1.08, 1.16, 1.15, 1.39),
    lci = c(0.76, 0.86, 0.95, 0.94, 1.17),
    uci = c(1.24, 1.30, 1.36, 1.35, 1.62),
    se = c(0.123, 0.113, 0.106, 0.106, 0.115),
    events = c(73, 82, 88, 87, 114),
    median = c(3.32, 5.74, 8.06, 10.74, 15.81),
    hr_cont = c(1.12, NA, NA, NA, NA),
    lci_cont = c(1.01, NA, NA, NA, NA),
    uci_cont = c(1.24, NA, NA, NA, NA),
    p_trend = c("=0.039", NA, NA, NA, NA),
    unit = c("per 5 mm Hg", NA, NA, NA, NA)
  ) %>% 
  process_data(hr, hr_cont, lci_cont, uci_cont)

# Data for hospitalisation for heart failure
hhf = 
  tibble::tibble(
    grp = c(1, 2, 3, 4, 5),
    hr = c(1.00, 1.03, 1.25, 1.45, 1.86),
    lci = c(0.59, 0.65, 0.91, 1.16, 1.56),
    uci = c(1.41, 1.42, 1.59, 1.74, 2.16),
    se = c(0.210, 0.197, 0.172, 0.147, 0.155),
    events = c(25, 27, 34, 44, 67),
    median = c(3.32, 5.74, 8.06, 10.74, 15.81),
    hr_cont = c(1.18, NA, NA, NA, NA),
    lci_cont = c(1.02, NA, NA, NA, NA),
    uci_cont = c(1.36, NA, NA, NA, NA),
    p_trend = c("=0.030", NA, NA, NA, NA),
    unit = c("per 5 mm Hg", NA, NA, NA, NA)
  ) %>% 
  process_data(hr, hr_cont, lci_cont, uci_cont)

# Data for kidney failure
eskd = 
  tibble::tibble(
    grp = c(1, 2, 3, 4, 5),
    hr = c(1.00, 1.38, 0.93, 0.83, 0.85),
    lci = c(0.53, 0.92, 0.50, 0.40, 0.46), 
    uci = c(1.47, 1.83, 1.36, 1.27, 1.25), 
    se = c(0.242, 0.234, 0.219, 0.220, 0.201),
    events = c(20, 21, 21, 22, 43),
    median = c(3.32, 5.74, 8.06, 10.74, 15.81),
    hr_cont = c(0.98, NA, NA, NA, NA),
    lci_cont = c(0.82, NA, NA, NA, NA),
    uci_cont = c(1.17, NA, NA, NA, NA),
    p_trend = c("0.797"),
    unit = c("per 5 mm Hg", NA, NA, NA, NA)
  ) %>% 
  process_data(hr, hr_cont, lci_cont, uci_cont)


# Create plots ------------------------------------------------------------

# Plot for all-cause mortality
dth_plot = 
  plot_ordered_sd_plot(
    .data = dth, .panel_label = "A", .outcome_label = "All-cause mortality",
    .main_colour = off_black, .square_colour = ocean_blue
  )

# Plot for hospitalisation for heart failure
hhf_plot = 
  plot_ordered_sd_plot(
    .data = hhf, .panel_label = "B", 
    .outcome_label = "Hospitalisation for heart failure",
    .main_colour = off_black, .square_colour = ocean_blue
  )

# Plot for kidney failure
eskd_plot = 
  plot_ordered_sd_plot(
    .data = eskd, .panel_label = "C", .outcome_label = "Kidney failure",
    .outcome_type = "kidney", .eskd = TRUE, .main_colour = off_black, 
    .square_colour = ocean_blue
  )