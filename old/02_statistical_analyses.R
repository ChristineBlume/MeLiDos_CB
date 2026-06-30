# ============================================================
# STATISTICAL ANALYSIS: Sleep outcomes and environmental exposure
# ============================================================
# PURPOSE:
# Fit GAMs aligned with the uploaded Light_climate.html model,
# but using sleep outcomes instead of light exposure.
#
# IMPORTANT:
# - no within-/between-person decomposition
# - no age or gender covariates
#
# The original Analytical model used:
# lzMEDI ~ s(Time, Id, bs = "fs", xt = list(bs = "cc")) +
#          s(Id_date, bs = "re") +
#          te(Temperature, Irradiance) +
#          s(Precipitation)
#
# Since the current dataset is person-night-level rather than hourly,
# there is no time-of-day smooth. The analogous person-night model uses:
# outcome ~ te(nocturnal_temperature, daytime_light) +
#           s(Id, bs = "re")
# ============================================================


# ------------------------------------------------------------
# STEP 1: Load packages
# ------------------------------------------------------------
# PURPOSE:
# Load packages needed for GAM modelling and tidy summaries.
# ------------------------------------------------------------
setwd("C:/Users/chris/OneDrive/Documents/GitHub/MeLiDos_CB")


packages_needed <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "tibble",
  "stringr",
  "mgcv",
  "gratia",
  "ggplot2"
)

packages_missing <- packages_needed[
  !packages_needed %in% rownames(installed.packages())
]

if (length(packages_missing) > 0) {
  install.packages(packages_missing)
}

invisible(
  lapply(
    packages_needed,
    library,
    character.only = TRUE
  )
)

analysis_results_dir <- "output/statistical_analysis"

dir.create(
  analysis_results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# STEP 2: Load prepared person-night dataset
# ------------------------------------------------------------
# PURPOSE:
# Use the analysis dataset created during preprocessing.
# ------------------------------------------------------------

analysis_sleep <- readRDS(
  "data/processed/analysis_sleep_ucr_person_night.rds"
)


# ------------------------------------------------------------
# STEP 3: Check required variables
# ------------------------------------------------------------
# PURPOSE:
# Make sure the required sleep outcomes and exposure variables exist.
#
# NOTE:
# For the HTML-aligned model, the closest available predictors are:
# - temperature_mean_night: nocturnal temperature
# - medi_mean_day: daytime personal melanopic EDI
#
# twb_mean_night can be used as a humidity-integrated sensitivity
# exposure, but the primary HTML-aligned model should use temperature.
# ------------------------------------------------------------

required_analysis_variables <- c(
  "Id",
  "site",
  "sleep_date",
  "sleep_efficiency_diary",
  "sleepquality",
  "pim_interdaily_stability",
  "temperature_mean_night",
  "twb_mean_night",
  "medi_mean_day"
)

analysis_variable_check <- tibble::tibble(
  variable =
    required_analysis_variables,
  
  present =
    variable %in% names(analysis_sleep)
)

print(
  analysis_variable_check,
  width = Inf
)

if (any(!analysis_variable_check$present)) {
  stop(
    "The following required variables are missing from analysis_sleep: ",
    paste(
      analysis_variable_check$variable[
        !analysis_variable_check$present
      ],
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# STEP 4: Prepare person-night modelling dataset
# ------------------------------------------------------------
# PURPOSE:
# Create a clean person-night dataset for sleep efficiency and
# subjective sleep quality models.
#
# IMPORTANT:
# No lagged variables are created.
# ------------------------------------------------------------

analysis_sleep_gam <- analysis_sleep %>%
  dplyr::filter(
    site == "UCR"
  ) %>%
  dplyr::mutate(
    Id =
      factor(
        Id
      ),
    
    sleep_date =
      as.Date(
        sleep_date
      ),
    
    sleep_efficiency_diary =
      as.numeric(
        sleep_efficiency_diary
      ),
    
    sleepquality =
      as.numeric(
        sleepquality
      ),
    
    pim_interdaily_stability =
      as.numeric(
        pim_interdaily_stability
      ),
    
    temperature_mean_night =
      as.numeric(
        temperature_mean_night
      ),
    
    twb_mean_night =
      as.numeric(
        twb_mean_night
      ),
    
    medi_mean_day =
      as.numeric(
        medi_mean_day
      ),
    
    # Log-transform mEDI as in the HTML model logic.
    # The HTML used log10(melanopic EDI + 0.1).
    lmedi_mean_day =
      log10(
        medi_mean_day + 0.1
      )
  )

analysis_sleep_gam_check <- analysis_sleep_gam %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),
    
    n_ids =
      dplyr::n_distinct(
        Id
      ),
    
    n_with_sleep_efficiency =
      sum(
        is.finite(
          sleep_efficiency_diary
        )
      ),
    
    n_with_sleepquality =
      sum(
        is.finite(
          sleepquality
        )
      ),
    
    n_with_interdaily_stability =
      sum(
        is.finite(
          pim_interdaily_stability
        )
      ),
    
    n_with_temperature =
      sum(
        is.finite(
          temperature_mean_night
        )
      ),
    
    n_with_twb =
      sum(
        is.finite(
          twb_mean_night
        )
      ),
    
    n_with_lmedi_day =
      sum(
        is.finite(
          lmedi_mean_day
        )
      )
  )

print(
  analysis_sleep_gam_check,
  width = Inf
)


# ------------------------------------------------------------
# STEP 5: Define outcome-specific complete-case datasets
# ------------------------------------------------------------
# PURPOSE:
# Prepare one dataset per person-night outcome.
#
# OUTCOMES:
# 1. Diary-based sleep efficiency
# 2. Subjective sleep quality
#
# NOTE:
# Interdaily stability is handled separately below because it is
# probably participant-level and repeated across nights after joining.
# ------------------------------------------------------------

sleep_efficiency_model_data <- analysis_sleep_gam %>%
  dplyr::filter(
    is.finite(
      sleep_efficiency_diary
    ),
    is.finite(
      temperature_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

sleepquality_model_data <- analysis_sleep_gam %>%
  dplyr::filter(
    is.finite(
      sleepquality
    ),
    is.finite(
      temperature_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

model_sample_check <- tibble::tibble(
  model =
    c(
      "sleep_efficiency_diary",
      "sleepquality"
    ),
  
  n_rows =
    c(
      nrow(
        sleep_efficiency_model_data
      ),
      nrow(
        sleepquality_model_data
      )
    ),
  
  n_ids =
    c(
      dplyr::n_distinct(
        sleep_efficiency_model_data$Id
      ),
      dplyr::n_distinct(
        sleepquality_model_data$Id
      )
    )
)

print(
  model_sample_check,
  width = Inf
)

# ------------------------------------------------------------
# STEP 6: Fit HTML-aligned GAMs for person-night outcomes
# ------------------------------------------------------------
# PURPOSE:
# Fit GAMs analogous to the HTML model, but with sleep outcomes.
#
# MODEL STRUCTURE:
# outcome ~ te(Temperature, lMEDI) + s(Id, bs = "re")
#
# WHY NO s(Time, Id)?
# The current data are person-night-level, not hourly.
# Therefore there is no within-day time-of-day variable.
#
# WHY NO s(Id_date)?
# There is one row per participant-night, so an Id-date random effect
# would absorb the observation-level residual and is not appropriate
# here.
# ------------------------------------------------------------

m_sleep_efficiency <- mgcv::bam(
  formula =
    sleep_efficiency_diary ~
    te(
      temperature_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleep_efficiency_model_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

m_sleepquality <- mgcv::bam(
  formula =
    sleepquality ~
    te(
      temperature_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleepquality_model_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

# ------------------------------------------------------------
# STEP 6b: Create effect-surface plots
# ------------------------------------------------------------
# PURPOSE:
# Visualise the fitted tensor-product smooth:
# temperature or wet-bulb temperature × daytime lMEDI.
#
# INTERPRETATION:
# The plot shows the population-level fitted surface.
# The participant random effect s(Id) is excluded from prediction.
# ------------------------------------------------------------

create_effect_surface_plot <- function(
    model,
    data,
    x_variable,
    y_variable,
    outcome_label,
    x_label,
    y_label,
    file_stub
) {
  
  # Define a prediction grid within the observed exposure range.
  x_limits <- stats::quantile(
    data[[x_variable]],
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  y_limits <- stats::quantile(
    data[[y_variable]],
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  prediction_grid <- tidyr::expand_grid(
    x_value =
      seq(
        from =
          x_limits[[1]],
        to =
          x_limits[[2]],
        length.out =
          80
      ),
    
    y_value =
      seq(
        from =
          y_limits[[1]],
        to =
          y_limits[[2]],
        length.out =
          80
      )
  )
  
  # Store the grid values under the original variable names.
  prediction_grid[[x_variable]] <- prediction_grid$x_value
  prediction_grid[[y_variable]] <- prediction_grid$y_value
  
  # Add a valid participant factor level.
  # The participant random effect is excluded from prediction below.
  prediction_grid <- prediction_grid %>%
    dplyr::mutate(
      Id =
        factor(
          levels(
            data$Id
          )[[1]],
          levels =
            levels(
              data$Id
            )
        )
    ) %>%
    dplyr::select(
      -x_value,
      -y_value
    )
  
  # Predict the population-level surface, excluding s(Id).
  predictions <- stats::predict(
    model,
    newdata =
      prediction_grid,
    type =
      "response",
    se.fit =
      TRUE,
    exclude =
      "s(Id)"
  )
  
  prediction_surface <- prediction_grid %>%
    dplyr::mutate(
      fit =
        as.numeric(
          predictions$fit
        ),
      
      se_fit =
        as.numeric(
          predictions$se.fit
        ),
      
      fit_lower_approx =
        fit - 1.96 * se_fit,
      
      fit_upper_approx =
        fit + 1.96 * se_fit
    )
  
  # Save the prediction surface as CSV.
  readr::write_csv(
    prediction_surface,
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_effect_surface.csv"
      )
    )
  )
  
  # Create the effect-surface plot.
  effect_surface_plot <- ggplot2::ggplot(
    prediction_surface,
    ggplot2::aes(
      x =
        .data[[x_variable]],
      y =
        .data[[y_variable]],
      fill =
        fit
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_contour(
      ggplot2::aes(
        z =
          fit
      )
    ) +
    ggplot2::labs(
      title =
        paste0(
          outcome_label,
          ": fitted exposure surface"
        ),
      
      x =
        x_label,
      
      y =
        y_label,
      
      fill =
        "Predicted outcome"
    ) +
    ggplot2::theme_minimal()
  
  # Save the plot.
  ggplot2::ggsave(
    filename =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_effect_surface.png"
        )
      ),
    
    plot =
      effect_surface_plot,
    
    width =
      7,
    
    height =
      5,
    
    dpi =
      300
  )
  
  output <- list(
    prediction_surface =
      prediction_surface,
    
    effect_surface_plot =
      effect_surface_plot
  )
  
  rm(
    x_limits,
    y_limits,
    prediction_grid,
    predictions
  )
  
  return(
    output
  )
}


# ------------------------------------------------------------
# STEP 7: Save model summaries
# ------------------------------------------------------------
# PURPOSE:
# Save model objects and readable summaries.
# ------------------------------------------------------------

saveRDS(
  m_sleep_efficiency,
  file.path(
    analysis_results_dir,
    "m_sleep_efficiency_temperature_lmedi_gam.rds"
  )
)

saveRDS(
  m_sleepquality,
  file.path(
    analysis_results_dir,
    "m_sleepquality_temperature_lmedi_gam.rds"
  )
)

sink(
  file.path(
    analysis_results_dir,
    "m_sleep_efficiency_summary.txt"
  )
)

print(
  summary(
    m_sleep_efficiency
  )
)

sink()

sink(
  file.path(
    analysis_results_dir,
    "m_sleepquality_summary.txt"
  )
)

print(
  summary(
    m_sleepquality
  )
)

sink()


# ------------------------------------------------------------
# STEP 8: Check concurvity
# ------------------------------------------------------------
# PURPOSE:
# Mirror the HTML workflow by checking concurvity among smooth terms.
# ------------------------------------------------------------

concurvity_sleep_efficiency <- mgcv::concurvity(
  m_sleep_efficiency,
  full = TRUE
)

concurvity_sleepquality <- mgcv::concurvity(
  m_sleepquality,
  full = TRUE
)

capture.output(
  concurvity_sleep_efficiency,
  file =
    file.path(
      analysis_results_dir,
      "concurvity_sleep_efficiency.txt"
    )
)

capture.output(
  concurvity_sleepquality,
  file =
    file.path(
      analysis_results_dir,
      "concurvity_sleepquality.txt"
    )
)


# ------------------------------------------------------------
# STEP 9: Model diagnostics
# ------------------------------------------------------------
# PURPOSE:
# Inspect residuals and basic GAM diagnostics.
# ------------------------------------------------------------

png(
  filename =
    file.path(
      analysis_results_dir,
      "diagnostics_sleep_efficiency.png"
    ),
  width = 1200,
  height = 900,
  res = 150
)

mgcv::gam.check(
  m_sleep_efficiency
)

dev.off()

png(
  filename =
    file.path(
      analysis_results_dir,
      "diagnostics_sleepquality.png"
    ),
  width = 1200,
  height = 900,
  res = 150
)

mgcv::gam.check(
  m_sleepquality
)

dev.off()


# ------------------------------------------------------------
# STEP 10: Smooth-term summaries
# ------------------------------------------------------------
# PURPOSE:
# Extract smooth summaries for reporting.
# ------------------------------------------------------------

smooth_summary_sleep_efficiency <- as.data.frame(
  summary(
    m_sleep_efficiency
  )$s.table
) %>%
  tibble::rownames_to_column(
    "smooth_term"
  ) %>%
  dplyr::mutate(
    model =
      "sleep_efficiency_diary",
    .before =
      1
  )

smooth_summary_sleepquality <- as.data.frame(
  summary(
    m_sleepquality
  )$s.table
) %>%
  tibble::rownames_to_column(
    "smooth_term"
  ) %>%
  dplyr::mutate(
    model =
      "sleepquality",
    .before =
      1
  )

smooth_summary_all <- dplyr::bind_rows(
  smooth_summary_sleep_efficiency,
  smooth_summary_sleepquality
)

readr::write_csv(
  smooth_summary_all,
  file.path(
    analysis_results_dir,
    "smooth_summary_sleep_outcomes.csv"
  )
)

smooth_summary_all

# ------------------------------------------------------------
# Create rounded smooth-summary table for reporting
# ------------------------------------------------------------

smooth_summary_reporting <- smooth_summary_all %>%
  dplyr::mutate(
    edf =
      round(
        edf,
        2
      ),
    
    Ref.df =
      round(
        Ref.df,
        2
      ),
    
    F =
      round(
        F,
        2
      ),
    
    p_value =
      dplyr::case_when(
        `p-value` < 0.001 ~
          "< .001",
        
        TRUE ~
          paste0(
            "= ",
            formatC(
              `p-value`,
              format = "f",
              digits = 3
            )
          )
      )
  ) %>%
  dplyr::select(
    model,
    smooth_term,
    edf,
    Ref.df,
    F,
    p_value
  )

smooth_summary_reporting

readr::write_csv(
  smooth_summary_reporting,
  file.path(
    analysis_results_dir,
    "smooth_summary_reporting.csv"
  )
)

# ------------------------------------------------------------
# STEP 11: Sensitivity models with wet-bulb temperature
# ------------------------------------------------------------
# PURPOSE:
# Repeat the same HTML-aligned model structure using wet-bulb
# temperature instead of air temperature.
#
# RATIONALE:
# The HTML model used Temperature. Wet-bulb temperature is added here
# as a humidity-integrated sensitivity exposure.
#
# MODEL STRUCTURE:
# outcome ~ te(wet-bulb temperature, daytime lMEDI) + s(Id, bs = "re")
#
# IMPORTANT:
# This is not a lagged analysis.
# This step creates:
# - wet-bulb model sample checks
# - model summaries
# - smooth-term summaries
# - model-fit summaries
# - k checks
# - concurvity checks
# - variance contribution checks
# - diagnostic plots
# - prediction surfaces
# ------------------------------------------------------------


# ------------------------------------------------------------
# STEP 11a: Prepare wet-bulb complete-case datasets
# ------------------------------------------------------------
# PURPOSE:
# Avoid silent row deletion inside mgcv::bam() by explicitly defining
# the complete-case sample for the wet-bulb sensitivity models.
# ------------------------------------------------------------

sleep_efficiency_twb_model_data <- sleep_efficiency_model_data %>%
  dplyr::filter(
    is.finite(
      sleep_efficiency_diary
    ),
    is.finite(
      twb_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

sleepquality_twb_model_data <- sleepquality_model_data %>%
  dplyr::filter(
    is.finite(
      sleepquality
    ),
    is.finite(
      twb_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

twb_model_sample_check <- tibble::tibble(
  model =
    c(
      "sleep_efficiency_diary_twb",
      "sleepquality_twb"
    ),
  
  n_rows =
    c(
      nrow(
        sleep_efficiency_twb_model_data
      ),
      nrow(
        sleepquality_twb_model_data
      )
    ),
  
  n_ids =
    c(
      dplyr::n_distinct(
        sleep_efficiency_twb_model_data$Id
      ),
      dplyr::n_distinct(
        sleepquality_twb_model_data$Id
      )
    ),
  
  min_twb_mean_night =
    c(
      min(
        sleep_efficiency_twb_model_data$twb_mean_night,
        na.rm = TRUE
      ),
      min(
        sleepquality_twb_model_data$twb_mean_night,
        na.rm = TRUE
      )
    ),
  
  median_twb_mean_night =
    c(
      median(
        sleep_efficiency_twb_model_data$twb_mean_night,
        na.rm = TRUE
      ),
      median(
        sleepquality_twb_model_data$twb_mean_night,
        na.rm = TRUE
      )
    ),
  
  max_twb_mean_night =
    c(
      max(
        sleep_efficiency_twb_model_data$twb_mean_night,
        na.rm = TRUE
      ),
      max(
        sleepquality_twb_model_data$twb_mean_night,
        na.rm = TRUE
      )
    )
)

print(
  twb_model_sample_check,
  width = Inf
)

readr::write_csv(
  twb_model_sample_check,
  file.path(
    analysis_results_dir,
    "twb_model_sample_check.csv"
  )
)


# ------------------------------------------------------------
# STEP 11b: Fit wet-bulb sensitivity GAMs
# ------------------------------------------------------------
# PURPOSE:
# Fit the HTML-aligned model structure using wet-bulb temperature.
# ------------------------------------------------------------

m_sleep_efficiency_twb <- mgcv::bam(
  formula =
    sleep_efficiency_diary ~
    te(
      twb_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleep_efficiency_twb_model_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

m_sleepquality_twb <- mgcv::bam(
  formula =
    sleepquality ~
    te(
      twb_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleepquality_twb_model_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

saveRDS(
  m_sleep_efficiency_twb,
  file.path(
    analysis_results_dir,
    "m_sleep_efficiency_twb_lmedi_gam.rds"
  )
)

saveRDS(
  m_sleepquality_twb,
  file.path(
    analysis_results_dir,
    "m_sleepquality_twb_lmedi_gam.rds"
  )
)


# ------------------------------------------------------------
# STEP 11c: Define helper functions for model investigations
# ------------------------------------------------------------
# PURPOSE:
# Avoid repeating the same summary, diagnostic and variance-
# contribution code for both wet-bulb models.
# ------------------------------------------------------------

extract_gam_smooth_summary <- function(
    model,
    model_label,
    exposure_label
) {
  
  smooth_table <- as.data.frame(
    summary(
      model
    )$s.table
  ) %>%
    tibble::rownames_to_column(
      "smooth_term"
    ) %>%
    dplyr::mutate(
      model =
        model_label,
      
      exposure =
        exposure_label,
      
      .before =
        1
    )
  
  return(
    smooth_table
  )
}


extract_gam_fit_summary <- function(
    model,
    model_label,
    exposure_label
) {
  
  model_summary <- summary(
    model
  )
  
  fit_summary <- tibble::tibble(
    model =
      model_label,
    
    exposure =
      exposure_label,
    
    n =
      stats::nobs(
        model
      ),
    
    adjusted_r_squared =
      unname(
        model_summary$r.sq
      ),
    
    deviance_explained =
      unname(
        model_summary$dev.expl
      ),
    
    scale_estimate =
      unname(
        model_summary$scale
      ),
    
    fREML_score =
      unname(
        model$gcv.ubre
      )
  )
  
  return(
    fit_summary
  )
}


extract_gam_k_check <- function(
    model,
    model_label,
    exposure_label
) {
  
  k_check <- as.data.frame(
    mgcv::k.check(
      model
    )
  ) %>%
    tibble::rownames_to_column(
      "smooth_term"
    ) %>%
    dplyr::mutate(
      model =
        model_label,
      
      exposure =
        exposure_label,
      
      .before =
        1
    )
  
  return(
    k_check
  )
}


save_gam_summary_text <- function(
    model,
    file_stub
) {
  
  sink(
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_summary.txt"
      )
    )
  )
  
  print(
    summary(
      model
    )
  )
  
  sink()
  
  invisible(
    NULL
  )
}


save_gam_diagnostics <- function(
    model,
    file_stub
) {
  
  # Save textual gam.check() output.
  capture.output(
    mgcv::gam.check(
      model
    ),
    file =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_gam_check.txt"
        )
      )
  )
  
  # Save graphical gam.check() output.
  png(
    filename =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_gam_check.png"
        )
      ),
    width = 1200,
    height = 900,
    res = 150
  )
  
  mgcv::gam.check(
    model
  )
  
  dev.off()
  
  invisible(
    NULL
  )
}


save_gam_concurvity <- function(
    model,
    file_stub
) {
  
  concurvity_current <- mgcv::concurvity(
    model,
    full = TRUE
  )
  
  capture.output(
    concurvity_current,
    file =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_concurvity.txt"
        )
      )
  )
  
  invisible(
    concurvity_current
  )
}


extract_term_variance_contribution <- function(
    model,
    model_label,
    exposure_label,
    file_stub
) {
  
  # Extract partial model terms, as in the HTML workflow.
  term_matrix <- stats::predict(
    model,
    type = "terms"
  )
  
  term_variance <- apply(
    term_matrix,
    2,
    stats::var,
    na.rm = TRUE
  )
  
  term_variance_table <- tibble::enframe(
    term_variance,
    name = "term",
    value = "variance"
  ) %>%
    dplyr::mutate(
      model =
        model_label,
      
      exposure =
        exposure_label,
      
      variance_contribution =
        variance / sum(
          variance,
          na.rm = TRUE
        ),
      
      variance_contribution_percent =
        100 * variance_contribution,
      
      .before =
        1
    )
  
  term_correlation_table <- as.data.frame(
    as.table(
      stats::cor(
        term_matrix,
        use = "pairwise.complete.obs"
      )
    )
  ) %>%
    dplyr::rename(
      term_1 =
        Var1,
      
      term_2 =
        Var2,
      
      correlation =
        Freq
    ) %>%
    dplyr::mutate(
      model =
        model_label,
      
      exposure =
        exposure_label,
      
      .before =
        1
    )
  
  readr::write_csv(
    term_variance_table,
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_term_variance_contribution.csv"
      )
    )
  )
  
  readr::write_csv(
    term_correlation_table,
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_term_correlation.csv"
      )
    )
  )
  
  output <- list(
    term_variance_table =
      term_variance_table,
    
    term_correlation_table =
      term_correlation_table
  )
  
  rm(
    term_matrix,
    term_variance
  )
  
  return(
    output
  )
}


create_tensor_prediction_surface <- function(
    model,
    data,
    x_variable,
    y_variable,
    model_label,
    exposure_label,
    outcome_label,
    file_stub
) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  
  x_limits <- stats::quantile(
    data[[x_variable]],
    probs = c(
      0.02,
      0.98
    ),
    na.rm = TRUE
  )
  
  y_limits <- stats::quantile(
    data[[y_variable]],
    probs = c(
      0.02,
      0.98
    ),
    na.rm = TRUE
  )
  
  prediction_grid <- tidyr::expand_grid(
    x_value =
      seq(
        from =
          x_limits[[1]],
        to =
          x_limits[[2]],
        length.out =
          80
      ),
    
    y_value =
      seq(
        from =
          y_limits[[1]],
        to =
          y_limits[[2]],
        length.out =
          80
      )
  )
  
  prediction_grid[[x_variable]] <- prediction_grid$x_value
  prediction_grid[[y_variable]] <- prediction_grid$y_value
  
  prediction_grid <- prediction_grid %>%
    dplyr::mutate(
      Id =
        factor(
          data$Id[[1]],
          levels =
            levels(
              data$Id
            )
        )
    ) %>%
    dplyr::select(
      -x_value,
      -y_value
    )
  
  predictions <- stats::predict(
    model,
    newdata =
      prediction_grid,
    type =
      "response",
    se.fit =
      TRUE,
    exclude =
      "s(Id)"
  )
  
  prediction_surface <- prediction_grid %>%
    dplyr::mutate(
      model =
        model_label,
      
      exposure =
        exposure_label,
      
      fit =
        as.numeric(
          predictions$fit
        ),
      
      se_fit =
        as.numeric(
          predictions$se.fit
        ),
      
      fit_lower_approx =
        fit - 1.96 * se_fit,
      
      fit_upper_approx =
        fit + 1.96 * se_fit
    )
  
  readr::write_csv(
    prediction_surface,
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_prediction_surface.csv"
      )
    )
  )
  
  prediction_plot <- ggplot2::ggplot(
    prediction_surface,
    ggplot2::aes(
      x =
        .data[[x_variable]],
      y =
        .data[[y_variable]],
      fill =
        fit
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_contour(
      ggplot2::aes(
        z =
          fit
      )
    ) +
    ggplot2::labs(
      title =
        paste0(
          outcome_label,
          ": wet-bulb temperature × daytime lMEDI"
        ),
      
      x =
        "Mean nocturnal wet-bulb temperature",
      
      y =
        "Daytime log10(mEDI + 0.1)",
      
      fill =
        "Predicted outcome"
    ) +
    ggplot2::theme_minimal()
  
  ggplot2::ggsave(
    filename =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_prediction_surface.png"
        )
      ),
    plot =
      prediction_plot,
    width =
      7,
    height =
      5,
    dpi =
      300
  )
  
  rm(
    x_limits,
    y_limits,
    predictions,
    prediction_plot
  )
  
  return(
    prediction_surface
  )
}


# ------------------------------------------------------------
# STEP 11d: Save summaries, diagnostics and checks
# ------------------------------------------------------------
# PURPOSE:
# Apply all investigation functions to the two wet-bulb models.
# ------------------------------------------------------------

twb_smooth_summary_all <- dplyr::bind_rows(
  extract_gam_smooth_summary(
    model =
      m_sleep_efficiency_twb,
    model_label =
      "sleep_efficiency_diary",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  ),
  
  extract_gam_smooth_summary(
    model =
      m_sleepquality_twb,
    model_label =
      "sleepquality",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  )
)

twb_model_fit_summary_all <- dplyr::bind_rows(
  extract_gam_fit_summary(
    model =
      m_sleep_efficiency_twb,
    model_label =
      "sleep_efficiency_diary",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  ),
  
  extract_gam_fit_summary(
    model =
      m_sleepquality_twb,
    model_label =
      "sleepquality",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  )
)

twb_k_check_all <- dplyr::bind_rows(
  extract_gam_k_check(
    model =
      m_sleep_efficiency_twb,
    model_label =
      "sleep_efficiency_diary",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  ),
  
  extract_gam_k_check(
    model =
      m_sleepquality_twb,
    model_label =
      "sleepquality",
    exposure_label =
      "twb_mean_night_x_lmedi_mean_day"
  )
)

readr::write_csv(
  twb_smooth_summary_all,
  file.path(
    analysis_results_dir,
    "twb_smooth_summary_all.csv"
  )
)

readr::write_csv(
  twb_model_fit_summary_all,
  file.path(
    analysis_results_dir,
    "twb_model_fit_summary_all.csv"
  )
)

readr::write_csv(
  twb_k_check_all,
  file.path(
    analysis_results_dir,
    "twb_k_check_all.csv"
  )
)

save_gam_summary_text(
  model =
    m_sleep_efficiency_twb,
  file_stub =
    "m_sleep_efficiency_twb"
)

save_gam_summary_text(
  model =
    m_sleepquality_twb,
  file_stub =
    "m_sleepquality_twb"
)

save_gam_diagnostics(
  model =
    m_sleep_efficiency_twb,
  file_stub =
    "m_sleep_efficiency_twb"
)

save_gam_diagnostics(
  model =
    m_sleepquality_twb,
  file_stub =
    "m_sleepquality_twb"
)

concurvity_sleep_efficiency_twb <- save_gam_concurvity(
  model =
    m_sleep_efficiency_twb,
  file_stub =
    "m_sleep_efficiency_twb"
)

concurvity_sleepquality_twb <- save_gam_concurvity(
  model =
    m_sleepquality_twb,
  file_stub =
    "m_sleepquality_twb"
)


# ------------------------------------------------------------
# STEP 11e: Variance contribution of partial model terms
# ------------------------------------------------------------
# PURPOSE:
# Mirror the HTML investigation using:
# predict(model, type = "terms")
# followed by variance decomposition of the partial effects.
# ------------------------------------------------------------

term_variance_sleep_efficiency_twb <- extract_term_variance_contribution(
  model =
    m_sleep_efficiency_twb,
  model_label =
    "sleep_efficiency_diary",
  exposure_label =
    "twb_mean_night_x_lmedi_mean_day",
  file_stub =
    "m_sleep_efficiency_twb"
)

term_variance_sleepquality_twb <- extract_term_variance_contribution(
  model =
    m_sleepquality_twb,
  model_label =
    "sleepquality",
  exposure_label =
    "twb_mean_night_x_lmedi_mean_day",
  file_stub =
    "m_sleepquality_twb"
)

twb_term_variance_contribution_all <- dplyr::bind_rows(
  term_variance_sleep_efficiency_twb$term_variance_table,
  term_variance_sleepquality_twb$term_variance_table
)

readr::write_csv(
  twb_term_variance_contribution_all,
  file.path(
    analysis_results_dir,
    "twb_term_variance_contribution_all.csv"
  )
)


# ------------------------------------------------------------
# STEP 11f: Prediction surfaces
# ------------------------------------------------------------
# PURPOSE:
# Visualise the fitted tensor-product smooth while excluding the
# participant random effect s(Id).
#
# INTERPRETATION:
# These plots show the population-level fitted surface for the
# wet-bulb temperature × daytime lMEDI term.
# ------------------------------------------------------------

prediction_surface_sleep_efficiency_twb <- create_tensor_prediction_surface(
  model =
    m_sleep_efficiency_twb,
  data =
    sleep_efficiency_twb_model_data,
  x_variable =
    "twb_mean_night",
  y_variable =
    "lmedi_mean_day",
  model_label =
    "sleep_efficiency_diary",
  exposure_label =
    "twb_mean_night_x_lmedi_mean_day",
  outcome_label =
    "Diary sleep efficiency",
  file_stub =
    "m_sleep_efficiency_twb"
)

prediction_surface_sleepquality_twb <- create_tensor_prediction_surface(
  model =
    m_sleepquality_twb,
  data =
    sleepquality_twb_model_data,
  x_variable =
    "twb_mean_night",
  y_variable =
    "lmedi_mean_day",
  model_label =
    "sleepquality",
  exposure_label =
    "twb_mean_night_x_lmedi_mean_day",
  outcome_label =
    "Subjective sleep quality",
  file_stub =
    "m_sleepquality_twb"
)


# ------------------------------------------------------------
# STEP 11g: Optional matched comparison of air temperature vs wet-bulb
# ------------------------------------------------------------
# PURPOSE:
# Compare air-temperature and wet-bulb models on exactly the same
# complete-case rows.
#
# IMPORTANT:
# This is descriptive. The models are not nested.
# It is useful only to check whether the humidity-integrated exposure
# gives clearly better or worse fit than air temperature.
# ------------------------------------------------------------

sleep_efficiency_matched_temp_twb_data <- analysis_sleep_gam %>%
  dplyr::filter(
    is.finite(
      sleep_efficiency_diary
    ),
    is.finite(
      temperature_mean_night
    ),
    is.finite(
      twb_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

sleepquality_matched_temp_twb_data <- analysis_sleep_gam %>%
  dplyr::filter(
    is.finite(
      sleepquality
    ),
    is.finite(
      temperature_mean_night
    ),
    is.finite(
      twb_mean_night
    ),
    is.finite(
      lmedi_mean_day
    ),
    !is.na(
      Id
    )
  )

m_sleep_efficiency_air_matched <- mgcv::bam(
  formula =
    sleep_efficiency_diary ~
    te(
      temperature_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleep_efficiency_matched_temp_twb_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

m_sleep_efficiency_twb_matched <- mgcv::bam(
  formula =
    sleep_efficiency_diary ~
    te(
      twb_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleep_efficiency_matched_temp_twb_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

m_sleepquality_air_matched <- mgcv::bam(
  formula =
    sleepquality ~
    te(
      temperature_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleepquality_matched_temp_twb_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

m_sleepquality_twb_matched <- mgcv::bam(
  formula =
    sleepquality ~
    te(
      twb_mean_night,
      lmedi_mean_day
    ) +
    s(
      Id,
      bs = "re"
    ),
  
  data =
    sleepquality_matched_temp_twb_data,
  
  family =
    gaussian(),
  
  method =
    "fREML",
  
  discrete =
    TRUE
)

matched_model_fit_summary <- dplyr::bind_rows(
  extract_gam_fit_summary(
    m_sleep_efficiency_air_matched,
    "sleep_efficiency_diary",
    "temperature_mean_night_x_lmedi_mean_day_matched"
  ),
  extract_gam_fit_summary(
    m_sleep_efficiency_twb_matched,
    "sleep_efficiency_diary",
    "twb_mean_night_x_lmedi_mean_day_matched"
  ),
  extract_gam_fit_summary(
    m_sleepquality_air_matched,
    "sleepquality",
    "temperature_mean_night_x_lmedi_mean_day_matched"
  ),
  extract_gam_fit_summary(
    m_sleepquality_twb_matched,
    "sleepquality",
    "twb_mean_night_x_lmedi_mean_day_matched"
  )
)

matched_model_aic <- dplyr::bind_rows(
  tibble::tibble(
    model =
      "sleep_efficiency_diary",
    
    exposure =
      c(
        "temperature_mean_night_x_lmedi_mean_day_matched",
        "twb_mean_night_x_lmedi_mean_day_matched"
      ),
    
    AIC =
      stats::AIC(
        m_sleep_efficiency_air_matched,
        m_sleep_efficiency_twb_matched
      )$AIC
  ),
  
  tibble::tibble(
    model =
      "sleepquality",
    
    exposure =
      c(
        "temperature_mean_night_x_lmedi_mean_day_matched",
        "twb_mean_night_x_lmedi_mean_day_matched"
      ),
    
    AIC =
      stats::AIC(
        m_sleepquality_air_matched,
        m_sleepquality_twb_matched
      )$AIC
  )
) %>%
  dplyr::group_by(
    model
  ) %>%
  dplyr::mutate(
    delta_AIC =
      AIC - min(
        AIC,
        na.rm = TRUE
      )
  ) %>%
  dplyr::ungroup()

readr::write_csv(
  matched_model_fit_summary,
  file.path(
    analysis_results_dir,
    "matched_temperature_vs_twb_model_fit_summary.csv"
  )
)

readr::write_csv(
  matched_model_aic,
  file.path(
    analysis_results_dir,
    "matched_temperature_vs_twb_aic.csv"
  )
)

saveRDS(
  m_sleep_efficiency_air_matched,
  file.path(
    analysis_results_dir,
    "m_sleep_efficiency_air_matched.rds"
  )
)

saveRDS(
  m_sleep_efficiency_twb_matched,
  file.path(
    analysis_results_dir,
    "m_sleep_efficiency_twb_matched.rds"
  )
)

saveRDS(
  m_sleepquality_air_matched,
  file.path(
    analysis_results_dir,
    "m_sleepquality_air_matched.rds"
  )
)

saveRDS(
  m_sleepquality_twb_matched,
  file.path(
    analysis_results_dir,
    "m_sleepquality_twb_matched.rds"
  )
)


# ------------------------------------------------------------
# STEP 11h: Inspect key outputs
# ------------------------------------------------------------

twb_smooth_summary_all

twb_model_fit_summary_all

twb_k_check_all

twb_term_variance_contribution_all

matched_model_aic


# ------------------------------------------------------------
# STEP 11i: Clean up temporary helper objects
# ------------------------------------------------------------

rm(
  term_variance_sleep_efficiency_twb,
  term_variance_sleepquality_twb
)

# ------------------------------------------------------------
# STEP 11j: Create effect-surface plots
# ------------------------------------------------------------
# PURPOSE:
# Visualise the fitted tensor-product smooth:
# temperature or wet-bulb temperature × daytime lMEDI.
#
# INTERPRETATION:
# The plot shows the population-level fitted surface.
# The participant random effect s(Id) is excluded from prediction.
# ------------------------------------------------------------

create_effect_surface_plot <- function(
    model,
    data,
    x_variable,
    y_variable,
    outcome_label,
    x_label,
    y_label,
    file_stub
) {
  
  # Define a prediction grid within the observed exposure range.
  x_limits <- stats::quantile(
    data[[x_variable]],
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  y_limits <- stats::quantile(
    data[[y_variable]],
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  prediction_grid <- tidyr::expand_grid(
    x_value =
      seq(
        from =
          x_limits[[1]],
        to =
          x_limits[[2]],
        length.out =
          80
      ),
    
    y_value =
      seq(
        from =
          y_limits[[1]],
        to =
          y_limits[[2]],
        length.out =
          80
      )
  )
  
  # Store the grid values under the original variable names.
  prediction_grid[[x_variable]] <- prediction_grid$x_value
  prediction_grid[[y_variable]] <- prediction_grid$y_value
  
  # Add a valid participant factor level.
  # The participant random effect is excluded from prediction below.
  prediction_grid <- prediction_grid %>%
    dplyr::mutate(
      Id =
        factor(
          levels(
            data$Id
          )[[1]],
          levels =
            levels(
              data$Id
            )
        )
    ) %>%
    dplyr::select(
      -x_value,
      -y_value
    )
  
  # Predict the population-level surface, excluding s(Id).
  predictions <- stats::predict(
    model,
    newdata =
      prediction_grid,
    type =
      "response",
    se.fit =
      TRUE,
    exclude =
      "s(Id)"
  )
  
  prediction_surface <- prediction_grid %>%
    dplyr::mutate(
      fit =
        as.numeric(
          predictions$fit
        ),
      
      se_fit =
        as.numeric(
          predictions$se.fit
        ),
      
      fit_lower_approx =
        fit - 1.96 * se_fit,
      
      fit_upper_approx =
        fit + 1.96 * se_fit
    )
  
  # Save the prediction surface as CSV.
  readr::write_csv(
    prediction_surface,
    file.path(
      analysis_results_dir,
      paste0(
        file_stub,
        "_effect_surface.csv"
      )
    )
  )
  
  # Create the effect-surface plot.
  effect_surface_plot <- ggplot2::ggplot(
    prediction_surface,
    ggplot2::aes(
      x =
        .data[[x_variable]],
      y =
        .data[[y_variable]],
      fill =
        fit
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_contour(
      ggplot2::aes(
        z =
          fit
      )
    ) +
    ggplot2::labs(
      title =
        paste0(
          outcome_label,
          ": fitted exposure surface"
        ),
      
      x =
        x_label,
      
      y =
        y_label,
      
      fill =
        "Predicted outcome"
    ) +
    ggplot2::theme_minimal()
  
  # Save the plot.
  ggplot2::ggsave(
    filename =
      file.path(
        analysis_results_dir,
        paste0(
          file_stub,
          "_effect_surface.png"
        )
      ),
    
    plot =
      effect_surface_plot,
    
    width =
      7,
    
    height =
      5,
    
    dpi =
      300
  )
  
  output <- list(
    prediction_surface =
      prediction_surface,
    
    effect_surface_plot =
      effect_surface_plot
  )
  
  rm(
    x_limits,
    y_limits,
    prediction_grid,
    predictions
  )
  
  return(
    output
  )
}

# ------------------------------------------------------------
# STEP 12: Interdaily stability as participant-level outcome
# ------------------------------------------------------------
# PURPOSE:
# Analyse interdaily stability without pseudo-replicating it across
# nights.
#
# IMPORTANT:
# In the preprocessing script, pim_interdaily_stability is joined by Id.
# Therefore it is likely constant within each participant.
#
# MODEL STRUCTURE:
# participant-level interdaily stability ~
#   te(mean nocturnal temperature, mean daytime lMEDI)
#
# This keeps the environmental tensor-product structure from the HTML,
# but moves to participant-level summaries because the outcome is
# participant-level.
# ------------------------------------------------------------

interdaily_stability_model_data <- analysis_sleep_gam %>%
  dplyr::group_by(
    Id
  ) %>%
  dplyr::summarise(
    pim_interdaily_stability =
      dplyr::first(
        stats::na.omit(
          pim_interdaily_stability
        )
      ),
    
    temperature_mean_person =
      mean(
        temperature_mean_night,
        na.rm = TRUE
      ),
    
    twb_mean_person =
      mean(
        twb_mean_night,
        na.rm = TRUE
      ),
    
    lmedi_mean_day_person =
      mean(
        lmedi_mean_day,
        na.rm = TRUE
      ),
    
    n_nights =
      dplyr::n(),
    
    .groups =
      "drop"
  ) %>%
  dplyr::filter(
    is.finite(
      pim_interdaily_stability
    ),
    is.finite(
      temperature_mean_person
    ),
    is.finite(
      lmedi_mean_day_person
    )
  )

interdaily_stability_sample_check <- interdaily_stability_model_data %>%
  dplyr::summarise(
    n_ids =
      dplyr::n(),
    
    median_interdaily_stability =
      median(
        pim_interdaily_stability,
        na.rm = TRUE
      ),
    
    min_interdaily_stability =
      min(
        pim_interdaily_stability,
        na.rm = TRUE
      ),
    
    max_interdaily_stability =
      max(
        pim_interdaily_stability,
        na.rm = TRUE
      )
  )

print(
  interdaily_stability_sample_check,
  width = Inf
)

readr::write_csv(
  interdaily_stability_model_data,
  file.path(
    analysis_results_dir,
    "interdaily_stability_model_data.csv"
  )
)

readr::write_csv(
  interdaily_stability_sample_check,
  file.path(
    analysis_results_dir,
    "interdaily_stability_sample_check.csv"
  )
)

m_interdaily_stability <- mgcv::gam(
  formula =
    pim_interdaily_stability ~
    te(
      temperature_mean_person,
      lmedi_mean_day_person,
      k = c(4, 4)
    ),
  
  data =
    interdaily_stability_model_data,
  
  family =
    gaussian(),
  
  method =
    "REML"
)

saveRDS(
  m_interdaily_stability,
  file.path(
    analysis_results_dir,
    "m_interdaily_stability_temperature_lmedi_gam.rds"
  )
)

sink(
  file.path(
    analysis_results_dir,
    "m_interdaily_stability_summary.txt"
  )
)

print(
  summary(
    m_interdaily_stability
  )
)

sink()


# ------------------------------------------------------------
# STEP 13: Optional precipitation extension
# ------------------------------------------------------------
# PURPOSE:
# Add precipitation once a nightly/daytime precipitation variable is
# available in analysis_sleep.
#
# HTML-ALIGNED STRUCTURE:
# outcome ~ te(Temperature, Irradiance) + s(Precipitation) + s(Id, bs = "re")
# ------------------------------------------------------------

# Example once precipitation_mean_night exists:
#
# m_sleep_efficiency_precip <- mgcv::bam(
#   formula =
#     sleep_efficiency_diary ~
#     te(
#       temperature_mean_night,
#       lmedi_mean_day
#     ) +
#     s(
#       precipitation_mean_night
#     ) +
#     s(
#       Id,
#       bs = "re"
#     ),
#   
#   data =
#     sleep_efficiency_model_data %>%
#     dplyr::filter(
#       is.finite(
#         precipitation_mean_night
#       )
#     ),
#   
#   family =
#     gaussian(),
#   
#   method =
#     "fREML",
#   
#   discrete =
#     TRUE
# )


# ------------------------------------------------------------
# STEP 14: Clean up temporary objects
# ------------------------------------------------------------

rm(
  packages_needed,
  packages_missing,
  required_analysis_variables
)

gc()