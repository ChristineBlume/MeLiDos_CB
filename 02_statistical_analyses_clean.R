# ============================================================
# STATISTICAL ANALYSIS: Sleep outcomes and environmental exposure
# ============================================================
# PURPOSE:
# Fit GAMs aligned with the uploaded Light_climate.html model,
# but using sleep outcomes instead of light exposure.
#
# IMPORTANT:
# - No lagged exposure variables.
# - No within-/between-person decomposition.
# - No age or gender covariates.
# - All outcomes and exposures follow the same workflow:
#   prepare data -> fit model -> extract results -> diagnostics -> plots.
#
# HTML-ALIGNED MODEL LOGIC:
# Original hourly light model:
# lzMEDI ~ s(Time, Id, bs = "fs", xt = list(bs = "cc")) +
#          s(Id_date, bs = "re") +
#          te(Temperature, Irradiance) +
#          s(Precipitation)
#
# Person-night analogue used here:
# outcome ~ te(nocturnal_temperature, daytime_lMEDI) + s(Id, bs = "re")
#
# Person-level analogue for interdaily stability:
# interdaily_stability ~ te(mean_nocturnal_temperature, mean_daytime_lMEDI)
#
# NOTE:
# Interdaily stability is person-level in the current preprocessing.
# Therefore it is analysed once per participant, without s(Id).
# ============================================================


# ------------------------------------------------------------
# STEP 1: Load packages and define output folders
# ------------------------------------------------------------
# PURPOSE:
# Load packages and create a clean output structure.
#
# OUTPUT STRUCTURE:
# output/statistical_analysis/
#   models/       fitted model objects
#   tables/       result and diagnostic tables only
#   plots/        effect-surface and diagnostic plots
#   text/         readable model summaries and gam.check output
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
models_dir <- file.path(analysis_results_dir, "models")
tables_dir <- file.path(analysis_results_dir, "tables")
plots_dir <- file.path(analysis_results_dir, "plots")
text_dir <- file.path(analysis_results_dir, "text")

dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(text_dir, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# STEP 2: Load prepared person-night dataset and add weather if needed
# ------------------------------------------------------------
# PURPOSE:
# Load the person-night dataset created during preprocessing.
#
# IMPORTANT:
# If the saved RDS is an older version without weather variables,
# this step joins the saved weather-night summary back into the
# analysis dataset.
# ------------------------------------------------------------

analysis_sleep <- readRDS(
  "data/processed/analysis_sleep_ucr_person_night.rds"
)

required_weather_variables <- c(
  "temperature_mean_night",
  "twb_mean_night"
)

weather_variables_missing <- !all(
  required_weather_variables %in% names(
    analysis_sleep
  )
)

if (weather_variables_missing) {
  
  message(
    "Weather variables are missing from analysis_sleep. ",
    "Trying to join them from the saved preprocessing weather summary."
  )
  
  weather_summary_candidates <- c(
    "output/preprocessing/09_weather_night_summary.csv",
    "GitHub/MeLiDos_CB/output/preprocessing/09_weather_night_summary.csv"
  )
  
  weather_summary_file <- weather_summary_candidates[
    file.exists(
      weather_summary_candidates
    )
  ][1]
  
  if (is.na(weather_summary_file)) {
    stop(
      "Could not find output/preprocessing/09_weather_night_summary.csv. ",
      "Please rerun the weather preprocessing step and save weather_night first.",
      call. = FALSE
    )
  }
  
  weather_night_saved <- readr::read_csv(
    weather_summary_file,
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      Id =
        as.character(
          Id
        ),
      
      site =
        as.character(
          site
        ),
      
      sleep_date =
        as.Date(
          sleep_date
        )
    )
  
  required_weather_summary_variables <- c(
    "Id",
    "site",
    "sleep_date",
    "temperature_mean_night",
    "temperature_median_night",
    "temperature_min_night",
    "temperature_max_night",
    "rh_mean_night",
    "rh_median_night",
    "twb_mean_night",
    "twb_median_night",
    "twb_min_night",
    "twb_max_night",
    "n_weather_records_night",
    "nocturnal_weather_qc"
  )
  
  available_weather_summary_variables <- intersect(
    required_weather_summary_variables,
    names(
      weather_night_saved
    )
  )
  
  missing_core_weather_variables <- setdiff(
    required_weather_variables,
    available_weather_summary_variables
  )
  
  if (length(missing_core_weather_variables) > 0) {
    stop(
      "The saved weather summary exists, but these core variables are missing: ",
      paste(
        missing_core_weather_variables,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  analysis_sleep <- analysis_sleep %>%
    dplyr::mutate(
      Id =
        as.character(
          Id
        ),
      
      site =
        as.character(
          site
        ),
      
      sleep_date =
        as.Date(
          sleep_date
        )
    ) %>%
    dplyr::select(
      -dplyr::any_of(
        setdiff(
          available_weather_summary_variables,
          c(
            "Id",
            "site",
            "sleep_date"
          )
        )
      )
    ) %>%
    dplyr::left_join(
      weather_night_saved %>%
        dplyr::select(
          dplyr::all_of(
            available_weather_summary_variables
          )
        ),
      by =
        c(
          "Id",
          "site",
          "sleep_date"
        )
    )
  
  rm(
    weather_summary_candidates,
    weather_summary_file,
    weather_night_saved,
    required_weather_summary_variables,
    available_weather_summary_variables,
    missing_core_weather_variables
  )
}

rm(
  required_weather_variables,
  weather_variables_missing
)


# ------------------------------------------------------------
# STEP 3: Check required variables
# ------------------------------------------------------------
# PURPOSE:
# Make sure the required sleep outcomes and exposure variables exist.
#
# NOTE:
# This check is printed only. It is not saved as a separate CSV.
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
    variable %in% names(
      analysis_sleep
    )
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
# STEP 4: Prepare common analysis dataset
# ------------------------------------------------------------
# PURPOSE:
# Create cleaned variables used across all analyses.
#
# IMPORTANT:
# No lagged variables are created.
# ------------------------------------------------------------

analysis_sleep_gam <- analysis_sleep %>%
  dplyr::filter(
    site == "UCR"
  ) %>%
  dplyr::mutate(
    Id = factor(Id),
    sleep_date = as.Date(sleep_date),
    sleep_efficiency_diary = as.numeric(sleep_efficiency_diary),
    sleepquality = as.numeric(sleepquality),
    pim_interdaily_stability = as.numeric(pim_interdaily_stability),
    temperature_mean_night = as.numeric(temperature_mean_night),
    twb_mean_night = as.numeric(twb_mean_night),
    medi_mean_day = as.numeric(medi_mean_day),
    lmedi_mean_day = log10(medi_mean_day + 0.1)
  )

analysis_overview <- analysis_sleep_gam %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_ids = dplyr::n_distinct(Id),
    first_sleep_date = min(sleep_date, na.rm = TRUE),
    last_sleep_date = max(sleep_date, na.rm = TRUE),
    n_with_sleep_efficiency = sum(is.finite(sleep_efficiency_diary)),
    n_with_sleepquality = sum(is.finite(sleepquality)),
    n_with_interdaily_stability = sum(is.finite(pim_interdaily_stability)),
    n_with_temperature = sum(is.finite(temperature_mean_night)),
    n_with_twb = sum(is.finite(twb_mean_night)),
    n_with_lmedi = sum(is.finite(lmedi_mean_day))
  )

print(
  analysis_overview,
  width = Inf
)


# ------------------------------------------------------------
# STEP 5: Define model registry
# ------------------------------------------------------------
# PURPOSE:
# Define all analyses in one table so that every model follows the
# same pattern.
#
# EXPOSURES:
# - air_temperature: primary HTML-aligned exposure
# - wetbulb: humidity-integrated sensitivity exposure
#
# OUTCOMES:
# - sleep efficiency: person-night level
# - subjective sleep quality: person-night level
# - interdaily stability: person level
# ------------------------------------------------------------

model_registry <- tibble::tribble(
  ~model_id,                         ~analysis_level, ~outcome_variable,              ~outcome_label,                  ~temperature_variable,       ~temperature_label,                         ~light_variable,      ~light_label,                  ~exposure_family,
  "sleep_efficiency_air",            "night",        "sleep_efficiency_diary",       "Diary sleep efficiency",       "temperature_mean_night",   "Mean nocturnal temperature",              "lmedi_mean_day",    "Daytime log10(mEDI + 0.1)",  "air_temperature",
  "sleepquality_air",                "night",        "sleepquality",                 "Subjective sleep quality",     "temperature_mean_night",   "Mean nocturnal temperature",              "lmedi_mean_day",    "Daytime log10(mEDI + 0.1)",  "air_temperature",
  "interdaily_stability_air",        "person",       "pim_interdaily_stability",     "Interdaily stability",         "temperature_mean_person",  "Mean nocturnal temperature",              "lmedi_mean_person", "Mean daytime log10(mEDI + 0.1)", "air_temperature",
  "sleep_efficiency_twb",            "night",        "sleep_efficiency_diary",       "Diary sleep efficiency",       "twb_mean_night",           "Mean nocturnal wet-bulb temperature",     "lmedi_mean_day",    "Daytime log10(mEDI + 0.1)",  "wetbulb_sensitivity",
  "sleepquality_twb",                "night",        "sleepquality",                 "Subjective sleep quality",     "twb_mean_night",           "Mean nocturnal wet-bulb temperature",     "lmedi_mean_day",    "Daytime log10(mEDI + 0.1)",  "wetbulb_sensitivity",
  "interdaily_stability_twb",        "person",       "pim_interdaily_stability",     "Interdaily stability",         "twb_mean_person",          "Mean nocturnal wet-bulb temperature",     "lmedi_mean_person", "Mean daytime log10(mEDI + 0.1)", "wetbulb_sensitivity"
)

print(
  model_registry,
  width = Inf
)


# ------------------------------------------------------------
# STEP 6: Prepare person-level dataset for interdaily stability
# ------------------------------------------------------------
# PURPOSE:
# Create one row per participant for interdaily stability models.
#
# WHY:
# pim_interdaily_stability is joined by Id during preprocessing and
# is therefore constant within participant.
# ------------------------------------------------------------

analysis_person_gam <- analysis_sleep_gam %>%
  dplyr::group_by(
    Id
  ) %>%
  dplyr::summarise(
    pim_interdaily_stability = dplyr::if_else(
      all(is.na(pim_interdaily_stability)),
      NA_real_,
      dplyr::first(pim_interdaily_stability[!is.na(pim_interdaily_stability)])
    ),
    temperature_mean_person = mean(temperature_mean_night, na.rm = TRUE),
    twb_mean_person = mean(twb_mean_night, na.rm = TRUE),
    lmedi_mean_person = mean(lmedi_mean_day, na.rm = TRUE),
    n_nights_person = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    temperature_mean_person = dplyr::if_else(
      is.nan(temperature_mean_person),
      NA_real_,
      temperature_mean_person
    ),
    twb_mean_person = dplyr::if_else(
      is.nan(twb_mean_person),
      NA_real_,
      twb_mean_person
    ),
    lmedi_mean_person = dplyr::if_else(
      is.nan(lmedi_mean_person),
      NA_real_,
      lmedi_mean_person
    )
  )

# ------------------------------------------------------------
# STEP 7: Define reusable modelling functions
# ------------------------------------------------------------
# PURPOSE:
# Use the same analysis pattern for every outcome and exposure.
# ------------------------------------------------------------
source("~/GitHub/MeLiDos_CB/Functions_MeLiDos_CB_Stats.R")

# ------------------------------------------------------------
# STEP 8: Fit all models using the same workflow
# ------------------------------------------------------------
# PURPOSE:
# Fit all registered models and create diagnostics/results/plots.
# ------------------------------------------------------------

model_results <- model_registry %>%
  dplyr::group_split(
    model_id
  ) %>%
  purrr::map(
    ~ fit_gam_model(
      model_config = .x[1, ]
    )
  ) %>%
  purrr::set_names(
    model_registry$model_id
  )

model_result_tables <- purrr::map(
  model_results,
  extract_model_results
)

purrr::walk(
  model_results,
  save_model_text_outputs
)

purrr::walk(
  model_results,
  save_model_diagnostic_plot
)

model_effect_surfaces <- purrr::map(
  model_results,
  create_effect_surface_plot
)


# ------------------------------------------------------------
# STEP 9: Combine and save final result tables
# ------------------------------------------------------------
# PURPOSE:
# Save only the tables needed for interpretation.
#
# SAVED TABLES:
# - model registry
# - model fit summaries
# - smooth summaries
# - parametric summaries
# - k checks
# - concurvity summaries
# - term variance contributions
# ------------------------------------------------------------

fit_summary_all <- purrr::map_dfr(
  model_result_tables,
  "fit_summary"
)

smooth_summary_all <- purrr::map_dfr(
  model_result_tables,
  "smooth_summary"
)

parametric_summary_all <- purrr::map_dfr(
  model_result_tables,
  "parametric_summary"
)

k_check_all <- purrr::map_dfr(
  model_result_tables,
  "k_check"
)

concurvity_summary_all <- purrr::map_dfr(
  model_result_tables,
  "concurvity_summary"
)

term_variance_summary_all <- purrr::map_dfr(
  model_result_tables,
  "term_variance_summary"
)

smooth_summary_reporting <- smooth_summary_all %>%
  dplyr::mutate(
    edf = round(edf, 2),
    Ref.df = round(Ref.df, 2),
    F = round(F, 2),
    p_value = dplyr::case_when(
      `p-value` < 0.001 ~ "< .001",
      TRUE ~ paste0(
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
    model_id,
    analysis_level,
    outcome_label,
    exposure_family,
    smooth_term,
    edf,
    Ref.df,
    F,
    p_value
  )

readr::write_csv(
  model_registry,
  file.path(
    tables_dir,
    "model_registry.csv"
  )
)

readr::write_csv(
  fit_summary_all,
  file.path(
    tables_dir,
    "model_fit_summary.csv"
  )
)

readr::write_csv(
  smooth_summary_all,
  file.path(
    tables_dir,
    "smooth_summary_full.csv"
  )
)

readr::write_csv(
  smooth_summary_reporting,
  file.path(
    tables_dir,
    "smooth_summary_reporting.csv"
  )
)

readr::write_csv(
  parametric_summary_all,
  file.path(
    tables_dir,
    "parametric_summary.csv"
  )
)

readr::write_csv(
  k_check_all,
  file.path(
    tables_dir,
    "k_check_summary.csv"
  )
)

readr::write_csv(
  concurvity_summary_all,
  file.path(
    tables_dir,
    "concurvity_summary.csv"
  )
)

readr::write_csv(
  term_variance_summary_all,
  file.path(
    tables_dir,
    "term_variance_contribution.csv"
  )
)


# ------------------------------------------------------------
# STEP 10: Inspect key outputs in R
# ------------------------------------------------------------
# PURPOSE:
# Show the key interpretation tables without saving extra checks.
# ------------------------------------------------------------

View(fit_summary_all)

smooth_summary_reporting

k_check_all

term_variance_summary_all


# ------------------------------------------------------------
# STEP 11: Clean up temporary helper objects
# ------------------------------------------------------------
# PURPOSE:
# Keep final model objects, result tables and effect surfaces in memory,
# but remove package bookkeeping and helper checks.
# ------------------------------------------------------------

rm(
  packages_needed,
  packages_missing,
  required_analysis_variables,
  analysis_variable_check
)

gc()
