# 01_prepare_sleep_exposure_data.R

# ============================================================
# PREPROCESSING: UCR sleep diary, chest mEDI, wrist PIM and weather data
# ============================================================
# PURPOSE:
# Prepare a person-night dataset for analyses of:
# - diary-based sleep outcomes,
# - diary-based sleep efficiency,
# - daytime and evening mEDI exposure from chest-worn light sensors,
# - wrist-based PIM as the primary actimetry candidate,
# - optional nightly outdoor temperature and wet-bulb temperature.
#
# IMPORTANT:
# This script is restricted to UCR data only.
#
# NOTE ON ACTIMETRY:
# Wrist PIM is prepared here as the primary actimetry candidate.
# These wrist-PIM summaries are not GGIR-derived sleep estimates.
# If raw ActLumus accelerometer files are available, GGIR should be
# run separately to extract actigraphy-based sleep timing, sleep duration
# and sleep efficiency.
#
# NOTE ON FUNCTIONS:
# Helper functions are sourced from Functions_MeLiDos_CB.R and are not
# defined in this script.
#
# MAIN OUTPUT:
# data/processed/analysis_sleep_ucr_person_night.csv
# data/processed/analysis_sleep_ucr_person_night.rds
# ============================================================


# ------------------------------------------------------------
# STEP 1: Load required packages
# ------------------------------------------------------------
# PURPOSE:
# Load all packages needed for data loading, preprocessing,
# aggregation, joining and saving.
#
# INPUT:
# List of required package names.
#
# OUTPUT:
# Packages are installed if missing and loaded into the session.
# ------------------------------------------------------------

packages_needed <- c(
  "melidosData",
  "data.table",
  "dplyr",
  "lubridate",
  "purrr",
  "readr",
  "readxl",
  "openxlsx",
  "rlang",
  "slider",
  "stringr",
  "tibble"
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


# ------------------------------------------------------------
# STEP 2: Define analysis settings and output folders
# ------------------------------------------------------------
# PURPOSE:
# Define the study site, time zone, function file and output folders.
#
# INPUT:
# None.
#
# OUTPUT:
# site
# analysis_tz
# functions_file
# data/processed/
# output/preprocessing/
# ------------------------------------------------------------

site <- "UCR"
analysis_tz <- "America/Costa_Rica"

setwd("C:/Users/chris/OneDrive/Documents/GitHub/MeLiDos_CB")

functions_file <- "C:/Users/chris/OneDrive/Documents/GitHub/MeLiDos_CB/Functions_MeLiDos_CB.R"

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/preprocessing",
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# STEP 3: Source helper functions
# ------------------------------------------------------------
# PURPOSE:
# Source all helper functions used in this script and check that
# the required functions are available.
#
# INPUT:
# functions_file
#
# OUTPUT:
# Helper functions available in the R session.
#
# REQUIRED FUNCTIONS:
# load_melidos_flat_strict()
# pick_col()
# pick_col_optional()
# as_posix_analysis_tz()
# combine_date_with_clock_time()
# assign_sleep_night_time()
# calc_twb_stull()
# ------------------------------------------------------------

source(functions_file)

required_functions <- c(
  "load_melidos_flat",
  "pick_col",
  "pick_col_optional",
  "as_posix_analysis_tz",
  "extract_diary_date",
  "extract_diary_clock_time",
  "combine_date_with_clock_time",
  "assign_sleep_night_time",
  "calc_twb_stull",
  "score_cole_kripke_60s",
  "calculate_interdaily_stability"
)

missing_functions <- required_functions[
  !purrr::map_lgl(
    required_functions,
    exists,
    mode = "function"
  )
]

if (length(missing_functions) > 0) {
  stop(
    "The following required helper functions are missing from functions_file: ",
    paste(missing_functions, collapse = ", "),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# STEP 4: Load UCR datasets
# ------------------------------------------------------------
# PURPOSE:
# Load only the UCR datasets needed for this preprocessing script.
# This block is intentionally strict:
# - it overwrites any previous data_list object,
# - it checks whether each dataset was loaded,
# - it stops immediately if light_chest or light_wrist is missing.
#
# INPUT:
# melidosData package
# site = "UCR"
#
# OUTPUT:
# data_list
# sleep_diary_raw
# wearlog_raw
# light_chest_raw
# light_wrist_raw
# loaded_modalities_check
#
# DATASETS:
# sleepdiaries: sleep timing and diary-based sleep outcomes.
# wearlog:      device wear information.
# light_chest:  chest-level light data used for mEDI exposure.
# light_wrist:  wrist-level actimetry data used for PIM-derived sleep.
# ------------------------------------------------------------

modalities <- c(
  "sleepdiaries",
  "wearlog",
  "light_chest",
  "light_wrist"
)

# Overwrite any previously existing data_list to avoid accidentally using
# an older object from a previous all-site run.

data_list <- modalities %>%
  rlang::set_names() %>%
  purrr::map(
    ~ load_melidos_flat(
      modality_value = .x,
      site_value = site,
      tz_value = dplyr::if_else(
        .x == "sleepdiaries",
        analysis_tz,
        "UTC"
      )
    )
  )

loaded_modalities_check <- tibble::tibble(
  modality = names(data_list),
  n_rows = purrr::map_int(
    data_list,
    nrow
  ),
  n_cols = purrr::map_int(
    data_list,
    ncol
  ),
  size_gb = purrr::map_dbl(
    data_list,
    ~ as.numeric(object.size(.x)) / 1024^3
  )
)

print(loaded_modalities_check)

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   loaded_modalities_check,
#   "output/preprocessing/00_loaded_modalities_check.csv"
# )

if (!"light_chest" %in% names(data_list)) {
  stop(
    "light_chest was not loaded. Chest data are required for mEDI exposure.",
    call. = FALSE
  )
}

if (!"light_wrist" %in% names(data_list)) {
  stop(
    "light_wrist was not loaded. Wrist data are required for PIM-based actimetry. ",
    "If the wrist modality has a different name in melidosData, change it in the modalities vector.",
    call. = FALSE
  )
}

sleep_diary_raw <- data_list[["sleepdiaries"]] %>%
  dplyr::filter(
    site == "UCR"
  )

wearlog_raw <- data_list[["wearlog"]] %>%
  dplyr::filter(
    site == "UCR"
  )

light_chest_raw <- data_list[["light_chest"]] %>%
  dplyr::filter(
    site == "UCR"
  )

light_wrist_raw <- data_list[["light_wrist"]] %>%
  dplyr::filter(
    site == "UCR"
  )


# ------------------------------------------------------------
# STEP 5: Check that only UCR data were loaded
# ------------------------------------------------------------
# PURPOSE:
# Check that the raw datasets contain only one site and that
# this site is UCR.
#
# INPUT:
# sleep_diary_raw
# wearlog_raw
# light_chest_raw
# light_wrist_raw
#
# OUTPUT:
# ucr_site_check
# output/preprocessing/00_ucr_site_check.csv, optional
# ------------------------------------------------------------

ucr_site_check <- tibble::tibble(
  dataset = c(
    "sleepdiaries",
    "wearlog",
    "light_chest",
    "light_wrist"
  ),
  n_rows = c(
    nrow(sleep_diary_raw),
    nrow(wearlog_raw),
    nrow(light_chest_raw),
    nrow(light_wrist_raw)
  ),
  n_sites = c(
    dplyr::n_distinct(sleep_diary_raw$site),
    dplyr::n_distinct(wearlog_raw$site),
    dplyr::n_distinct(light_chest_raw$site),
    dplyr::n_distinct(light_wrist_raw$site)
  ),
  sites = c(
    paste(unique(sleep_diary_raw$site), collapse = ", "),
    paste(unique(wearlog_raw$site), collapse = ", "),
    paste(unique(light_chest_raw$site), collapse = ", "),
    paste(unique(light_wrist_raw$site), collapse = ", ")
  )
)

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   ucr_site_check,
#   "output/preprocessing/00_ucr_site_check.csv"
# )

ucr_site_check


# ------------------------------------------------------------
# STEP 6: Prepare diary-based sleep outcomes
# ------------------------------------------------------------
# PURPOSE:
# Prepare sleep diary variables for person-night analyses.
# This includes:
# - reconstructing sleep-night timestamps,
# - preserving the reported diary clock times,
# - defining wake_date from the diary wake date,
# - defining sleep_date as the day before wake_date,
# - converting duration variables to minutes,
# - calculating diary-based sleep efficiency using sleepprep_time.
#
# INPUT:
# sleep_diary_raw
#
# OUTPUT:
# sleep_diary
# diary_clock_reconstruction_check
# diary_clock_reconstruction_summary
#
# MAIN DERIVED VARIABLES:
# wake_date
# sleep_date
# sleep_start
# wake_time
# sleepprep_time
# out_ofbed_time
# sleep_duration_min
# sleepdelay_min
# awake_duration_min
# time_sleepprep_to_wake_min
# sleep_efficiency_diary
#
# DATE LOGIC:
# wake_date is the calendar date of waking.
# sleep_date is the day before wake_date and is used as the night label.
#
# TIME-ZONE LOGIC:
# Diary times are treated as displayed clock times.
# They are not shifted between time zones.
#
# SLEEP EFFICIENCY FORMULA:
# sleep_efficiency_diary =
#   sleep_duration_min / time_sleepprep_to_wake_min * 100
# ------------------------------------------------------------

sleep_diary <- sleep_diary_raw %>%
  dplyr::mutate(
    raw_wake_date =
      extract_diary_date(
        wake
      ),
    
    wake_date =
      raw_wake_date,
    
    sleep_date =
      wake_date - lubridate::days(1)
  ) %>%
  dplyr::mutate(
    sleep_start =
      assign_sleep_night_time(
        time_source = sleep,
        sleep_date = sleep_date,
        wake_date = wake_date,
        tz_value = analysis_tz
      ),
    
    sleepprep_time =
      assign_sleep_night_time(
        time_source = sleepprep,
        sleep_date = sleep_date,
        wake_date = wake_date,
        tz_value = analysis_tz
      ),
    
    wake_time =
      combine_date_with_clock_time(
        date_value = wake_date,
        time_source = wake,
        tz_value = analysis_tz
      ),
    
    out_ofbed_time =
      combine_date_with_clock_time(
        date_value = wake_date,
        time_source = out_ofbed,
        tz_value = analysis_tz
      )
  ) %>%
  dplyr::mutate(
    sleep_duration_min =
      as.numeric(
        sleep_duration
      ) * 60,
    
    sleepdelay_min =
      as.numeric(
        sleepdelay
      ),
    
    awake_duration_min =
      as.numeric(
        awake_duration
      ),
    
    time_sleepprep_to_wake_min =
      as.numeric(
        difftime(
          wake_time,
          sleepprep_time,
          units = "mins"
        )
      ),
    
    sleep_efficiency_diary =
      sleep_duration_min / time_sleepprep_to_wake_min * 100,
    
    sleep_efficiency_diary =
      dplyr::if_else(
        sleep_efficiency_diary >= 0 &
          sleep_efficiency_diary <= 100,
        sleep_efficiency_diary,
        NA_real_
      )
  ) %>%
  dplyr::select(
    -raw_wake_date
  )

# ------------------------------------------------------------
# STEP 6a: Check diary clock-time reconstruction
# ------------------------------------------------------------
# PURPOSE:
# Verify that reconstructed timestamps preserve the displayed diary
# clock times. This specifically checks for systematic time-zone shifts.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# diary_clock_reconstruction_check
# diary_clock_reconstruction_summary
# ------------------------------------------------------------

diary_clock_reconstruction_check <- sleep_diary %>%
  dplyr::transmute(
    Id,
    site,
    sleep_date,
    wake_date,
    
    raw_sleepprep_clock =
      extract_diary_clock_time(
        sleepprep
      ),
    
    raw_sleep_clock =
      extract_diary_clock_time(
        sleep
      ),
    
    raw_wake_clock =
      extract_diary_clock_time(
        wake
      ),
    
    raw_out_ofbed_clock =
      extract_diary_clock_time(
        out_ofbed
      ),
    
    reconstructed_sleepprep_clock =
      format(
        sleepprep_time,
        format = "%H:%M:%S"
      ),
    
    reconstructed_sleep_clock =
      format(
        sleep_start,
        format = "%H:%M:%S"
      ),
    
    reconstructed_wake_clock =
      format(
        wake_time,
        format = "%H:%M:%S"
      ),
    
    reconstructed_out_ofbed_clock =
      format(
        out_ofbed_time,
        format = "%H:%M:%S"
      ),
    
    sleep_clock_matches =
      raw_sleep_clock == reconstructed_sleep_clock,
    
    sleepprep_clock_matches =
      raw_sleepprep_clock == reconstructed_sleepprep_clock,
    
    wake_clock_matches =
      raw_wake_clock == reconstructed_wake_clock,
    
    out_ofbed_clock_matches =
      raw_out_ofbed_clock == reconstructed_out_ofbed_clock,
    
    sleep_interval_min =
      as.numeric(
        difftime(
          wake_time,
          sleep_start,
          units = "mins"
        )
      ),
    
    sleepprep_to_wake_min =
      as.numeric(
        difftime(
          wake_time,
          sleepprep_time,
          units = "mins"
        )
      )
  )

diary_clock_reconstruction_summary <- diary_clock_reconstruction_check %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),
    
    n_sleep_clock_mismatches =
      sum(!sleep_clock_matches, na.rm = TRUE),
    
    n_sleepprep_clock_mismatches =
      sum(!sleepprep_clock_matches, na.rm = TRUE),
    
    n_wake_clock_mismatches =
      sum(!wake_clock_matches, na.rm = TRUE),
    
    n_out_ofbed_clock_mismatches =
      sum(!out_ofbed_clock_matches, na.rm = TRUE),
    
    min_sleep_interval_min =
      min(sleep_interval_min, na.rm = TRUE),
    
    median_sleep_interval_min =
      median(sleep_interval_min, na.rm = TRUE),
    
    max_sleep_interval_min =
      max(sleep_interval_min, na.rm = TRUE),
    
    min_sleepprep_to_wake_min =
      min(sleepprep_to_wake_min, na.rm = TRUE),
    
    median_sleepprep_to_wake_min =
      median(sleepprep_to_wake_min, na.rm = TRUE),
    
    max_sleepprep_to_wake_min =
      max(sleepprep_to_wake_min, na.rm = TRUE)
  )

View(diary_clock_reconstruction_summary)

# ------------------------------------------------------------
# STEP 6b: Check remaining implausible diary clock times
# ------------------------------------------------------------
# PURPOSE:
# Identify sleep and wake times that remain implausible after
# clock-time reconstruction. This is a data check, not an exclusion.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# implausible_sleep_clock_times
# ------------------------------------------------------------

implausible_sleep_clock_times <- sleep_diary %>%
  dplyr::filter(
    lubridate::hour(sleep_start) >= 12 &
      lubridate::hour(sleep_start) < 18 |
      lubridate::hour(wake_time) < 3 |
      lubridate::hour(wake_time) > 12
  ) %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    wake_date,
    sleep,
    sleepprep,
    wake,
    out_ofbed,
    sleep_start,
    sleepprep_time,
    wake_time,
    out_ofbed_time
  )

View(implausible_sleep_clock_times)

# ------------------------------------------------------------
# STEP 7: Check sleep-night date reconstruction and durations
# ------------------------------------------------------------
# PURPOSE:
# Check that:
# - sleep_date is the day before wake_date,
# - reconstructed sleep_start occurs before wake_time,
# - duration variables have plausible values,
# - sleep efficiency is available and within range.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# sleep_date_check
# sleep_duration_unit_check
# sleep_diary_check
# ------------------------------------------------------------

sleep_date_check <- sleep_diary %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),

    n_sleep_date_equals_wake_date =
      sum(sleep_date == wake_date, na.rm = TRUE),

    n_sleep_date_day_before_wake_date =
      sum(sleep_date == wake_date - lubridate::days(1), na.rm = TRUE),

    n_sleep_start_after_wake_time =
      sum(sleep_start >= wake_time, na.rm = TRUE),

    n_sleepprep_after_wake_time =
      sum(sleepprep_time >= wake_time, na.rm = TRUE),

    min_sleep_start =
      min(sleep_start, na.rm = TRUE),

    max_sleep_start =
      max(sleep_start, na.rm = TRUE),

    min_wake_time =
      min(wake_time, na.rm = TRUE),

    max_wake_time =
      max(wake_time, na.rm = TRUE)
  )

sleep_duration_unit_check <- sleep_diary %>%
  dplyr::summarise(
    median_sleep_duration_raw_hours =
      median(sleep_duration, na.rm = TRUE),

    median_sleep_duration_min =
      median(sleep_duration_min, na.rm = TRUE),

    median_sleepdelay_min =
      median(sleepdelay_min, na.rm = TRUE),

    median_awake_duration_min =
      median(awake_duration_min, na.rm = TRUE),

    median_time_sleepprep_to_wake_min =
      median(time_sleepprep_to_wake_min, na.rm = TRUE),

    median_sleep_efficiency_diary =
      median(sleep_efficiency_diary, na.rm = TRUE),

    n_implausible_sleepprep_to_wake =
      sum(
        time_sleepprep_to_wake_min <= 0 |
          time_sleepprep_to_wake_min > 18 * 60,
        na.rm = TRUE
      )
  )

sleep_diary_check <- sleep_diary %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),

    n_ids =
      dplyr::n_distinct(Id),

    n_sites =
      dplyr::n_distinct(site),

    sites =
      paste(unique(site), collapse = ", "),

    first_sleep_date =
      min(sleep_date, na.rm = TRUE),

    last_sleep_date =
      max(sleep_date, na.rm = TRUE),

    n_missing_sleep_duration =
      sum(is.na(sleep_duration_min)),

    n_missing_time_sleepprep_to_wake =
      sum(is.na(time_sleepprep_to_wake_min)),

    n_missing_sleep_efficiency =
      sum(is.na(sleep_efficiency_diary)),

    min_sleep_efficiency =
      min(sleep_efficiency_diary, na.rm = TRUE),

    median_sleep_efficiency =
      median(sleep_efficiency_diary, na.rm = TRUE),

    max_sleep_efficiency =
      max(sleep_efficiency_diary, na.rm = TRUE)
  )

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   sleep_date_check,
#   "output/preprocessing/01b_sleep_date_check.csv"
# )
#
# readr::write_csv(
#   sleep_duration_unit_check,
#   "output/preprocessing/01c_sleep_duration_unit_check.csv"
# )
#
# readr::write_csv(
#   sleep_diary_check,
#   "output/preprocessing/01_sleep_diary_check.csv"
# )

View(sleep_date_check)
View(sleep_duration_unit_check)
View(sleep_diary_check)

# ------------------------------------------------------------
# STEP 8: Define sleep and exposure windows
# ------------------------------------------------------------
# PURPOSE:
# Define the time windows used to assign exposure data to each
# sleep night.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# sleep_windows
#
# WINDOW DEFINITIONS:
# sleep_start to wake_time:
#   Reported sleep interval from the diary.
#
# night_start_fixed to night_end_fixed:
#   Fixed night window for temperature exposure and fixed-night
#   PIM summaries.
#
# pre_sleep_anchor:
#   Preferred end point of waking light exposure.
#   sleepprep_time is used first, because this marks the beginning
#   of the sleep-opportunity window.
#   sleep_start is used as fallback if sleepprep_time is missing.
#
# medi_day_start to medi_day_end:
#   Earlier daytime mEDI exposure on the sleep_date.
#   This window ends 4 hours before pre_sleep_anchor.
#
# medi_evening_start to medi_evening_end:
#   Evening/pre-sleep mEDI exposure on the sleep_date.
#   This window covers the last 4 hours before pre_sleep_anchor.
#
# IMPORTANT:
# medi_day and medi_evening are mutually exclusive.
# ------------------------------------------------------------

sleep_windows <- sleep_diary %>%
  dplyr::mutate(
    pre_sleep_anchor =
      dplyr::coalesce(
        sleepprep_time,
        sleep_start
      )
  ) %>%
  dplyr::transmute(
    Id,
    site,
    sleep_date,
    wake_date,
    sleep_start,
    wake_time,
    sleepprep_time,
    out_ofbed_time,
    pre_sleep_anchor,
    
    night_start_fixed =
      as.POSIXct(
        paste0(sleep_date, " 22:00:00"),
        tz = analysis_tz
      ),
    
    night_end_fixed =
      as.POSIXct(
        paste0(wake_date, " 07:00:00"),
        tz = analysis_tz
      ),
    
    medi_day_start =
      as.POSIXct(
        paste0(sleep_date, " 06:00:00"),
        tz = analysis_tz
      ),
    
    medi_evening_start =
      pre_sleep_anchor - lubridate::hours(4),
    
    medi_evening_end =
      pre_sleep_anchor,
    
    medi_day_end =
      medi_evening_start
  ) %>%
  dplyr::filter(
    !is.na(Id),
    !is.na(site),
    !is.na(sleep_date),
    !is.na(wake_date),
    !is.na(sleep_start),
    !is.na(wake_time),
    !is.na(pre_sleep_anchor),
    wake_time > sleep_start,
    pre_sleep_anchor > medi_day_start,
    medi_evening_end > medi_evening_start,
    medi_day_end > medi_day_start
  )

# ------------------------------------------------------------
# STEP 8a: Check exposure window definitions
# ------------------------------------------------------------
# PURPOSE:
# Verify that daytime and evening mEDI windows do not overlap.
#
# INPUT:
# sleep_windows
#
# OUTPUT:
# exposure_window_check
# ------------------------------------------------------------

exposure_window_check <- sleep_windows %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),
    
    n_medi_day_evening_overlap =
      sum(
        medi_day_end > medi_evening_start &
          medi_day_start < medi_evening_end,
        na.rm = TRUE
      ),
    
    min_medi_day_duration_min =
      min(
        as.numeric(
          difftime(
            medi_day_end,
            medi_day_start,
            units = "mins"
          )
        ),
        na.rm = TRUE
      ),
    
    median_medi_day_duration_min =
      median(
        as.numeric(
          difftime(
            medi_day_end,
            medi_day_start,
            units = "mins"
          )
        ),
        na.rm = TRUE
      ),
    
    min_medi_evening_duration_min =
      min(
        as.numeric(
          difftime(
            medi_evening_end,
            medi_evening_start,
            units = "mins"
          )
        ),
        na.rm = TRUE
      ),
    
    median_medi_evening_duration_min =
      median(
        as.numeric(
          difftime(
            medi_evening_end,
            medi_evening_start,
            units = "mins"
          )
        ),
        na.rm = TRUE
      )
  )

View(
  exposure_window_check
)

# ------------------------------------------------------------
# STEP 9: Identify mEDI columns in light_chest and PIM columns in light_wrist
# ------------------------------------------------------------
# PURPOSE:
# Identify:
# - the date-time column and mEDI column in chest-level light data,
# - the date-time column and PIM column in wrist-level actimetry data.
#
# INPUT:
# light_chest_raw
# light_wrist_raw
#
# OUTPUT:
# light_chest_datetime_col
# medi_col
# wrist_datetime_col
# wrist_pim_col
# light_variable_check
# output/preprocessing/02_light_variable_check.csv, optional
#
# IMPORTANT:
# mEDI is derived from chest data.
# PIM-based actimetry is derived from wrist data.
# ------------------------------------------------------------

light_chest_datetime_col <- pick_col(
  data = light_chest_raw,
  candidates = c(
    "datetime",
    "timestamp",
    "time",
    "Time",
    "date_time",
    "DateTime"
  ),
  pattern = "datetime|timestamp|time",
  object_name = "light_chest"
)

medi_col <- pick_col(
  data = light_chest_raw,
  candidates = c(
    "mEDI",
    "MEDI",
    "medi",
    "melanopic_EDI",
    "melanopic_edi",
    "melanopicEDI",
    "melEDI"
  ),
  pattern = "medi|melanopic.*edi|mel.*edi",
  object_name = "light_chest"
)

wrist_datetime_col <- pick_col(
  data = light_wrist_raw,
  candidates = c(
    "datetime",
    "timestamp",
    "time",
    "Time",
    "date_time",
    "DateTime"
  ),
  pattern = "datetime|timestamp|time",
  object_name = "light_wrist"
)

wrist_pim_col <- pick_col(
  data = light_wrist_raw,
  candidates = c(
    "PIM",
    "pim"
  ),
  pattern = "^pim$",
  object_name = "light_wrist"
)

light_variable_check <- tibble::tibble(
  light_chest_datetime_col = light_chest_datetime_col,
  medi_col = medi_col,
  wrist_datetime_col = wrist_datetime_col,
  wrist_pim_col = wrist_pim_col
)

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   light_variable_check,
#   "output/preprocessing/02_light_variable_check.csv"
# )

light_variable_check


# ------------------------------------------------------------
# STEP 10: Prepare chest mEDI data and wrist PIM data
# ------------------------------------------------------------
# PURPOSE:
# Prepare two separate data streams:
# - light_chest: chest-level mEDI data for light exposure.
# - actimetry_wrist: wrist-level PIM data for actimetry.
#
# INPUT:
# light_chest_raw
# light_wrist_raw
# light_chest_datetime_col
# medi_col
# wrist_datetime_col
# wrist_pim_col
#
# OUTPUT:
# light_chest
# actimetry_wrist
# ------------------------------------------------------------

selected_light_columns <- unique(
  stats::na.omit(
    c(
      "Id",
      "site",
      light_chest_datetime_col,
      medi_col
    )
  )
)

light_chest <- light_chest_raw %>%
  dplyr::select(
    dplyr::all_of(selected_light_columns)
  ) %>%
  dplyr::rename(
    datetime =
      dplyr::all_of(light_chest_datetime_col),

    mEDI =
      dplyr::all_of(medi_col)
  ) %>%
  dplyr::mutate(
    datetime =
      as_posix_analysis_tz(
        datetime,
        analysis_tz
      ),

    mEDI =
      as.numeric(mEDI)
  )

selected_wrist_columns <- unique(
  stats::na.omit(
    c(
      "Id",
      "site",
      wrist_datetime_col,
      wrist_pim_col
    )
  )
)

actimetry_wrist <- light_wrist_raw %>%
  dplyr::select(
    dplyr::all_of(selected_wrist_columns)
  ) %>%
  dplyr::rename(
    datetime =
      dplyr::all_of(wrist_datetime_col),

    PIM =
      dplyr::all_of(wrist_pim_col)
  ) %>%
  dplyr::mutate(
    datetime =
      as_posix_analysis_tz(
        datetime,
        analysis_tz
      ),

    PIM =
      as.numeric(PIM)
  )


# ------------------------------------------------------------
# STEP 11: Aggregate daytime and evening chest mEDI
# ------------------------------------------------------------
# PURPOSE:
# Aggregate chest-derived mEDI exposure for each person-night.
#
# INPUT:
# light_chest
# sleep_windows
#
# OUTPUT:
# medi_day
# medi_evening
#
# EXPOSURE LOGIC:
# mEDI from the day of sleep onset is assigned to the following
# sleep night.
#
# EXAMPLE:
# mEDI on Monday daytime and evening is assigned to the sleep
# night from Monday to Tuesday.
# ------------------------------------------------------------

light_dt <- data.table::as.data.table(light_chest)
windows_dt <- data.table::as.data.table(sleep_windows)

data.table::setkey(
  light_dt,
  Id,
  site,
  datetime
)

medi_day <- light_dt[
  windows_dt,
  on = .(
    Id,
    site,
    datetime >= medi_day_start,
    datetime < medi_day_end
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
][
  ,
  .(
    n_medi_records_day =
      .N,

    medi_mean_day =
      mean(mEDI, na.rm = TRUE),

    medi_median_day =
      median(mEDI, na.rm = TRUE),

    medi_logmean_day =
      mean(log10(mEDI + 0.1), na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

medi_evening <- light_dt[
  windows_dt,
  on = .(
    Id,
    site,
    datetime >= medi_evening_start,
    datetime < medi_evening_end
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
][
  ,
  .(
    n_medi_records_evening =
      .N,

    medi_mean_evening =
      mean(mEDI, na.rm = TRUE),

    medi_median_evening =
      median(mEDI, na.rm = TRUE),

    medi_logmean_evening =
      mean(log10(mEDI + 0.1), na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

medi_day <- tibble::as_tibble(medi_day)
medi_evening <- tibble::as_tibble(medi_evening)


# ------------------------------------------------------------
# STEP 12: Aggregate wrist PIM and derive diary-anchored actigraphy variables
# ------------------------------------------------------------
# PURPOSE:
# Aggregate wrist PIM as the primary actimetry candidate and derive
# exploratory diary-anchored wrist-PIM sleep/wake variables.
#
# INPUT:
# actimetry_wrist
# sleep_windows
#
# OUTPUT:
# pim_reported_sleep
# pim_fixed_night
# pim_sleep_night
# pim_sleep_scoring_check
# sleep_amount_discrepancy_check
# pim_threshold_sensitivity_check
# pim_night_retention_check
# pim_interdaily_stability
#
# IMPORTANT:
# These are wrist-PIM-based actimetry estimates. They are not
# GGIR-derived and not ActStudio-derived sleep estimates.
# ------------------------------------------------------------

wrist_dt <- data.table::as.data.table(
  actimetry_wrist
)

data.table::setkey(
  wrist_dt,
  Id,
  site,
  datetime
)

pim_reported_sleep <- wrist_dt[
  windows_dt,
  on = .(
    Id,
    site,
    datetime >= sleep_start,
    datetime < wake_time
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
][
  ,
  .(
    n_pim_records_reported_sleep =
      .N,

    pim_mean_reported_sleep =
      mean(PIM, na.rm = TRUE),

    pim_median_reported_sleep =
      median(PIM, na.rm = TRUE),

    pim_sum_reported_sleep =
      sum(PIM, na.rm = TRUE),

    pim_max_reported_sleep =
      max(PIM, na.rm = TRUE),

    pim_zero_prop_reported_sleep =
      mean(PIM == 0, na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

pim_fixed_night <- wrist_dt[
  windows_dt,
  on = .(
    Id,
    site,
    datetime >= night_start_fixed,
    datetime < night_end_fixed
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
][
  ,
  .(
    n_pim_records_fixed_night =
      .N,

    pim_mean_fixed_night =
      mean(PIM, na.rm = TRUE),

    pim_median_fixed_night =
      median(PIM, na.rm = TRUE),

    pim_sum_fixed_night =
      sum(PIM, na.rm = TRUE),

    pim_max_fixed_night =
      max(PIM, na.rm = TRUE),

    pim_zero_prop_fixed_night =
      mean(PIM == 0, na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

pim_reported_sleep <- tibble::as_tibble(
  pim_reported_sleep
)

pim_fixed_night <- tibble::as_tibble(
  pim_fixed_night
)

# ------------------------------------------------------------
# STEP 12a: Derive diary-anchored wrist-PIM sleep variables
# ------------------------------------------------------------
# PURPOSE:
# Derive exploratory wrist-PIM sleep variables.
#
# INPUT:
# actimetry_wrist
# sleep_windows
#
# OUTPUT:
# pim_sleep_night
#
# METHOD:
# Wrist PIM is aggregated from 10-second to 60-second epochs.
# Two Cole-Kripke-style thresholds are used:
# - a lower threshold for detecting candidate timing transitions,
# - a higher threshold for scoring sleep/wake amount.
#
# IMPORTANT:
# Sleep onset and offset are diary-anchored PIM refinements. They
# are not fully independent actigraphy-derived timing estimates.
# ------------------------------------------------------------

pim_ck_threshold_timing <- 1.5
pim_ck_threshold_sleep_amount <- 4.5

pim_60s <- actimetry_wrist %>%
  dplyr::filter(
    !is.na(Id),
    !is.na(site),
    !is.na(datetime),
    !is.na(PIM)
  ) %>%
  dplyr::mutate(
    datetime_60s =
      lubridate::floor_date(
        datetime,
        unit = "60 seconds"
      )
  ) %>%
  dplyr::group_by(
    Id,
    site,
    datetime_60s
  ) %>%
  dplyr::summarise(
    PIM_60s =
      sum(
        PIM,
        na.rm = TRUE
      ),

    n_pim_10s_epochs =
      dplyr::n(),

    .groups =
      "drop"
  ) %>%
  dplyr::filter(
    n_pim_10s_epochs >= 4
  ) %>%
  dplyr::arrange(
    Id,
    site,
    datetime_60s
  ) %>%
  dplyr::group_by(
    Id,
    site
  ) %>%
  dplyr::mutate(
    pim_ck_sleep_timing =
      score_cole_kripke_60s(
        activity =
          PIM_60s,
        threshold =
          pim_ck_threshold_timing
      ),

    pim_ck_sleep_amount =
      score_cole_kripke_60s(
        activity =
          PIM_60s,
        threshold =
          pim_ck_threshold_sleep_amount
      ),

    # Legacy name used in later aggregation.
    # This refers to the sleep/wake scoring used for sleep amount.
    pim_ck_sleep =
      pim_ck_sleep_amount
  ) %>%
  dplyr::ungroup()

# ------------------------------------------------------------
# STEP 12a.1: Match 60-second PIM epochs to sleep-opportunity windows
# ------------------------------------------------------------
# PURPOSE:
# Assign each 60-second PIM epoch to the correct participant-night
# sleep-opportunity window without creating a many-to-many join.
#
# WINDOW:
# Start: sleepprep_time
# End:   out_ofbed_time, with wake_time as fallback if out_ofbed_time
#        is missing.
# ------------------------------------------------------------

pim_60s_dt <- data.table::as.data.table(
  pim_60s
)

pim_60s_dt[
  ,
  epoch_start := datetime_60s
]

pim_60s_dt[
  ,
  epoch_end := datetime_60s + lubridate::minutes(1)
]

sleep_windows_pim_dt <- sleep_windows %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    sleepprep_time,
    sleep_start,
    wake_time,
    out_ofbed_time
  ) %>%
  dplyr::mutate(
    actigraphy_window_start =
      sleepprep_time,

    actigraphy_window_end =
      dplyr::coalesce(
        out_ofbed_time,
        wake_time
      )
  ) %>%
  dplyr::filter(
    !is.na(Id),
    !is.na(site),
    !is.na(sleep_date),
    !is.na(actigraphy_window_start),
    !is.na(actigraphy_window_end),
    actigraphy_window_end > actigraphy_window_start
  ) %>%
  data.table::as.data.table()

data.table::setkey(
  sleep_windows_pim_dt,
  Id,
  site,
  actigraphy_window_start,
  actigraphy_window_end
)

pim_sleep_epoch_candidates <- data.table::foverlaps(
  x =
    pim_60s_dt,
  y =
    sleep_windows_pim_dt,
  by.x =
    c(
      "Id",
      "site",
      "epoch_start",
      "epoch_end"
    ),
  by.y =
    c(
      "Id",
      "site",
      "actigraphy_window_start",
      "actigraphy_window_end"
    ),
  type =
    "within",
  nomatch =
    0L
) %>%
  tibble::as_tibble() %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    sleepprep_time,
    sleep_start,
    wake_time,
    out_ofbed_time,
    datetime_60s,
    PIM_60s,
    n_pim_10s_epochs,
    pim_ck_sleep_timing,
    pim_ck_sleep_amount,
    pim_ck_sleep
  ) %>%
  dplyr::arrange(
    Id,
    site,
    sleep_date,
    datetime_60s
  ) %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::mutate(
    prop_sleep_next_10_timing =
      slider::slide_dbl(
        pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 0,
        .after = 9,
        .complete = TRUE
      ),

    prop_sleep_next_30_timing =
      slider::slide_dbl(
        pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 0,
        .after = 29,
        .complete = TRUE
      ),

    prop_sleep_prev_15_timing =
      slider::slide_dbl(
        pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 14,
        .after = 0,
        .complete = TRUE
      ),

    prop_sleep_prev_30_timing =
      slider::slide_dbl(
        pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 29,
        .after = 0,
        .complete = TRUE
      ),

    prop_wake_next_10_timing =
      slider::slide_dbl(
        !pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 0,
        .after = 9,
        .complete = TRUE
      ),

    prop_wake_next_15_timing =
      slider::slide_dbl(
        !pim_ck_sleep_timing,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = 0,
        .after = 14,
        .complete = TRUE
      ),

    sustained_sleep_10min_timing =
      prop_sleep_next_10_timing >= 0.90,

    sustained_wake_10min_timing =
      prop_wake_next_10_timing >= 0.80
  ) %>%
  dplyr::ungroup()

pim_sleep_windows <- sleep_windows_pim_dt %>%
  tibble::as_tibble() %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    sleepprep_time,
    sleep_start,
    wake_time,
    out_ofbed_time,
    actigraphy_window_start,
    actigraphy_window_end
  )

# ------------------------------------------------------------
# STEP 12a.2: Derive unconstrained immobility onset for QC
# ------------------------------------------------------------
# PURPOSE:
# Derive the first sustained low-movement period after preparation
# for sleep. This variable is kept as a quality-control marker for
# quiet wakefulness before reported sleep onset.
# ------------------------------------------------------------

pim_immobility_onset_unconstrained <- pim_sleep_epoch_candidates %>%
  dplyr::filter(
    sustained_sleep_10min_timing
  ) %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::summarise(
    pim_immobility_onset_unconstrained =
      min(
        datetime_60s,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  )

# ------------------------------------------------------------
# STEP 12a.3: Select diary-anchored PIM-refined sleep onset
# ------------------------------------------------------------
# PURPOSE:
# Select the best wake-to-sleep transition candidate near the diary
# sleep_start. If no candidate is detected, fall back to diary
# sleep_start and flag the source.
#
# CANDIDATE WINDOW:
# sleep_start - 30 minutes to sleep_start + 90 minutes.
#
# CANDIDATE SCORE:
# Lower scores indicate better candidates. The score favours:
# - closeness to diary sleep_start,
# - stable sleep-like epochs after the candidate,
# - wake evidence before the candidate.
# ------------------------------------------------------------

pim_onset_transition_candidates <- pim_sleep_epoch_candidates %>%
  dplyr::filter(
    datetime_60s >= sleep_start - lubridate::minutes(30),
    datetime_60s <= sleep_start + lubridate::minutes(90),
    sustained_sleep_10min_timing
  ) %>%
  dplyr::mutate(
    onset_distance_min =
      abs(
        as.numeric(
          difftime(
            datetime_60s,
            sleep_start,
            units = "mins"
          )
        )
      ),

    onset_following_sleep_score =
      dplyr::coalesce(
        prop_sleep_next_30_timing,
        prop_sleep_next_10_timing,
        0
      ),

    onset_preceding_wake_score =
      1 -
      dplyr::coalesce(
        prop_sleep_prev_15_timing,
        1
      ),

    onset_transition_score =
      onset_distance_min -
      15 * onset_following_sleep_score -
      10 * onset_preceding_wake_score,

    onset_candidate_quality =
      dplyr::case_when(
        onset_following_sleep_score >= 0.80 &
          onset_preceding_wake_score >= 0.40 ~
          "transition",

        onset_following_sleep_score >= 0.80 ~
          "sustained_sleep_only",

        TRUE ~
          "weak"
      )
  )

pim_onset_transition_detected <- pim_onset_transition_candidates %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::arrange(
    onset_transition_score,
    onset_distance_min,
    datetime_60s,
    .by_group = TRUE
  ) %>%
  dplyr::slice(
    1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    Id,
    site,
    sleep_date,

    pim_sleep_onset_detected =
      datetime_60s,

    pim_sleep_onset_distance_min =
      onset_distance_min,

    pim_sleep_onset_transition_score =
      onset_transition_score,

    pim_sleep_onset_candidate_quality =
      onset_candidate_quality,

    pim_sleep_onset_following_sleep_score =
      onset_following_sleep_score,

    pim_sleep_onset_preceding_wake_score =
      onset_preceding_wake_score
  )

pim_sleep_onset <- pim_sleep_windows %>%
  dplyr::left_join(
    pim_onset_transition_detected,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::mutate(
    pim_sleep_onset_search_start =
      sleep_start - lubridate::minutes(30),

    pim_sleep_onset_search_end =
      sleep_start + lubridate::minutes(90),

    pim_sleep_onset =
      dplyr::coalesce(
        pim_sleep_onset_detected,
        sleep_start
      ),

    pim_sleep_onset_source =
      dplyr::case_when(
        !is.na(pim_sleep_onset_detected) &
          pim_sleep_onset_candidate_quality == "transition" ~
          "transition_detected",

        !is.na(pim_sleep_onset_detected) ~
          "sustained_sleep_near_diary",

        TRUE ~
          "diary_sleep_start_fallback"
      ),

    pim_sleep_onset_confidence =
      dplyr::case_when(
        pim_sleep_onset_source == "transition_detected" &
          pim_sleep_onset_distance_min <= 30 ~
          "high",

        pim_sleep_onset_source %in%
          c(
            "transition_detected",
            "sustained_sleep_near_diary"
          ) ~
          "medium",

        TRUE ~
          "fallback"
      )
  ) %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    pim_sleep_onset,
    pim_sleep_onset_detected,
    pim_sleep_onset_source,
    pim_sleep_onset_confidence,
    pim_sleep_onset_search_start,
    pim_sleep_onset_search_end,
    pim_sleep_onset_distance_min,
    pim_sleep_onset_transition_score,
    pim_sleep_onset_candidate_quality,
    pim_sleep_onset_following_sleep_score,
    pim_sleep_onset_preceding_wake_score
  )

# ------------------------------------------------------------
# STEP 12a.4: Select diary-anchored PIM-refined sleep offset
# ------------------------------------------------------------
# PURPOSE:
# Select the best sleep-to-wake transition candidate near the diary
# wake_time. If no candidate is detected, fall back to diary wake_time
# and flag the source.
#
# CANDIDATE WINDOW:
# wake_time - 90 minutes to wake_time + 60 minutes, capped at
# out_ofbed_time where out_ofbed_time is available and later than
# wake_time.
#
# CANDIDATE SCORE:
# Lower scores indicate better candidates. The score favours:
# - closeness to diary wake_time,
# - stable wake-like epochs after the candidate,
# - sleep evidence before the candidate.
# ------------------------------------------------------------

pim_offset_transition_candidates <- pim_sleep_epoch_candidates %>%
  dplyr::mutate(
    offset_search_start =
      wake_time - lubridate::minutes(90),

    offset_search_default_end =
      wake_time + lubridate::minutes(60),

    offset_search_end =
      dplyr::case_when(
        !is.na(out_ofbed_time) &
          out_ofbed_time > wake_time ~
          pmin(
            out_ofbed_time,
            offset_search_default_end
          ),

        TRUE ~
          offset_search_default_end
      )
  ) %>%
  dplyr::filter(
    datetime_60s >= offset_search_start,
    datetime_60s <= offset_search_end,
    sustained_wake_10min_timing
  ) %>%
  dplyr::mutate(
    offset_distance_min =
      abs(
        as.numeric(
          difftime(
            datetime_60s,
            wake_time,
            units = "mins"
          )
        )
      ),

    offset_following_wake_score =
      dplyr::coalesce(
        prop_wake_next_15_timing,
        prop_wake_next_10_timing,
        0
      ),

    offset_preceding_sleep_score =
      dplyr::coalesce(
        prop_sleep_prev_30_timing,
        prop_sleep_prev_15_timing,
        0
      ),

    offset_transition_score =
      offset_distance_min -
      15 * offset_following_wake_score -
      10 * offset_preceding_sleep_score,

    offset_candidate_quality =
      dplyr::case_when(
        offset_following_wake_score >= 0.80 &
          offset_preceding_sleep_score >= 0.70 ~
          "transition",

        offset_following_wake_score >= 0.80 ~
          "sustained_wake_only",

        TRUE ~
          "weak"
      )
  )

pim_offset_transition_detected <- pim_offset_transition_candidates %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::arrange(
    offset_transition_score,
    offset_distance_min,
    datetime_60s,
    .by_group = TRUE
  ) %>%
  dplyr::slice(
    1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    Id,
    site,
    sleep_date,

    pim_sleep_offset_detected =
      datetime_60s,

    pim_sleep_offset_distance_min =
      offset_distance_min,

    pim_sleep_offset_transition_score =
      offset_transition_score,

    pim_sleep_offset_candidate_quality =
      offset_candidate_quality,

    pim_sleep_offset_following_wake_score =
      offset_following_wake_score,

    pim_sleep_offset_preceding_sleep_score =
      offset_preceding_sleep_score
  )

pim_sleep_offset <- pim_sleep_windows %>%
  dplyr::mutate(
    pim_sleep_offset_search_start =
      wake_time - lubridate::minutes(90),

    pim_sleep_offset_search_default_end =
      wake_time + lubridate::minutes(60),

    pim_sleep_offset_search_end =
      dplyr::case_when(
        !is.na(out_ofbed_time) &
          out_ofbed_time > wake_time ~
          pmin(
            out_ofbed_time,
            pim_sleep_offset_search_default_end
          ),

        TRUE ~
          pim_sleep_offset_search_default_end
      )
  ) %>%
  dplyr::left_join(
    pim_offset_transition_detected,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::mutate(
    pim_sleep_offset =
      dplyr::coalesce(
        pim_sleep_offset_detected,
        wake_time
      ),

    pim_sleep_offset_source =
      dplyr::case_when(
        !is.na(pim_sleep_offset_detected) &
          pim_sleep_offset_candidate_quality == "transition" ~
          "transition_detected",

        !is.na(pim_sleep_offset_detected) ~
          "sustained_wake_near_diary",

        TRUE ~
          "diary_wake_fallback"
      ),

    pim_sleep_offset_confidence =
      dplyr::case_when(
        pim_sleep_offset_source == "transition_detected" &
          pim_sleep_offset_distance_min <= 30 ~
          "high",

        pim_sleep_offset_source %in%
          c(
            "transition_detected",
            "sustained_wake_near_diary"
          ) ~
          "medium",

        TRUE ~
          "fallback"
      )
  ) %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    pim_sleep_offset,
    pim_sleep_offset_detected,
    pim_sleep_offset_source,
    pim_sleep_offset_confidence,
    pim_sleep_offset_search_start,
    pim_sleep_offset_search_end,
    pim_sleep_offset_distance_min,
    pim_sleep_offset_transition_score,
    pim_sleep_offset_candidate_quality,
    pim_sleep_offset_following_wake_score,
    pim_sleep_offset_preceding_sleep_score
  )

# ------------------------------------------------------------
# STEP 12a.5: Summarise wrist-PIM sleep variables per night
# ------------------------------------------------------------
# PURPOSE:
# Create one row per participant-night with diary-anchored PIM
# timing, sleep/wake amount and sleep/wake scoring diagnostics.
# ------------------------------------------------------------

pim_sleep_night <- pim_sleep_epoch_candidates %>%
  dplyr::left_join(
    pim_immobility_onset_unconstrained,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_sleep_onset,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_sleep_offset,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::filter(
    !is.na(pim_sleep_onset),
    !is.na(pim_sleep_offset),
    pim_sleep_offset > pim_sleep_onset,
    datetime_60s >= pim_sleep_onset,
    datetime_60s < pim_sleep_offset
  ) %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::summarise(
    sleepprep_time =
      dplyr::first(
        sleepprep_time
      ),

    sleep_start =
      dplyr::first(
        sleep_start
      ),

    wake_time =
      dplyr::first(
        wake_time
      ),

    out_ofbed_time =
      dplyr::first(
        out_ofbed_time
      ),

    pim_immobility_onset_unconstrained =
      dplyr::first(
        pim_immobility_onset_unconstrained
      ),

    pim_sleep_onset =
      dplyr::first(
        pim_sleep_onset
      ),

    pim_sleep_onset_detected =
      dplyr::first(
        pim_sleep_onset_detected
      ),

    pim_sleep_onset_source =
      dplyr::first(
        pim_sleep_onset_source
      ),

    pim_sleep_onset_confidence =
      dplyr::first(
        pim_sleep_onset_confidence
      ),

    pim_sleep_onset_search_start =
      dplyr::first(
        pim_sleep_onset_search_start
      ),

    pim_sleep_onset_search_end =
      dplyr::first(
        pim_sleep_onset_search_end
      ),

    pim_sleep_onset_distance_min =
      dplyr::first(
        pim_sleep_onset_distance_min
      ),

    pim_sleep_onset_transition_score =
      dplyr::first(
        pim_sleep_onset_transition_score
      ),

    pim_sleep_onset_candidate_quality =
      dplyr::first(
        pim_sleep_onset_candidate_quality
      ),

    pim_sleep_onset_following_sleep_score =
      dplyr::first(
        pim_sleep_onset_following_sleep_score
      ),

    pim_sleep_onset_preceding_wake_score =
      dplyr::first(
        pim_sleep_onset_preceding_wake_score
      ),

    pim_sleep_offset =
      dplyr::first(
        pim_sleep_offset
      ),

    pim_sleep_offset_detected =
      dplyr::first(
        pim_sleep_offset_detected
      ),

    pim_sleep_offset_source =
      dplyr::first(
        pim_sleep_offset_source
      ),

    pim_sleep_offset_confidence =
      dplyr::first(
        pim_sleep_offset_confidence
      ),

    pim_sleep_offset_search_start =
      dplyr::first(
        pim_sleep_offset_search_start
      ),

    pim_sleep_offset_search_end =
      dplyr::first(
        pim_sleep_offset_search_end
      ),

    pim_sleep_offset_distance_min =
      dplyr::first(
        pim_sleep_offset_distance_min
      ),

    pim_sleep_offset_transition_score =
      dplyr::first(
        pim_sleep_offset_transition_score
      ),

    pim_sleep_offset_candidate_quality =
      dplyr::first(
        pim_sleep_offset_candidate_quality
      ),

    pim_sleep_offset_following_wake_score =
      dplyr::first(
        pim_sleep_offset_following_wake_score
      ),

    pim_sleep_offset_preceding_sleep_score =
      dplyr::first(
        pim_sleep_offset_preceding_sleep_score
      ),

    pim_sleep_duration_min =
      sum(
        pim_ck_sleep_amount,
        na.rm = TRUE
      ),

    pim_waso_min =
      sum(
        !pim_ck_sleep_amount,
        na.rm = TRUE
      ),

    pim_mean_sleep_period =
      mean(
        PIM_60s,
        na.rm = TRUE
      ),

    pim_median_sleep_period =
      median(
        PIM_60s,
        na.rm = TRUE
      ),

    pim_mean_scored_sleep_epochs =
      mean(
        PIM_60s[pim_ck_sleep_amount],
        na.rm = TRUE
      ),

    pim_mean_scored_wake_epochs =
      mean(
        PIM_60s[!pim_ck_sleep_amount],
        na.rm = TRUE
      ),

    pim_zero_prop_scored_sleep_epochs =
      mean(
        PIM_60s[pim_ck_sleep_amount] == 0,
        na.rm = TRUE
      ),

    pim_zero_prop_scored_wake_epochs =
      mean(
        PIM_60s[!pim_ck_sleep_amount] == 0,
        na.rm = TRUE
      ),

    pim_prop_scored_sleep_epochs =
      mean(
        pim_ck_sleep_amount,
        na.rm = TRUE
      ),

    n_pim_epochs_sleep_period =
      dplyr::n(),

    n_pim_epochs_scored_sleep =
      sum(
        pim_ck_sleep_amount,
        na.rm = TRUE
      ),

    n_pim_epochs_scored_wake =
      sum(
        !pim_ck_sleep_amount,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  ) %>%
  dplyr::mutate(
    pim_sleep_onset_latency_min =
      as.numeric(
        difftime(
          pim_sleep_onset,
          sleepprep_time,
          units = "mins"
        )
      ),

    pim_immobility_onset_vs_diary_sleep_start_min =
      as.numeric(
        difftime(
          pim_immobility_onset_unconstrained,
          sleep_start,
          units = "mins"
        )
      ),

    pim_sleep_onset_vs_diary_sleep_start_min =
      as.numeric(
        difftime(
          pim_sleep_onset,
          sleep_start,
          units = "mins"
        )
      ),

    pim_sleep_offset_vs_diary_wake_min =
      as.numeric(
        difftime(
          pim_sleep_offset,
          wake_time,
          units = "mins"
        )
      ),

    pim_time_sleepprep_to_outofbed_min =
      as.numeric(
        difftime(
          out_ofbed_time,
          sleepprep_time,
          units = "mins"
        )
      ),

    pim_sleep_period_min =
      as.numeric(
        difftime(
          pim_sleep_offset,
          pim_sleep_onset,
          units = "mins"
        )
      ),

    pim_sleep_efficiency =
      pim_sleep_duration_min /
      pim_time_sleepprep_to_outofbed_min * 100,

    pim_sleep_efficiency_sleep_period =
      pim_sleep_duration_min /
      pim_sleep_period_min * 100,

    pim_sleep_midpoint =
      pim_sleep_onset +
      as.numeric(
        difftime(
          pim_sleep_offset,
          pim_sleep_onset,
          units = "secs"
        )
      ) / 2,

    pim_sleep_onset_latency_min =
      dplyr::if_else(
        pim_sleep_onset_latency_min >= 0 &
          pim_sleep_onset_latency_min <= 6 * 60,
        pim_sleep_onset_latency_min,
        NA_real_
      ),

    pim_sleep_efficiency =
      dplyr::if_else(
        pim_sleep_efficiency >= 0 &
          pim_sleep_efficiency <= 100,
        pim_sleep_efficiency,
        NA_real_
      ),

    pim_sleep_efficiency_sleep_period =
      dplyr::if_else(
        pim_sleep_efficiency_sleep_period >= 0 &
          pim_sleep_efficiency_sleep_period <= 100,
        pim_sleep_efficiency_sleep_period,
        NA_real_
      )
  ) %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    pim_immobility_onset_unconstrained,
    pim_sleep_onset,
    pim_sleep_onset_detected,
    pim_sleep_onset_source,
    pim_sleep_onset_confidence,
    pim_sleep_onset_search_start,
    pim_sleep_onset_search_end,
    pim_sleep_onset_distance_min,
    pim_sleep_onset_transition_score,
    pim_sleep_onset_candidate_quality,
    pim_sleep_onset_following_sleep_score,
    pim_sleep_onset_preceding_wake_score,
    pim_sleep_offset,
    pim_sleep_offset_detected,
    pim_sleep_offset_source,
    pim_sleep_offset_confidence,
    pim_sleep_offset_search_start,
    pim_sleep_offset_search_end,
    pim_sleep_offset_distance_min,
    pim_sleep_offset_transition_score,
    pim_sleep_offset_candidate_quality,
    pim_sleep_offset_following_wake_score,
    pim_sleep_offset_preceding_sleep_score,
    pim_sleep_onset_latency_min,
    pim_immobility_onset_vs_diary_sleep_start_min,
    pim_sleep_onset_vs_diary_sleep_start_min,
    pim_sleep_offset_vs_diary_wake_min,
    pim_sleep_duration_min,
    pim_waso_min,
    pim_sleep_period_min,
    pim_sleep_efficiency,
    pim_sleep_efficiency_sleep_period,
    pim_sleep_midpoint,
    pim_mean_sleep_period,
    pim_median_sleep_period,
    pim_mean_scored_sleep_epochs,
    pim_mean_scored_wake_epochs,
    pim_zero_prop_scored_sleep_epochs,
    pim_zero_prop_scored_wake_epochs,
    pim_prop_scored_sleep_epochs,
    n_pim_epochs_sleep_period,
    n_pim_epochs_scored_sleep,
    n_pim_epochs_scored_wake
  )

# ------------------------------------------------------------
# STEP 12b: Check wrist-PIM sleep scoring behaviour
# ------------------------------------------------------------
# PURPOSE:
# Check whether diary-anchored wrist-PIM sleep/wake scoring produces
# plausible night-level estimates.
# ------------------------------------------------------------

pim_sleep_scoring_check <- pim_sleep_night %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),

    pim_ck_threshold_timing =
      pim_ck_threshold_timing,

    pim_ck_threshold_sleep_amount =
      pim_ck_threshold_sleep_amount,

    median_sleep_onset_latency_min =
      median(
        pim_sleep_onset_latency_min,
        na.rm = TRUE
      ),

    median_sleep_duration_min =
      median(
        pim_sleep_duration_min,
        na.rm = TRUE
      ),

    median_waso_min =
      median(
        pim_waso_min,
        na.rm = TRUE
      ),

    median_sleep_period_min =
      median(
        pim_sleep_period_min,
        na.rm = TRUE
      ),

    median_sleep_efficiency =
      median(
        pim_sleep_efficiency,
        na.rm = TRUE
      ),

    median_sleep_efficiency_sleep_period =
      median(
        pim_sleep_efficiency_sleep_period,
        na.rm = TRUE
      ),

    median_prop_scored_sleep_epochs =
      median(
        pim_prop_scored_sleep_epochs,
        na.rm = TRUE
      ),

    median_zero_prop_scored_sleep_epochs =
      median(
        pim_zero_prop_scored_sleep_epochs,
        na.rm = TRUE
      ),

    median_zero_prop_scored_wake_epochs =
      median(
        pim_zero_prop_scored_wake_epochs,
        na.rm = TRUE
      ),

    median_mean_scored_sleep_epochs =
      median(
        pim_mean_scored_sleep_epochs,
        na.rm = TRUE
      ),

    median_mean_scored_wake_epochs =
      median(
        pim_mean_scored_wake_epochs,
        na.rm = TRUE
      ),

    n_onset_high_confidence =
      sum(
        pim_sleep_onset_confidence == "high",
        na.rm = TRUE
      ),

    n_onset_medium_confidence =
      sum(
        pim_sleep_onset_confidence == "medium",
        na.rm = TRUE
      ),

    n_onset_fallback =
      sum(
        pim_sleep_onset_confidence == "fallback",
        na.rm = TRUE
      ),

    n_offset_high_confidence =
      sum(
        pim_sleep_offset_confidence == "high",
        na.rm = TRUE
      ),

    n_offset_medium_confidence =
      sum(
        pim_sleep_offset_confidence == "medium",
        na.rm = TRUE
      ),

    n_offset_fallback =
      sum(
        pim_sleep_offset_confidence == "fallback",
        na.rm = TRUE
      )
  )

View(
  pim_sleep_scoring_check
)

# ------------------------------------------------------------
# STEP 12b.1: Compare PIM-derived and diary-derived sleep amount
# ------------------------------------------------------------
# PURPOSE:
# Check whether wrist-PIM sleep/wake scoring gives plausible sleep
# duration and efficiency compared with diary reports.
# ------------------------------------------------------------

sleep_amount_discrepancy_check <- pim_sleep_night %>%
  dplyr::left_join(
    sleep_diary %>%
      dplyr::select(
        Id,
        site,
        sleep_date,
        sleep_duration_min,
        sleep_efficiency_diary
      ),
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),

    median_diary_sleep_duration_min =
      median(
        sleep_duration_min,
        na.rm = TRUE
      ),

    median_pim_sleep_duration_min =
      median(
        pim_sleep_duration_min,
        na.rm = TRUE
      ),

    median_pim_minus_diary_sleep_duration_min =
      median(
        pim_sleep_duration_min - sleep_duration_min,
        na.rm = TRUE
      ),

    median_abs_pim_minus_diary_sleep_duration_min =
      median(
        abs(
          pim_sleep_duration_min - sleep_duration_min
        ),
        na.rm = TRUE
      ),

    median_diary_sleep_efficiency =
      median(
        sleep_efficiency_diary,
        na.rm = TRUE
      ),

    median_pim_sleep_efficiency =
      median(
        pim_sleep_efficiency,
        na.rm = TRUE
      ),

    median_pim_minus_diary_sleep_efficiency =
      median(
        pim_sleep_efficiency - sleep_efficiency_diary,
        na.rm = TRUE
      )
  )

View(
  sleep_amount_discrepancy_check
)

# ------------------------------------------------------------
# STEP 12b.2: Check retained and excluded wrist-PIM nights
# ------------------------------------------------------------
# PURPOSE:
# Check how many nights with wrist-PIM data in the diary sleep
# interval were retained after diary-anchored onset/offset refinement.
# ------------------------------------------------------------

pim_candidate_nights <- pim_sleep_epoch_candidates %>%
  dplyr::filter(
    datetime_60s >= sleep_start,
    datetime_60s < wake_time
  ) %>%
  dplyr::group_by(
    Id,
    site,
    sleep_date
  ) %>%
  dplyr::summarise(
    n_pim_epochs_diary_interval =
      dplyr::n(),

    .groups =
      "drop"
  )

pim_retained_nights <- pim_sleep_night %>%
  dplyr::distinct(
    Id,
    site,
    sleep_date
  )

pim_night_retention_check <- pim_candidate_nights %>%
  dplyr::mutate(
    has_pim_candidate_night =
      TRUE
  ) %>%
  dplyr::left_join(
    pim_retained_nights %>%
      dplyr::mutate(
        retained_in_pim_sleep_night =
          TRUE
      ),
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::mutate(
    retained_in_pim_sleep_night =
      dplyr::coalesce(
        retained_in_pim_sleep_night,
        FALSE
      )
  ) %>%
  dplyr::summarise(
    n_candidate_nights =
      dplyr::n(),

    n_retained_nights =
      sum(
        retained_in_pim_sleep_night,
        na.rm = TRUE
      ),

    n_excluded_nights =
      sum(
        !retained_in_pim_sleep_night,
        na.rm = TRUE
      ),

    prop_retained =
      mean(
        retained_in_pim_sleep_night,
        na.rm = TRUE
      )
  )

pim_missing_nights_check <- pim_candidate_nights %>%
  dplyr::anti_join(
    pim_retained_nights,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    sleep_diary %>%
      dplyr::select(
        Id,
        site,
        sleep_date,
        sleep_start,
        wake_time,
        sleepprep_time,
        out_ofbed_time,
        sleep_duration_min
      ),
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  )

View(
  pim_night_retention_check
)

View(
  pim_missing_nights_check
)

# ------------------------------------------------------------
# STEP 12b.3: Check sensitivity of wrist-PIM sleep/wake threshold
# ------------------------------------------------------------
# PURPOSE:
# Evaluate how strongly PIM-derived sleep duration depends on the
# Cole-Kripke-style threshold when scored within the diary-reported
# sleep interval.
# ------------------------------------------------------------

pim_threshold_sensitivity_check <- purrr::map_dfr(
  c(
    0.75,
    1.00,
    1.25,
    1.50,
    1.75,
    2.00,
    2.50,
    3.00,
    4.00,
    4.25,
    4.50,
    4.75,
    5.00
  ),
  function(threshold_value) {

    scored_tmp <- pim_sleep_epoch_candidates %>%
      dplyr::arrange(
        Id,
        site,
        sleep_date,
        datetime_60s
      ) %>%
      dplyr::group_by(
        Id,
        site
      ) %>%
      dplyr::mutate(
        pim_ck_sleep_tmp =
          score_cole_kripke_60s(
            activity =
              PIM_60s,
            threshold =
              threshold_value
          )
      ) %>%
      dplyr::ungroup()

    night_tmp <- scored_tmp %>%
      dplyr::filter(
        datetime_60s >= sleep_start,
        datetime_60s < wake_time
      ) %>%
      dplyr::group_by(
        Id,
        site,
        sleep_date
      ) %>%
      dplyr::summarise(
        pim_sleep_duration_tmp_min =
          sum(
            pim_ck_sleep_tmp,
            na.rm = TRUE
          ),

        pim_waso_tmp_min =
          sum(
            !pim_ck_sleep_tmp,
            na.rm = TRUE
          ),

        pim_prop_sleep_tmp =
          mean(
            pim_ck_sleep_tmp,
            na.rm = TRUE
          ),

        .groups =
          "drop"
      ) %>%
      dplyr::left_join(
        sleep_diary %>%
          dplyr::select(
            Id,
            site,
            sleep_date,
            sleep_duration_min,
            sleep_efficiency_diary
          ),
        by = c(
          "Id",
          "site",
          "sleep_date"
        )
      ) %>%
      dplyr::mutate(
        pim_minus_diary_sleep_duration_tmp_min =
          pim_sleep_duration_tmp_min - sleep_duration_min,

        abs_pim_minus_diary_sleep_duration_tmp_min =
          abs(
            pim_minus_diary_sleep_duration_tmp_min
          )
      )

    tibble::tibble(
      threshold =
        threshold_value,

      n_nights =
        nrow(
          night_tmp
        ),

      median_diary_sleep_duration_min =
        median(
          night_tmp$sleep_duration_min,
          na.rm = TRUE
        ),

      median_pim_sleep_duration_min =
        median(
          night_tmp$pim_sleep_duration_tmp_min,
          na.rm = TRUE
        ),

      median_pim_minus_diary_sleep_duration_min =
        median(
          night_tmp$pim_minus_diary_sleep_duration_tmp_min,
          na.rm = TRUE
        ),

      median_abs_pim_minus_diary_sleep_duration_min =
        median(
          night_tmp$abs_pim_minus_diary_sleep_duration_tmp_min,
          na.rm = TRUE
        ),

      median_pim_waso_min =
        median(
          night_tmp$pim_waso_tmp_min,
          na.rm = TRUE
        ),

      median_pim_prop_sleep =
        median(
          night_tmp$pim_prop_sleep_tmp,
          na.rm = TRUE
        )
    )
  }
)

View(
  pim_threshold_sensitivity_check
)

# ------------------------------------------------------------
# STEP 12c: Derive PIM-based interdaily stability
# ------------------------------------------------------------
# PURPOSE:
# Calculate interdaily stability from the continuous wrist-PIM time
# series.
# ------------------------------------------------------------

pim_interdaily_stability <- calculate_interdaily_stability(
  data = actimetry_wrist,
  id_col = "Id",
  site_col = "site",
  datetime_col = "datetime",
  activity_col = "PIM",
  epoch_minutes = 60
)


# ------------------------------------------------------------
# STEP 13: Read and aggregate nightly meteorological exposure
# ------------------------------------------------------------
# PURPOSE:
# Read hourly meteorological data from Finca2_variables.xlsx,
# calculate Stull wet-bulb temperature, save an updated Excel file,
# and assign nightly weather exposure to each sleep night.
#
# INPUT:
# Finca2_variables.xlsx with hourly records and the columns:
# - datetime: Costa Rica local clock time
# - temperature: air temperature in degrees Celsius
# - RH: relative humidity in percent
# - irradiance: optional, not used for nightly summaries here
# - precipitation: optional, not used for nightly summaries here
#
# OUTPUT:
# weather_hourly_raw
# weather_prepared
# weather_night
# weather_variable_check
# weather_night_check
# data/processed/Finca2_variables_with_wetbulb.xlsx
# output/preprocessing/08_weather_variable_check.csv
# output/preprocessing/09_weather_night_summary.csv
# output/preprocessing/10_weather_night_check.csv
#
# NIGHT WINDOW:
# sleep_date 22:00 to wake_date 07:00.
#
# IMPORTANT:
# The datetime column is treated as Costa Rica local clock time.
# It is not shifted from UTC or any other time zone.
# ------------------------------------------------------------

weather_file_candidates <- c(
  file.path(
    "C:/Users/chris/OneDrive/Documents/GitHub/MeLiDos_CB",
    "Finca2_variables.xlsx"
  ),
  "data/raw/Finca2_variables.xlsx",
  "data/Finca2_variables.xlsx",
  "input/Finca2_variables.xlsx",
  "Finca2_variables.xlsx"
)

weather_file_existing <- weather_file_candidates[
  file.exists(weather_file_candidates)
]

if (length(weather_file_existing) == 0) {
  stop(
    "Finca2_variables.xlsx was not found. Checked these locations: ",
    paste(weather_file_candidates, collapse = ", "),
    ". Current working directory: ",
    getwd(),
    call. = FALSE
  )
}

weather_file <- weather_file_existing[1]

weather_file_info <- file.info(
  weather_file
)

if (is.na(weather_file_info$size) || weather_file_info$size <= 0) {
  stop(
    "Finca2_variables.xlsx was found but appears to be empty: ",
    normalizePath(weather_file, winslash = "/", mustWork = FALSE),
    call. = FALSE
  )
}

# IMPORTANT:
# Do not pre-test the file with utils::unzip(). Some Excel-readable
# files fail this ZIP pre-test even though readxl can still read them.
# Instead, try readxl directly and report a clearer diagnostic if that
# fails.

weather_hourly_raw <- tryCatch(
  {
    readxl::read_excel(
      path = weather_file
    ) %>%
      tibble::as_tibble()
  },
  error = function(e) {
    stop(
      "Finca2_variables.xlsx was found but could not be read by readxl: ",
      normalizePath(weather_file, winslash = "/", mustWork = FALSE),
      "\n\nOriginal readxl error:\n",
      conditionMessage(e),
      "\n\nPlease close the file in Excel, then reopen it and use 'Save As' > ",
      "'Excel Workbook (*.xlsx)'. If the file came from an export system, ",
      "also check whether it is actually CSV or old .xls content with an .xlsx extension.",
      call. = FALSE
    )
  }
)

weather_datetime_col <- pick_col(
  data = weather_hourly_raw,
  candidates = c(
    "datetime",
    "date_time",
    "DateTime",
    "timestamp",
    "time",
    "Time"
  ),
  pattern = "datetime|date.*time|timestamp|time"
)

temperature_col <- pick_col(
  data = weather_hourly_raw,
  candidates = c(
    "temperature",
    "Temperature",
    "temperature_c",
    "temp_c",
    "Tair",
    "air_temperature"
  ),
  pattern = "temperature|temp|tair"
)

rh_col <- pick_col(
  data = weather_hourly_raw,
  candidates = c(
    "RH",
    "rh",
    "relative_humidity",
    "humidity"
  ),
  pattern = "^rh$|relative.*humidity|humidity"
)

irradiance_col <- pick_col_optional(
  data = weather_hourly_raw,
  candidates = c(
    "irradiance",
    "Irradiance",
    "global_irradiance",
    "solar_irradiance"
  ),
  pattern = "irradiance"
)

precipitation_col <- pick_col_optional(
  data = weather_hourly_raw,
  candidates = c(
    "precipitation",
    "Precipitation",
    "rain",
    "rainfall"
  ),
  pattern = "precip|rain"
)

weather_datetime_raw <- weather_hourly_raw[[weather_datetime_col]]

weather_datetime_local <- NULL

if (inherits(weather_datetime_raw, "POSIXt")) {
  weather_datetime_local <- as.POSIXct(
    format(
      weather_datetime_raw,
      format = "%Y-%m-%d %H:%M:%S"
    ),
    tz = analysis_tz
  )
} else if (inherits(weather_datetime_raw, "Date")) {
  weather_datetime_local <- as.POSIXct(
    weather_datetime_raw,
    tz = analysis_tz
  )
} else if (is.numeric(weather_datetime_raw)) {
  weather_datetime_local <- as.POSIXct(
    format(
      openxlsx::convertToDateTime(
        weather_datetime_raw
      ),
      format = "%Y-%m-%d %H:%M:%S"
    ),
    tz = analysis_tz
  )
} else {
  weather_datetime_local <- lubridate::parse_date_time(
    as.character(
      weather_datetime_raw
    ),
    orders = c(
      "Ymd HMS",
      "Ymd HM",
      "Ymd H",
      "dmY HMS",
      "dmY HM",
      "dmY H",
      "mdY HMS",
      "mdY HM",
      "mdY H"
    ),
    tz = analysis_tz
  )
}

weather_prepared_all <- weather_hourly_raw %>%
  dplyr::mutate(
    site =
      "UCR",
    
    datetime =
      weather_datetime_local,
    
    temperature_c =
      as.numeric(
        .data[[temperature_col]]
      ),
    
    rh_percent =
      as.numeric(
        .data[[rh_col]]
      ),
    
    rh_percent =
      dplyr::if_else(
        !is.na(rh_percent) &
          rh_percent <= 1.5,
        rh_percent * 100,
        rh_percent
      ),
    
    twb_stull_c =
      calc_twb_stull(
        temp_c = temperature_c,
        rh_percent = rh_percent
      )
  )

weather_prepared <- weather_prepared_all %>%
  dplyr::filter(
    !is.na(datetime)
  )

weather_with_wetbulb_export <- weather_hourly_raw %>%
  dplyr::mutate(
    twb_stull_c =
      weather_prepared_all$twb_stull_c
  )

weather_with_wetbulb_file <- file.path(
  "data/processed",
  "Finca2_variables_with_wetbulb.xlsx"
)

openxlsx::write.xlsx(
  weather_with_wetbulb_export,
  file = weather_with_wetbulb_file,
  overwrite = TRUE
)

weather_variable_check <- tibble::tibble(
  weather_file =
    normalizePath(
      weather_file,
      winslash = "/",
      mustWork = FALSE
    ),
  
  weather_file_size_bytes =
    as.numeric(
      weather_file_info$size
    ),
  
  n_weather_rows_raw =
    nrow(
      weather_hourly_raw
    ),
  
  n_weather_rows_prepared =
    nrow(
      weather_prepared
    ),
  
  datetime_col =
    weather_datetime_col,
  
  temperature_col =
    temperature_col,
  
  rh_col =
    rh_col,
  
  irradiance_col =
    if (is.null(irradiance_col)) {
      NA_character_
    } else {
      irradiance_col
    },
  
  precipitation_col =
    if (is.null(precipitation_col)) {
      NA_character_
    } else {
      precipitation_col
    },
  
  first_weather_datetime =
    min(
      weather_prepared$datetime,
      na.rm = TRUE
    ),
  
  last_weather_datetime =
    max(
      weather_prepared$datetime,
      na.rm = TRUE
    ),
  
  median_temperature_c =
    median(
      weather_prepared$temperature_c,
      na.rm = TRUE
    ),
  
  median_rh_percent =
    median(
      weather_prepared$rh_percent,
      na.rm = TRUE
    ),
  
  median_twb_stull_c =
    median(
      weather_prepared$twb_stull_c,
      na.rm = TRUE
    )
)

write_csv_safely(
  data = weather_variable_check,
  path = "output/preprocessing/08_weather_variable_check.csv"
)

min_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    min(
      x,
      na.rm = TRUE
    )
  }
}

max_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(
      x,
      na.rm = TRUE
    )
  }
}

weather_dt <- data.table::as.data.table(
  weather_prepared
)

data.table::setkey(
  weather_dt,
  site,
  datetime
)

weather_night <- weather_dt[
  windows_dt,
  on = .(
    site,
    datetime >= night_start_fixed,
    datetime < night_end_fixed
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
][
  ,
  .(
    n_weather_records_night =
      .N,
    
    n_temperature_records_night =
      sum(
        !is.na(temperature_c)
      ),
    
    n_rh_records_night =
      sum(
        !is.na(rh_percent)
      ),
    
    n_twb_records_night =
      sum(
        !is.na(twb_stull_c)
      ),
    
    temperature_mean_night =
      mean(
        temperature_c,
        na.rm = TRUE
      ),
    
    temperature_median_night =
      median(
        temperature_c,
        na.rm = TRUE
      ),
    
    temperature_min_night =
      min_or_na(
        temperature_c
      ),
    
    temperature_max_night =
      max_or_na(
        temperature_c
      ),
    
    rh_mean_night =
      mean(
        rh_percent,
        na.rm = TRUE
      ),
    
    rh_median_night =
      median(
        rh_percent,
        na.rm = TRUE
      ),
    
    rh_min_night =
      min_or_na(
        rh_percent
      ),
    
    rh_max_night =
      max_or_na(
        rh_percent
      ),
    
    twb_mean_night =
      mean(
        twb_stull_c,
        na.rm = TRUE
      ),
    
    twb_median_night =
      median(
        twb_stull_c,
        na.rm = TRUE
      ),
    
    twb_min_night =
      min_or_na(
        twb_stull_c
      ),
    
    twb_max_night =
      max_or_na(
        twb_stull_c
      )
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
] %>%
  tibble::as_tibble()

weather_night_check <- weather_night %>%
  dplyr::summarise(
    n_nights_with_weather =
      dplyr::n(),
    
    min_weather_records_night =
      min(
        n_weather_records_night,
        na.rm = TRUE
      ),
    
    median_weather_records_night =
      median(
        n_weather_records_night,
        na.rm = TRUE
      ),
    
    max_weather_records_night =
      max(
        n_weather_records_night,
        na.rm = TRUE
      ),
    
    median_temperature_mean_night =
      median(
        temperature_mean_night,
        na.rm = TRUE
      ),
    
    median_rh_mean_night =
      median(
        rh_mean_night,
        na.rm = TRUE
      ),
    
    median_twb_mean_night =
      median(
        twb_mean_night,
        na.rm = TRUE
      )
  )

write_csv_safely(
  data = weather_night,
  path = "output/preprocessing/09_weather_night_summary.csv"
)

write_csv_safely(
  data = weather_night_check,
  path = "output/preprocessing/10_weather_night_check.csv"
)

View(
  weather_variable_check
)

View(
  weather_night_check
)

rm(
  weather_file_candidates,
  weather_file_existing,
  min_or_na,
  max_or_na
)

# ------------------------------------------------------------
# STEP 14: Create UCR person-night analysis dataset
# ------------------------------------------------------------
# PURPOSE:
# Combine diary-based sleep outcomes, mEDI exposure, PIM summaries
# and optional weather variables into one analysis dataset.
#
# INPUT:
# sleep_diary
# medi_day
# medi_evening
# pim_reported_sleep
# pim_fixed_night
# pim_sleep_night
# pim_interdaily_stability
# weather_night
#
# OUTPUT:
# analysis_sleep
#
# DATA STRUCTURE:
# One row per participant and sleep night.
# ------------------------------------------------------------

analysis_sleep <- sleep_diary %>%
  dplyr::select(
    Id,
    site,
    sleep_date,
    wake_date,
    sleep_start,
    wake_time,
    sleepprep_time,
    out_ofbed_time,
    sleep_duration_min,
    sleepdelay_min,
    awake_duration_min,
    time_sleepprep_to_wake_min,
    sleep_efficiency_diary,
    sleepquality,
    awakenings,
    daytype2
  ) %>%
  dplyr::left_join(
    medi_day,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    medi_evening,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_reported_sleep,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_fixed_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_sleep_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::left_join(
    pim_interdaily_stability,
    by = c(
      "Id",
      "site"
    )
  ) %>%
  dplyr::left_join(
    weather_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::filter(
    site == "UCR"
  ) %>%
  dplyr::mutate(
    pim_sleep_onset_vs_diary_sleep_start_min =
      as.numeric(
        difftime(
          pim_sleep_onset,
          sleep_start,
          units = "mins"
        )
      ),
    
    pim_sleep_onset_vs_diary_sleepprep_min =
      as.numeric(
        difftime(
          pim_sleep_onset,
          sleepprep_time,
          units = "mins"
        )
      ),
    
    pim_sleep_offset_vs_diary_wake_min =
      as.numeric(
        difftime(
          pim_sleep_offset,
          wake_time,
          units = "mins"
        )
      ),
    
    pim_sleep_offset_vs_diary_outofbed_min =
      as.numeric(
        difftime(
          pim_sleep_offset,
          out_ofbed_time,
          units = "mins"
        )
      ),
    
    pim_sleep_midpoint_vs_diary_midpoint_min =
      as.numeric(
        difftime(
          pim_sleep_midpoint,
          sleep_start +
            as.numeric(
              difftime(
                wake_time,
                sleep_start,
                units = "secs"
              )
            ) / 2,
          units = "mins"
        )
      ),
    
    abs_pim_sleep_onset_vs_diary_sleep_start_min =
      abs(
        pim_sleep_onset_vs_diary_sleep_start_min
      ),
    
    abs_pim_sleep_offset_vs_diary_wake_min =
      abs(
        pim_sleep_offset_vs_diary_wake_min
      ),
    
    abs_pim_sleep_midpoint_vs_diary_midpoint_min =
      abs(
        pim_sleep_midpoint_vs_diary_midpoint_min
      ),
    
    pim_sleep_onset_discrepancy_gt_30min =
      abs_pim_sleep_onset_vs_diary_sleep_start_min > 30,
    
    pim_sleep_offset_discrepancy_gt_30min =
      abs_pim_sleep_offset_vs_diary_wake_min > 30,
    
    pim_sleep_midpoint_discrepancy_gt_30min =
      abs_pim_sleep_midpoint_vs_diary_midpoint_min > 30,
    
    pim_sleep_onset_discrepancy_gt_60min =
      abs_pim_sleep_onset_vs_diary_sleep_start_min > 60,
    
    pim_sleep_offset_discrepancy_gt_60min =
      abs_pim_sleep_offset_vs_diary_wake_min > 60,
    
    pim_sleep_midpoint_discrepancy_gt_60min =
      abs_pim_sleep_midpoint_vs_diary_midpoint_min > 60
  )

# ------------------------------------------------------------
# STEP 14a: Check discrepancies between diary and PIM sleep timing
# ------------------------------------------------------------
# PURPOSE:
# Summarise differences between self-reported sleep timing and
# PIM-derived actigraphy sleep timing.
#
# INPUT:
# analysis_sleep
#
# OUTPUT:
# sleep_actigraphy_discrepancy_check
# sleep_actigraphy_discrepancy_by_id
#
# INTERPRETATION:
# Positive values indicate that PIM-derived timing is later than
# diary-reported timing.
# Negative values indicate that PIM-derived timing is earlier than
# diary-reported timing.
# ------------------------------------------------------------

sleep_actigraphy_discrepancy_check <- analysis_sleep %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),
    
    n_with_pim_sleep_onset =
      sum(
        !is.na(pim_sleep_onset)
      ),
    
    n_with_pim_sleep_offset =
      sum(
        !is.na(pim_sleep_offset)
      ),
    
    median_onset_difference_min =
      median(
        pim_sleep_onset_vs_diary_sleep_start_min,
        na.rm = TRUE
      ),
    
    median_abs_onset_difference_min =
      median(
        abs_pim_sleep_onset_vs_diary_sleep_start_min,
        na.rm = TRUE
      ),
    
    median_offset_difference_min =
      median(
        pim_sleep_offset_vs_diary_wake_min,
        na.rm = TRUE
      ),
    
    median_abs_offset_difference_min =
      median(
        abs_pim_sleep_offset_vs_diary_wake_min,
        na.rm = TRUE
      ),
    
    median_midpoint_difference_min =
      median(
        pim_sleep_midpoint_vs_diary_midpoint_min,
        na.rm = TRUE
      ),
    
    median_abs_midpoint_difference_min =
      median(
        abs_pim_sleep_midpoint_vs_diary_midpoint_min,
        na.rm = TRUE
      ),
    
    n_onset_discrepancy_gt_30min =
      sum(
        pim_sleep_onset_discrepancy_gt_30min,
        na.rm = TRUE
      ),
    
    n_offset_discrepancy_gt_30min =
      sum(
        pim_sleep_offset_discrepancy_gt_30min,
        na.rm = TRUE
      ),
    
    n_midpoint_discrepancy_gt_30min =
      sum(
        pim_sleep_midpoint_discrepancy_gt_30min,
        na.rm = TRUE
      ),
    
    n_onset_discrepancy_gt_60min =
      sum(
        pim_sleep_onset_discrepancy_gt_60min,
        na.rm = TRUE
      ),
    
    n_offset_discrepancy_gt_60min =
      sum(
        pim_sleep_offset_discrepancy_gt_60min,
        na.rm = TRUE
      ),
    
    n_midpoint_discrepancy_gt_60min =
      sum(
        pim_sleep_midpoint_discrepancy_gt_60min,
        na.rm = TRUE
      )
  )

sleep_actigraphy_discrepancy_by_id <- analysis_sleep %>%
  dplyr::group_by(
    Id
  ) %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),
    
    n_with_pim_sleep_onset =
      sum(
        !is.na(pim_sleep_onset)
      ),
    
    n_with_pim_sleep_offset =
      sum(
        !is.na(pim_sleep_offset)
      ),
    
    median_onset_difference_min =
      median(
        pim_sleep_onset_vs_diary_sleep_start_min,
        na.rm = TRUE
      ),
    
    median_offset_difference_min =
      median(
        pim_sleep_offset_vs_diary_wake_min,
        na.rm = TRUE
      ),
    
    median_midpoint_difference_min =
      median(
        pim_sleep_midpoint_vs_diary_midpoint_min,
        na.rm = TRUE
      ),
    
    median_abs_onset_difference_min =
      median(
        abs_pim_sleep_onset_vs_diary_sleep_start_min,
        na.rm = TRUE
      ),
    
    median_abs_offset_difference_min =
      median(
        abs_pim_sleep_offset_vs_diary_wake_min,
        na.rm = TRUE
      ),
    
    median_abs_midpoint_difference_min =
      median(
        abs_pim_sleep_midpoint_vs_diary_midpoint_min,
        na.rm = TRUE
      ),
    
    prop_onset_discrepancy_gt_30min =
      mean(
        pim_sleep_onset_discrepancy_gt_30min,
        na.rm = TRUE
      ),
    
    prop_offset_discrepancy_gt_30min =
      mean(
        pim_sleep_offset_discrepancy_gt_30min,
        na.rm = TRUE
      ),
    
    prop_onset_discrepancy_gt_60min =
      mean(
        pim_sleep_onset_discrepancy_gt_60min,
        na.rm = TRUE
      ),
    
    prop_offset_discrepancy_gt_60min =
      mean(
        pim_sleep_offset_discrepancy_gt_60min,
        na.rm = TRUE
      ),
    
    prop_onset_pim_later_than_diary_gt_30min =
      mean(
        pim_sleep_onset_vs_diary_sleep_start_min > 30,
        na.rm = TRUE
      ),
    
    prop_onset_pim_earlier_than_diary_gt_30min =
      mean(
        pim_sleep_onset_vs_diary_sleep_start_min < -30,
        na.rm = TRUE
      ),
    
    prop_offset_pim_later_than_diary_gt_30min =
      mean(
        pim_sleep_offset_vs_diary_wake_min > 30,
        na.rm = TRUE
      ),
    
    prop_offset_pim_earlier_than_diary_gt_30min =
      mean(
        pim_sleep_offset_vs_diary_wake_min < -30,
        na.rm = TRUE
      ),
    
    prop_onset_pim_later_than_diary_gt_60min =
      mean(
        pim_sleep_onset_vs_diary_sleep_start_min > 60,
        na.rm = TRUE
      ),
    
    prop_onset_pim_earlier_than_diary_gt_60min =
      mean(
        pim_sleep_onset_vs_diary_sleep_start_min < -60,
        na.rm = TRUE
      ),
    
    prop_offset_pim_later_than_diary_gt_60min =
      mean(
        pim_sleep_offset_vs_diary_wake_min > 60,
        na.rm = TRUE
      ),
    
    prop_offset_pim_earlier_than_diary_gt_60min =
      mean(
        pim_sleep_offset_vs_diary_wake_min < -60,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )

View(
  sleep_actigraphy_discrepancy_check
)

View(
  sleep_actigraphy_discrepancy_by_id # median_onset_difference_min > 0 = PIM-derived onset later than sleep diary // median_onset_difference_min < 0 = PIM-derived onset earlier than sleep diary
)

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   sleep_actigraphy_discrepancy_check,
#   "output/preprocessing/04_sleep_actigraphy_discrepancy_check.csv"
# )
#
# readr::write_csv(
#   sleep_actigraphy_discrepancy_by_id,
#   "output/preprocessing/04_sleep_actigraphy_discrepancy_by_id.csv"
# )



# ------------------------------------------------------------
# STEP 14b: Save diary fallback counts for PIM timing detection
# ------------------------------------------------------------
# PURPOSE:
# Count and save how often diary-anchored PIM onset/offset detection
# used the diary-reported time as fallback.
#
# INPUT:
# analysis_sleep
#
# OUTPUT:
# pim_timing_fallback_check
# pim_timing_fallback_by_id
# output/preprocessing/07_pim_timing_fallback_check.csv
# output/preprocessing/08_pim_timing_fallback_by_id.csv
#
# INTERPRETATION:
# A diary fallback means that no sufficiently stable PIM-based timing
# refinement was detected within the restricted diary-anchored search
# window, so the corresponding diary time was retained.
# ------------------------------------------------------------

pim_timing_fallback_check <- analysis_sleep %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),
    
    n_with_pim_sleep_onset =
      sum(
        !is.na(pim_sleep_onset)
      ),
    
    n_with_pim_sleep_offset =
      sum(
        !is.na(pim_sleep_offset)
      ),
    
    n_onset_pim_detected =
      sum(
        pim_sleep_onset_source != "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    n_onset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    prop_onset_diary_fallback =
      mean(
        pim_sleep_onset_source == "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    n_offset_pim_detected =
      sum(
        pim_sleep_offset_source != "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_offset_diary_fallback =
      sum(
        pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    prop_offset_diary_fallback =
      mean(
        pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_both_onset_and_offset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback" &
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    prop_both_onset_and_offset_diary_fallback =
      mean(
        pim_sleep_onset_source == "diary_sleep_start_fallback" &
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_either_onset_or_offset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback" |
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    prop_either_onset_or_offset_diary_fallback =
      mean(
        pim_sleep_onset_source == "diary_sleep_start_fallback" |
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      )
  )

pim_timing_fallback_by_id <- analysis_sleep %>%
  dplyr::group_by(
    Id
  ) %>%
  dplyr::summarise(
    n_nights =
      dplyr::n(),
    
    n_onset_pim_detected =
      sum(
        pim_sleep_onset_source != "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    n_onset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    prop_onset_diary_fallback =
      mean(
        pim_sleep_onset_source == "diary_sleep_start_fallback",
        na.rm = TRUE
      ),
    
    n_offset_pim_detected =
      sum(
        pim_sleep_offset_source != "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_offset_diary_fallback =
      sum(
        pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    prop_offset_diary_fallback =
      mean(
        pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_both_onset_and_offset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback" &
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    n_either_onset_or_offset_diary_fallback =
      sum(
        pim_sleep_onset_source == "diary_sleep_start_fallback" |
          pim_sleep_offset_source == "diary_wake_fallback",
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )

View(
  pim_timing_fallback_check
)

View(
  pim_timing_fallback_by_id
)

readr::write_csv(
  pim_timing_fallback_check,
  "output/preprocessing/07_pim_timing_fallback_check.csv"
)

readr::write_csv(
  pim_timing_fallback_by_id,
  "output/preprocessing/08_pim_timing_fallback_by_id.csv"
)

# ------------------------------------------------------------
# STEP 15: Check and save the prepared dataset
# ------------------------------------------------------------
# PURPOSE:
# Summarise the final analysis dataset, check availability of the
# most important variables and save the prepared dataset.
#
# INPUT:
# analysis_sleep
#
# OUTPUT:
# analysis_sleep_check
# data/processed/analysis_sleep_ucr_person_night.csv
# data/processed/analysis_sleep_ucr_person_night.rds
# ------------------------------------------------------------

analysis_sleep_check <- analysis_sleep %>%
  dplyr::summarise(
    n_rows =
      dplyr::n(),

    n_ids =
      dplyr::n_distinct(Id),

    n_sites =
      dplyr::n_distinct(site),

    sites =
      paste(unique(site), collapse = ", "),

    first_sleep_date =
      min(sleep_date, na.rm = TRUE),

    last_sleep_date =
      max(sleep_date, na.rm = TRUE),

    n_with_sleep_efficiency =
      sum(!is.na(sleep_efficiency_diary)),

    n_with_medi_day =
      sum(!is.na(medi_mean_day)),

    n_with_medi_evening =
      sum(!is.na(medi_mean_evening)),

    n_with_pim_reported_sleep =
      sum(!is.na(pim_mean_reported_sleep)),

    n_with_pim_fixed_night =
      sum(!is.na(pim_mean_fixed_night)),
    
    n_with_pim_sleep_onset =
      sum(!is.na(pim_sleep_onset)),
    
    n_with_pim_sleep_efficiency =
      sum(!is.na(pim_sleep_efficiency)),
    
    n_with_pim_interdaily_stability =
      sum(!is.na(pim_interdaily_stability)),

    n_with_weather =
      sum(!is.na(n_weather_records_night)),

    n_with_temperature =
      sum(!is.na(temperature_mean_night)),

    n_with_rh =
      sum(!is.na(rh_mean_night)),

    n_with_twb =
      sum(!is.na(twb_mean_night))
  )

# Uncomment once the script runs successfully and you want to save checks.
# readr::write_csv(
#   analysis_sleep_check,
#   "output/preprocessing/03_analysis_sleep_check.csv"
# )

analysis_sleep_check

readr::write_csv(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.csv"
)

saveRDS(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.rds"
)


# ------------------------------------------------------------
# STEP 16: Clean up temporary objects
# ------------------------------------------------------------
# PURPOSE:
# Remove intermediate helper objects that are no longer needed.
#
# INPUT:
# Temporary objects from preprocessing.
#
# OUTPUT:
# Cleaner R environment.
#
# NOTE:
# data_list, sleep_diary_raw, wearlog_raw, light_chest_raw and
# light_wrist_raw are intentionally kept in memory for now.
# ------------------------------------------------------------

rm(
  packages_needed,
  packages_missing,
  functions_file,
  required_functions,
  missing_functions,
  modalities,
  selected_light_columns,
  selected_wrist_columns,
  light_dt,
  wrist_dt,
  windows_dt,
  pim_60s_dt,
  sleep_windows_pim_dt,
  pim_sleep_windows,
  pim_60s,
  pim_sleep_epoch_candidates,
  pim_immobility_onset_unconstrained,
  pim_onset_transition_candidates,
  pim_onset_transition_detected,
  pim_offset_transition_candidates,
  pim_offset_transition_detected,
  pim_sleep_onset,
  pim_sleep_offset,
  pim_candidate_nights,
  pim_retained_nights
)

if (exists("weather_dt")) {
  rm(weather_dt)
}

if (exists("weather_prepared")) {
  rm(weather_prepared)
}

if (exists("weather_updated")) {
  rm(weather_updated)
}

if (exists("weather_raw")) {
  rm(weather_raw)
}

if (exists("weather_datetime_raw")) {
  rm(weather_datetime_raw)
}

if (exists("weather_datetime_local")) {
  rm(weather_datetime_local)
}

if (exists("weather_file_candidates")) {
  rm(weather_file_candidates)
}

if (exists("weather_file")) {
  rm(weather_file)
}

gc()
