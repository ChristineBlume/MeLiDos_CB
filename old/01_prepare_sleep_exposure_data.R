# 01_prepare_sleep_exposure_data.R
# Purpose: Prepare UCR person-night data for analyses of sleep, daytime mEDI,
# PIM-based actimetry candidates, and optional nightly weather exposure.
#
# Note: GGIR should be run separately if raw ActLumus acceleration files are
# available. PIM is prepared here as the primary actimetry candidate.

# 1) Load required packages ---------------------------------------------------

packages_needed <- c(
  "melidosData",
  "data.table",
  "dplyr",
  "lubridate",
  "purrr",
  "readr",
  "rlang",
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
  lapply(packages_needed, library, character.only = TRUE)
)

# 2) Define analysis settings -------------------------------------------------

site <- "UCR"
analysis_tz <- "America/Costa_Rica"

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

# 3) Define helper functions --------------------------------------------------
# 01_prepare_sleep_exposure_data.R

# ============================================================
# PREPROCESSING: UCR sleep diary, mEDI, PIM and weather data
# ============================================================
# PURPOSE:
# Prepare a person-night dataset for analyses of:
# - diary-based sleep outcomes,
# - diary-based sleep efficiency,
# - daytime and evening mEDI exposure for the following night,
# - PIM as the primary actimetry candidate,
# - optional nightly outdoor temperature and wet-bulb temperature.
#
# IMPORTANT:
# This script is restricted to UCR data only.
#
# NOTE ON ACTIMETRY:
# PIM is prepared here as the primary actimetry candidate.
# These PIM summaries are not yet GGIR-derived sleep estimates.
# If raw ActLumus accelerometer files are available, GGIR should be
# run separately to extract actigraphy-based sleep timing, sleep duration
# and sleep efficiency.
#
# MAIN OUTPUT:
# data/processed/analysis_sleep_ucr_person_night.csv
# data/processed/analysis_sleep_ucr_person_night.rds
# ============================================================


# ------------------------------------------------------------
# SETUP: Load required packages
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
  "rlang",
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
# SETUP: Define analysis settings and output folders
# ------------------------------------------------------------
# PURPOSE:
# Define the study site, time zone and output folders.
#
# INPUT:
# None.
#
# OUTPUT:
# site
# analysis_tz
# data/processed/
# output/preprocessing/
# ------------------------------------------------------------

site <- "UCR"
analysis_tz <- "America/Costa_Rica"

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
# SOURCE Functions for preprocessing
# ------------------------------------------------------------

source("C:/Users/chris/OneDrive/Documents/GitHub/MeLiDos_CB/Functions_MeLiDos_CB.R")

# ------------------------------------------------------------
# LOAD: UCR datasets
# ------------------------------------------------------------
# PURPOSE:
# Load only the UCR datasets needed for this preprocessing script.
# This block is intentionally strict:
# - it overwrites any previous data_list object,
# - it checks whether each dataset was loaded,
# - it stops immediately if light_chest is missing.
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
#
# DATASETS:
# sleepdiaries: sleep timing and diary-based sleep outcomes.
# wearlog:      device wear information.
# light_chest:  chest-level light data used in the previous HTML analysis.
# ------------------------------------------------------------

site <- "UCR"

modalities <- c(
  "sleepdiaries",
  "wearlog",
  "light_chest"
)

load_melidos_flat_strict <- function(
    modality_value,
    site_value,
    tz_value
) {
  
  message(
    "Loading modality: ",
    modality_value,
    " | site: ",
    site_value
  )
  
  data_raw <- tryCatch(
    melidosData::load_data(
      modality = modality_value,
      site = site_value
    ),
    error = function(e) {
      stop(
        "Could not load modality '",
        modality_value,
        "' for site '",
        site_value,
        "'. Original error message: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  if (inherits(data_raw, "melidos_data")) {
    data_raw <- melidosData::flatten_data(
      data_raw,
      tz = tz_value
    )
  }
  
  data_raw <- tibble::as_tibble(data_raw)
  
  if (!"site" %in% names(data_raw)) {
    data_raw <- data_raw %>%
      dplyr::mutate(
        site = site_value
      )
  }
  
  data_raw
}

# Overwrite any previously existing data_list.
# This avoids accidentally using an older object from a previous run.

data_list <- modalities %>%
  rlang::set_names() %>%
  purrr::map(
    ~ load_melidos_flat_strict(
      modality_value = .x,
      site_value = site,
      tz_value = "UTC"
    )
  )

# Check which modalities were actually loaded.

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

print(
  loaded_modalities_check
)

# readr::write_csv(
#   loaded_modalities_check,
#   "output/preprocessing/00_loaded_modalities_check.csv"
# )

# Stop if light_chest is still missing.

if (!"light_chest" %in% names(data_list)) {
  stop(
    "light_chest was not loaded. Check the output of loaded_modalities_check.",
    call. = FALSE
  )
}

# Extract raw datasets.

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

# ------------------------------------------------------------
# CHECK: Confirm that only UCR data were loaded
# ------------------------------------------------------------
# PURPOSE:
# Check that the raw datasets contain only one site and that
# this site is UCR.
#
# INPUT:
# sleep_diary_raw
# wearlog_raw
# light_chest_raw
#
# OUTPUT:
# ucr_site_check
# output/preprocessing/00_ucr_site_check.csv
# ------------------------------------------------------------

ucr_site_check <- tibble::tibble(
  dataset = c(
    "sleepdiaries",
    "wearlog",
    "light_chest"
  ),
  n_sites = c(
    dplyr::n_distinct(sleep_diary_raw$site),
    dplyr::n_distinct(wearlog_raw$site),
    dplyr::n_distinct(light_chest_raw$site)
  ),
  sites = c(
    paste(unique(sleep_diary_raw$site), collapse = ", "),
    paste(unique(wearlog_raw$site), collapse = ", "),
    paste(unique(light_chest_raw$site), collapse = ", ")
  )
)

# readr::write_csv(
#   ucr_site_check,
#   "output/preprocessing/00_ucr_site_check.csv"
# )

ucr_site_check


# ------------------------------------------------------------
# FUNCTION: Combine a date with a clock time
# ------------------------------------------------------------
# PURPOSE:
# Reconstruct a POSIXct timestamp by combining:
# - a date that defines the calendar day,
# - the clock time from another date-time variable.
#
# INPUT:
# date_value:  Date to be used as the calendar date.
# time_source: Variable from which the clock time is extracted.
# tz_value:    Target time zone.
#
# OUTPUT:
# POSIXct timestamp in the target time zone.
#
# USE CASE:
# The sleep diary time stamps appear to be anchored to the wake/report
# date. Therefore, we reconstruct sleep and wake timestamps from
# the appropriate analysis dates and the reported clock times.
# ------------------------------------------------------------

combine_date_with_clock_time <- function(
    date_value,
    time_source,
    tz_value
) {
  
  time_source <- as_posix_analysis_tz(
    time_source,
    tz_value
  )
  
  date_start <- as.POSIXct(
    date_value,
    tz = tz_value
  )
  
  date_start +
    lubridate::hours(lubridate::hour(time_source)) +
    lubridate::minutes(lubridate::minute(time_source)) +
    lubridate::seconds(lubridate::second(time_source))
}


# ------------------------------------------------------------
# FUNCTION: Assign sleep-night clock times to the correct date
# ------------------------------------------------------------
# PURPOSE:
# Reconstruct timestamps for sleep-night variables that can occur
# either before or after midnight.
#
# INPUT:
# time_source: Variable containing the reported clock time.
# sleep_date:  Date on which the sleep night began.
# wake_date:   Date on which the participant woke up.
# tz_value:    Target time zone.
# cutoff_hour: Clock times before this hour are assigned to wake_date.
#
# OUTPUT:
# POSIXct timestamp in the target time zone.
#
# LOGIC:
# - Times before cutoff_hour, for example 00:30, are assigned to wake_date.
# - Times from cutoff_hour onwards, for example 23:15, are assigned to sleep_date.
#
# EXAMPLE:
# wake_date  = Tuesday
# sleep_date = Monday
#
# sleep = 23:30 -> Monday 23:30
# sleep = 00:30 -> Tuesday 00:30
# ------------------------------------------------------------

assign_sleep_night_time <- function(
    time_source,
    sleep_date,
    wake_date,
    tz_value,
    cutoff_hour = 12
) {
  
  time_source_posix <- as_posix_analysis_tz(
    time_source,
    tz_value
  )
  
  assigned_date <- dplyr::if_else(
    lubridate::hour(time_source_posix) < cutoff_hour,
    wake_date,
    sleep_date
  )
  
  combine_date_with_clock_time(
    date_value = assigned_date,
    time_source = time_source_posix,
    tz_value = tz_value
  )
}


# ------------------------------------------------------------
# PREPARE: Diary-based sleep outcomes
# ------------------------------------------------------------
# PURPOSE:
# Prepare sleep diary variables for person-night analyses.
# This includes:
# - reconstructing sleep-night timestamps,
# - defining wake_date from the wake time,
# - defining sleep_date as the day before wake_date,
# - converting duration variables to minutes,
# - calculating diary-based sleep efficiency.
#
# INPUT:
# sleep_diary_raw
#
# OUTPUT:
# sleep_diary
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
# wake_time_diary_min
# time_in_bed_diary_min
# sleep_efficiency_diary
#
# DATE LOGIC:
# wake_date is the calendar date of waking.
# sleep_date is the day before wake_date and is used as the night label.
#
# SLEEP EFFICIENCY FORMULA:
# sleep_efficiency_diary =
#   sleep_duration /
#   (sleep_duration + sleepdelay + awake_duration) * 100
# ------------------------------------------------------------

sleep_diary <- sleep_diary_raw %>%
  dplyr::mutate(
    wake_time_tmp =
      as_posix_analysis_tz(
        wake,
        analysis_tz
      ),
    
    wake_date =
      as.Date(
        wake_time_tmp
      ),
    
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
    
    wake_time_diary_min =
      sleepdelay_min + awake_duration_min,
    
    time_in_bed_diary_min =
      sleep_duration_min + wake_time_diary_min,
    
    sleep_efficiency_diary =
      sleep_duration_min / time_in_bed_diary_min * 100,
    
    sleep_efficiency_diary =
      dplyr::if_else(
        sleep_efficiency_diary >= 0 & sleep_efficiency_diary <= 100,
        sleep_efficiency_diary,
        NA_real_
      )
  ) %>%
  dplyr::select(
    -wake_time_tmp
  )

# ------------------------------------------------------------
# CHECK: Sleep-night date reconstruction
# ------------------------------------------------------------
# PURPOSE:
# Verify that sleep_date is always the day before wake_date and
# that reconstructed sleep_start occurs before wake_time.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# sleep_date_check
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
    
    min_sleep_start =
      min(sleep_start, na.rm = TRUE),
    
    max_sleep_start =
      max(sleep_start, na.rm = TRUE),
    
    min_wake_time =
      min(wake_time, na.rm = TRUE),
    
    max_wake_time =
      max(wake_time, na.rm = TRUE)
  )

# readr::write_csv(
#   sleep_date_check,
#   "output/preprocessing/01b_sleep_date_check.csv"
# )

View(sleep_date_check)

# ------------------------------------------------------------
# CHECK: Diary duration variables
# ------------------------------------------------------------
# PURPOSE:
# Check the derived minute-based sleep duration variables.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# sleep_duration_unit_check
# ------------------------------------------------------------

sleep_duration_unit_check <- sleep_diary %>%
  dplyr::summarise(
    median_sleep_duration_raw =
      median(sleep_duration, na.rm = TRUE),
    
    median_sleep_duration_min =
      median(sleep_duration_min, na.rm = TRUE),
    
    median_sleepdelay_min =
      median(sleepdelay_min, na.rm = TRUE),
    
    median_awake_duration_min =
      median(awake_duration_min, na.rm = TRUE),
    
    median_time_in_bed_diary_min =
      median(time_in_bed_diary_min, na.rm = TRUE),
    
    median_sleep_efficiency_diary =
      median(sleep_efficiency_diary, na.rm = TRUE)
  )

View(sleep_duration_unit_check)

# ------------------------------------------------------------
# CHECK: Diary-based sleep efficiency
# ------------------------------------------------------------
# PURPOSE:
# Check whether the derived diary-based sleep efficiency variable
# has plausible values and how much data are missing.
#
# INPUT:
# sleep_diary
#
# OUTPUT:
# sleep_diary_check
# output/preprocessing/01_sleep_diary_check.csv
# ------------------------------------------------------------

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
    
    n_missing_sleep_efficiency =
      sum(is.na(sleep_efficiency_diary)),
    
    min_sleep_efficiency =
      min(sleep_efficiency_diary, na.rm = TRUE),
    
    median_sleep_efficiency =
      median(sleep_efficiency_diary, na.rm = TRUE),
    
    max_sleep_efficiency =
      max(sleep_efficiency_diary, na.rm = TRUE)
  )

# readr::write_csv(
#   sleep_diary_check,
#   "output/preprocessing/01_sleep_diary_check.csv"
# )

sleep_diary_check


# ------------------------------------------------------------
# PREPARE: Sleep and exposure windows
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
#   Fixed night window for temperature exposure.
#
# medi_day_start to medi_day_end:
#   Daytime mEDI exposure on the day before the sleep night.
#
# medi_evening_start to medi_evening_end:
#   Four-hour evening mEDI exposure window before sleep preparation
#   or sleep onset.
# ------------------------------------------------------------

sleep_windows <- sleep_diary %>%
  dplyr::transmute(
    Id,
    site,
    sleep_date,
    wake_date,
    sleep_start,
    wake_time,
    sleepprep_time,
    out_ofbed_time,
    
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
    
    medi_day_end =
      dplyr::coalesce(
        sleepprep_time,
        sleep_start,
        as.POSIXct(
          paste0(sleep_date, " 23:59:59"),
          tz = analysis_tz
        )
      ),
    
    medi_evening_start =
      dplyr::coalesce(
        sleepprep_time,
        sleep_start
      ) - lubridate::hours(4),
    
    medi_evening_end =
      dplyr::coalesce(
        sleepprep_time,
        sleep_start
      )
  ) %>%
  dplyr::filter(
    !is.na(Id),
    !is.na(site),
    !is.na(sleep_date),
    !is.na(wake_date),
    wake_time > sleep_start
  )


# ------------------------------------------------------------
# IDENTIFY: mEDI and PIM columns in light_chest
# ------------------------------------------------------------
# PURPOSE:
# Identify the date-time column, the mEDI column and the PIM column
# in the chest-level light data.
#
# INPUT:
# light_chest_raw
#
# OUTPUT:
# light_datetime_col
# medi_col
# pim_col
# light_variable_check
# output/preprocessing/02_light_variable_check.csv
#
# NOTE:
# PIM is optional because it may not be included in the currently
# loaded light_chest dataset.
# ------------------------------------------------------------

light_datetime_col <- pick_col(
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

pim_col <- pick_col_optional(
  data = light_chest_raw,
  candidates = c(
    "PIM",
    "pim"
  ),
  pattern = "^pim$"
)

light_variable_check <- tibble::tibble(
  light_datetime_col = light_datetime_col,
  medi_col = medi_col,
  pim_col = pim_col
)

# readr::write_csv(
#   light_variable_check,
#   "output/preprocessing/02_light_variable_check.csv"
# )

light_variable_check


# ------------------------------------------------------------
# PREPARE: Chest-level light data
# ------------------------------------------------------------
# PURPOSE:
# Keep only the columns needed for later aggregation:
# - participant ID,
# - site,
# - timestamp,
# - mEDI,
# - PIM if available.
#
# INPUT:
# light_chest_raw
# light_datetime_col
# medi_col
# pim_col
#
# OUTPUT:
# light_chest
# ------------------------------------------------------------

selected_light_columns <- unique(
  stats::na.omit(
    c(
      "Id",
      "site",
      light_datetime_col,
      medi_col,
      pim_col
    )
  )
)

light_chest <- light_chest_raw %>%
  dplyr::select(
    dplyr::all_of(selected_light_columns)
  ) %>%
  dplyr::rename(
    datetime =
      dplyr::all_of(light_datetime_col),
    
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

if (!is.na(pim_col)) {
  
  light_chest <- light_chest %>%
    dplyr::rename(
      PIM =
        dplyr::all_of(pim_col)
    ) %>%
    dplyr::mutate(
      PIM =
        as.numeric(PIM)
    )
  
} else {
  
  light_chest <- light_chest %>%
    dplyr::mutate(
      PIM =
        NA_real_
    )
  
  message(
    "No PIM column found in light_chest. PIM aggregation will return missing values."
  )
}


# ------------------------------------------------------------
# AGGREGATE: Daytime and evening mEDI
# ------------------------------------------------------------
# PURPOSE:
# Aggregate mEDI exposure for each person-night.
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


# # ------------------------------------------------------------
# # AGGREGATE: PIM during sleep and night windows
# # ------------------------------------------------------------
# # PURPOSE:
# # Aggregate PIM as the primary actimetry candidate.
# #
# # INPUT:
# # light_chest
# # sleep_windows
# #
# # OUTPUT:
# # pim_reported_sleep
# # pim_fixed_night
# #
# # IMPORTANT:
# # These are movement summaries, not GGIR-derived sleep estimates.
# #
# # WINDOWS:
# # reported sleep:
# #   sleep_start to wake_time, based on diary reports.
# #
# # fixed night:
# #   22:00 to 07:00, based on calendar time.
# # ------------------------------------------------------------
# 
# pim_reported_sleep <- light_dt[
#   windows_dt,
#   on = .(
#     Id,
#     site,
#     datetime >= sleep_start,
#     datetime < wake_time
#   ),
#   nomatch = 0L,
#   allow.cartesian = TRUE
# ][
#   ,
#   .(
#     n_pim_records_reported_sleep =
#       .N,
#     
#     pim_mean_reported_sleep =
#       mean(PIM, na.rm = TRUE),
#     
#     pim_median_reported_sleep =
#       median(PIM, na.rm = TRUE),
#     
#     pim_sum_reported_sleep =
#       sum(PIM, na.rm = TRUE),
#     
#     pim_max_reported_sleep =
#       max(PIM, na.rm = TRUE),
#     
#     pim_zero_prop_reported_sleep =
#       mean(PIM == 0, na.rm = TRUE)
#   ),
#   by = .(
#     Id,
#     site,
#     sleep_date
#   )
# ]
# 
# pim_fixed_night <- light_dt[
#   windows_dt,
#   on = .(
#     Id,
#     site,
#     datetime >= night_start_fixed,
#     datetime < night_end_fixed
#   ),
#   nomatch = 0L,
#   allow.cartesian = TRUE
# ][
#   ,
#   .(
#     n_pim_records_fixed_night =
#       .N,
#     
#     pim_mean_fixed_night =
#       mean(PIM, na.rm = TRUE),
#     
#     pim_median_fixed_night =
#       median(PIM, na.rm = TRUE),
#     
#     pim_sum_fixed_night =
#       sum(PIM, na.rm = TRUE),
#     
#     pim_max_fixed_night =
#       max(PIM, na.rm = TRUE),
#     
#     pim_zero_prop_fixed_night =
#       mean(PIM == 0, na.rm = TRUE)
#   ),
#   by = .(
#     Id,
#     site,
#     sleep_date
#   )
# ]
# 
# pim_reported_sleep <- tibble::as_tibble(pim_reported_sleep)
# pim_fixed_night <- tibble::as_tibble(pim_fixed_night)


# ------------------------------------------------------------
# OPTIONAL: Prepare nightly weather exposure
# ------------------------------------------------------------
# PURPOSE:
# If an object called weather_hourly exists, calculate nightly
# air temperature and wet-bulb temperature summaries.
#
# INPUT:
# weather_hourly, if available.
#
# EXPECTED WEATHER VARIABLES:
# - timestamp/date-time,
# - air temperature,
# - relative humidity.
#
# OUTPUT:
# weather_night
#
# NOTE:
# If weather_hourly does not exist, an empty weather_night object
# is created so that the later joins still work.
# ------------------------------------------------------------

if (exists("weather_hourly")) {
  
  weather_datetime_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "datetime",
      "timestamp",
      "time",
      "Time",
      "DateTime"
    ),
    pattern = "datetime|timestamp|time",
    object_name = "weather_hourly"
  )
  
  temperature_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "Temperature",
      "temperature",
      "temperature_c",
      "temp_c",
      "Tair"
    ),
    pattern = "temp|temperature",
    object_name = "weather_hourly"
  )
  
  rh_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "RH",
      "rh",
      "relative_humidity",
      "humidity"
    ),
    pattern = "rh|relative.*humidity|humidity",
    object_name = "weather_hourly"
  )
  
  weather_prepared <- weather_hourly %>%
    dplyr::rename(
      datetime =
        dplyr::all_of(weather_datetime_col),
      
      temperature_c =
        dplyr::all_of(temperature_col),
      
      rh_percent =
        dplyr::all_of(rh_col)
    ) %>%
    dplyr::mutate(
      site =
        if ("site" %in% names(weather_hourly)) {
          as.character(site)
        } else {
          "UCR"
        },
      
      datetime =
        as_posix_analysis_tz(
          datetime,
          analysis_tz
        ),
      
      temperature_c =
        as.numeric(temperature_c),
      
      rh_percent =
        as.numeric(rh_percent),
      
      twb_stull_c =
        calc_twb_stull(
          temp_c = temperature_c,
          rh_percent = rh_percent
        )
    ) %>%
    dplyr::filter(
      site == "UCR"
    )
  
  weather_dt <- data.table::as.data.table(weather_prepared)
  
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
      
      tmean_night =
        mean(temperature_c, na.rm = TRUE),
      
      tmin_night =
        min(temperature_c, na.rm = TRUE),
      
      tmax_night =
        max(temperature_c, na.rm = TRUE),
      
      rh_mean_night =
        mean(rh_percent, na.rm = TRUE),
      
      twb_mean_night =
        mean(twb_stull_c, na.rm = TRUE),
      
      twb_max_night =
        max(twb_stull_c, na.rm = TRUE)
    ),
    by = .(
      Id,
      site,
      sleep_date
    )
  ]
  
  weather_night <- tibble::as_tibble(weather_night)
  
} else {
  
  weather_night <- tibble::tibble(
    Id = character(),
    site = character(),
    sleep_date = as.Date(character()),
    n_weather_records_night = integer(),
    tmean_night = numeric(),
    tmin_night = numeric(),
    tmax_night = numeric(),
    rh_mean_night = numeric(),
    twb_mean_night = numeric(),
    twb_max_night = numeric()
  )
  
  message(
    "weather_hourly not found. Nightly weather aggregation was skipped."
  )
}


# ------------------------------------------------------------
# CREATE: Person-night analysis dataset
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
    wake_time_diary_min,
    time_in_bed_diary_min,
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
    weather_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) %>%
  dplyr::filter(
    site == "UCR"
  )


# ------------------------------------------------------------
# CHECK: Final person-night dataset
# ------------------------------------------------------------
# PURPOSE:
# Summarise the final analysis dataset and check availability of
# the most important variables.
#
# INPUT:
# analysis_sleep
#
# OUTPUT:
# analysis_sleep_check
# output/preprocessing/03_analysis_sleep_check.csv
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
    
    n_with_temperature =
      sum(!is.na(tmean_night)),
    
    n_with_twb =
      sum(!is.na(twb_mean_night))
  )

# readr::write_csv(
#   analysis_sleep_check,
#   "output/preprocessing/03_analysis_sleep_check.csv"
# )

analysis_sleep_check


# ------------------------------------------------------------
# SAVE: Final analysis dataset
# ------------------------------------------------------------
# PURPOSE:
# Save the prepared person-night dataset in two formats:
# - CSV for inspection and sharing,
# - RDS for preserving R-specific classes.
#
# INPUT:
# analysis_sleep
#
# OUTPUT:
# data/processed/analysis_sleep_ucr_person_night.csv
# data/processed/analysis_sleep_ucr_person_night.rds
# ------------------------------------------------------------

readr::write_csv(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.csv"
)

saveRDS(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.rds"
)


# ------------------------------------------------------------
# CLEANUP: Remove temporary objects
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
# data_list, sleep_diary_raw, wearlog_raw and light_chest_raw are
# intentionally kept in memory for now.
# ------------------------------------------------------------

rm(
  packages_needed,
  packages_missing,
  modalities,
  selected_light_columns,
  light_dt,
  windows_dt
)

if (exists("weather_dt")) {
  rm(weather_dt)
}

gc()
  
  stop(
    "No matching column found in ",
    object_name,
    ". Checked candidates: ",
    paste(candidates, collapse = ", ")
  )
}

pick_col_optional <- function(data, candidates, pattern = NULL) {
  
  data_names <- names(data)
  
  exact_match <- candidates[
    candidates %in% data_names
  ]
  
  if (length(exact_match) > 0) {
    return(exact_match[1])
  }
  
  if (!is.null(pattern)) {
    
    pattern_match <- data_names[
      stringr::str_detect(
        stringr::str_to_lower(data_names),
        pattern
      )
    ]
    
    if (length(pattern_match) > 0) {
      return(pattern_match[1])
    }
  }
  
  NA_character_
}

as_posix_analysis_tz <- function(x, tz_value) {
  
  if (inherits(x, "POSIXt")) {
    return(
      lubridate::with_tz(
        x,
        tzone = tz_value
      )
    )
  }
  
  lubridate::ymd_hms(
    x,
    tz = tz_value,
    quiet = TRUE
  )
}

to_minutes_if_needed <- function(x, variable_name) {
  
  if (inherits(x, "difftime")) {
    return(
      as.numeric(
        x,
        units = "mins"
      )
    )
  }
  
  x <- as.numeric(x)
  
  x_median <- median(
    x,
    na.rm = TRUE
  )
  
  if (is.na(x_median)) {
    return(x)
  }
  
  if (x_median <= 24) {
    
    message(
      variable_name,
      " appears to be in hours and is converted to minutes."
    )
    
    return(x * 60)
  }
  
  x
}

calc_twb_stull <- function(temp_c, rh_percent) {
  
  temp_c * atan(
    0.151977 * sqrt(rh_percent + 8.313659)
  ) +
    atan(temp_c + rh_percent) -
    atan(rh_percent - 1.676331) +
    0.00391838 * rh_percent^1.5 * atan(0.023101 * rh_percent) -
    4.686035
}

# 4) Load UCR datasets --------------------------------------------------------
# Existing objects are overwritten to avoid accidentally using all-site data.

modalities <- c(
  "sleepdiaries",
  "wearlog",
  "light_chest"
)

data_list <- modalities |>
  rlang::set_names() |>
  purrr::map(
    ~ load_melidos_flat(
      modality_value = .x,
      site_value = site,
      tz_value = "UTC"
    )
  )

sleep_diary_raw <- data_list[["sleepdiaries"]] |>
  dplyr::filter(
    site == "UCR"
  )

wearlog_raw <- data_list[["wearlog"]] |>
  dplyr::filter(
    site == "UCR"
  )

light_chest_raw <- data_list[["light_chest"]] |>
  dplyr::filter(
    site == "UCR"
  )

ucr_site_check <- tibble::tibble(
  dataset = c(
    "sleepdiaries",
    "wearlog",
    "light_chest"
  ),
  n_sites = c(
    dplyr::n_distinct(sleep_diary_raw$site),
    dplyr::n_distinct(wearlog_raw$site),
    dplyr::n_distinct(light_chest_raw$site)
  ),
  sites = c(
    paste(unique(sleep_diary_raw$site), collapse = ", "),
    paste(unique(wearlog_raw$site), collapse = ", "),
    paste(unique(light_chest_raw$site), collapse = ", ")
  )
)

readr::write_csv(
  ucr_site_check,
  "output/preprocessing/00_ucr_site_check.csv"
)

ucr_site_check

# 5) Prepare diary-based sleep outcomes ---------------------------------------
# Diary-based sleep efficiency is calculated as:
# sleep_duration / (sleep_duration + sleepdelay + awake_duration) * 100.

sleep_diary <- sleep_diary_raw |>
  dplyr::mutate(
    sleep_start = as_posix_analysis_tz(
      sleep,
      analysis_tz
    ),
    wake_time = as_posix_analysis_tz(
      wake,
      analysis_tz
    ),
    sleepprep_time = as_posix_analysis_tz(
      sleepprep,
      analysis_tz
    ),
    out_ofbed_time = as_posix_analysis_tz(
      out_ofbed,
      analysis_tz
    ),
    sleep_date = as.Date(sleep_start),
    wake_date = as.Date(wake_time)
  ) |>
  dplyr::mutate(
    sleep_duration_min = to_minutes_if_needed(
      sleep_duration,
      "sleep_duration"
    ),
    sleepdelay_min = to_minutes_if_needed(
      sleepdelay,
      "sleepdelay"
    ),
    awake_duration_min = to_minutes_if_needed(
      awake_duration,
      "awake_duration"
    ),
    wake_time_diary_min = sleepdelay_min + awake_duration_min,
    time_in_bed_diary_min = sleep_duration_min + wake_time_diary_min,
    sleep_efficiency_diary = sleep_duration_min /
      time_in_bed_diary_min * 100
  ) |>
  dplyr::mutate(
    sleep_efficiency_diary = dplyr::if_else(
      sleep_efficiency_diary >= 0 & sleep_efficiency_diary <= 100,
      sleep_efficiency_diary,
      NA_real_
    )
  )

sleep_diary_check <- sleep_diary |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_ids = dplyr::n_distinct(Id),
    n_sites = dplyr::n_distinct(site),
    sites = paste(unique(site), collapse = ", "),
    first_sleep_date = min(sleep_date, na.rm = TRUE),
    last_sleep_date = max(sleep_date, na.rm = TRUE),
    n_missing_sleep_duration = sum(is.na(sleep_duration_min)),
    n_missing_sleep_efficiency = sum(is.na(sleep_efficiency_diary)),
    min_sleep_efficiency = min(sleep_efficiency_diary, na.rm = TRUE),
    median_sleep_efficiency = median(sleep_efficiency_diary, na.rm = TRUE),
    max_sleep_efficiency = max(sleep_efficiency_diary, na.rm = TRUE)
  )

readr::write_csv(
  sleep_diary_check,
  "output/preprocessing/01_sleep_diary_check.csv"
)

sleep_diary_check

# 6) Define sleep and exposure windows ----------------------------------------
# mEDI is assigned to the day before the night.
# The primary temperature window is a fixed night window.

sleep_windows <- sleep_diary |>
  dplyr::transmute(
    Id,
    site,
    sleep_date,
    wake_date,
    sleep_start,
    wake_time,
    sleepprep_time,
    out_ofbed_time,
    night_start_fixed = as.POSIXct(
      paste0(sleep_date, " 22:00:00"),
      tz = analysis_tz
    ),
    night_end_fixed = as.POSIXct(
      paste0(wake_date, " 07:00:00"),
      tz = analysis_tz
    ),
    medi_day_start = as.POSIXct(
      paste0(sleep_date, " 06:00:00"),
      tz = analysis_tz
    ),
    medi_day_end = dplyr::coalesce(
      sleepprep_time,
      sleep_start,
      as.POSIXct(
        paste0(sleep_date, " 23:59:59"),
        tz = analysis_tz
      )
    ),
    medi_evening_start = dplyr::coalesce(
      sleepprep_time,
      sleep_start
    ) - lubridate::hours(4),
    medi_evening_end = dplyr::coalesce(
      sleepprep_time,
      sleep_start
    )
  ) |>
  dplyr::filter(
    !is.na(Id),
    !is.na(site),
    !is.na(sleep_date),
    !is.na(wake_date),
    wake_time > sleep_start
  )

# 7) Identify light, mEDI, and PIM columns ------------------------------------
# PIM is used as the primary actimetry candidate if present.

light_datetime_col <- pick_col(
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

pim_col <- pick_col_optional(
  data = light_chest_raw,
  candidates = c(
    "PIM",
    "pim"
  ),
  pattern = "^pim$"
)

light_variable_check <- tibble::tibble(
  light_datetime_col = light_datetime_col,
  medi_col = medi_col,
  pim_col = pim_col
)

readr::write_csv(
  light_variable_check,
  "output/preprocessing/02_light_variable_check.csv"
)

light_variable_check

selected_light_columns <- unique(
  stats::na.omit(
    c(
      "Id",
      "site",
      light_datetime_col,
      medi_col,
      pim_col
    )
  )
)

light_chest <- light_chest_raw |>
  dplyr::select(
    dplyr::all_of(selected_light_columns)
  ) |>
  dplyr::rename(
    datetime = dplyr::all_of(light_datetime_col),
    mEDI = dplyr::all_of(medi_col)
  ) |>
  dplyr::mutate(
    datetime = as_posix_analysis_tz(
      datetime,
      analysis_tz
    ),
    mEDI = as.numeric(mEDI)
  )

if (!is.na(pim_col)) {
  
  light_chest <- light_chest |>
    dplyr::rename(
      PIM = dplyr::all_of(pim_col)
    ) |>
    dplyr::mutate(
      PIM = as.numeric(PIM)
    )
  
} else {
  
  light_chest <- light_chest |>
    dplyr::mutate(
      PIM = NA_real_
    )
  
  message(
    "No PIM column found in light_chest. PIM aggregation will return missing values."
  )
}

# 8) Aggregate daytime and evening mEDI ---------------------------------------
# The mEDI exposure belongs to the day before the following night.

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
    n_medi_records_day = .N,
    medi_mean_day = mean(mEDI, na.rm = TRUE),
    medi_median_day = median(mEDI, na.rm = TRUE),
    medi_logmean_day = mean(log10(mEDI + 0.1), na.rm = TRUE)
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
    n_medi_records_evening = .N,
    medi_mean_evening = mean(mEDI, na.rm = TRUE),
    medi_median_evening = median(mEDI, na.rm = TRUE),
    medi_logmean_evening = mean(log10(mEDI + 0.1), na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

medi_day <- tibble::as_tibble(medi_day)
medi_evening <- tibble::as_tibble(medi_evening)

# 9) Aggregate PIM during reported sleep and fixed night windows --------------
# These summaries are not yet GGIR-derived sleep estimates.
# They describe movement based on the PIM signal.

pim_reported_sleep <- light_dt[
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
    n_pim_records_reported_sleep = .N,
    pim_mean_reported_sleep = mean(PIM, na.rm = TRUE),
    pim_median_reported_sleep = median(PIM, na.rm = TRUE),
    pim_sum_reported_sleep = sum(PIM, na.rm = TRUE),
    pim_max_reported_sleep = max(PIM, na.rm = TRUE),
    pim_zero_prop_reported_sleep = mean(PIM == 0, na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

pim_fixed_night <- light_dt[
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
    n_pim_records_fixed_night = .N,
    pim_mean_fixed_night = mean(PIM, na.rm = TRUE),
    pim_median_fixed_night = median(PIM, na.rm = TRUE),
    pim_sum_fixed_night = sum(PIM, na.rm = TRUE),
    pim_max_fixed_night = max(PIM, na.rm = TRUE),
    pim_zero_prop_fixed_night = mean(PIM == 0, na.rm = TRUE)
  ),
  by = .(
    Id,
    site,
    sleep_date
  )
]

pim_reported_sleep <- tibble::as_tibble(pim_reported_sleep)
pim_fixed_night <- tibble::as_tibble(pim_fixed_night)

# 10) Prepare optional nightly weather exposure -------------------------------
# This block runs only if an object called weather_hourly exists.
# Expected content: site, datetime, air temperature, and relative humidity.

if (exists("weather_hourly")) {
  
  weather_datetime_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "datetime",
      "timestamp",
      "time",
      "Time",
      "DateTime"
    ),
    pattern = "datetime|timestamp|time",
    object_name = "weather_hourly"
  )
  
  temperature_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "Temperature",
      "temperature",
      "temperature_c",
      "temp_c",
      "Tair"
    ),
    pattern = "temp|temperature",
    object_name = "weather_hourly"
  )
  
  rh_col <- pick_col(
    data = weather_hourly,
    candidates = c(
      "RH",
      "rh",
      "relative_humidity",
      "humidity"
    ),
    pattern = "rh|relative.*humidity|humidity",
    object_name = "weather_hourly"
  )
  
  weather_prepared <- weather_hourly |>
    dplyr::rename(
      datetime = dplyr::all_of(weather_datetime_col),
      temperature_c = dplyr::all_of(temperature_col),
      rh_percent = dplyr::all_of(rh_col)
    ) |>
    dplyr::mutate(
      site = dplyr::if_else(
        "site" %in% names(weather_hourly),
        as.character(site),
        "UCR"
      ),
      datetime = as_posix_analysis_tz(
        datetime,
        analysis_tz
      ),
      temperature_c = as.numeric(temperature_c),
      rh_percent = as.numeric(rh_percent),
      twb_stull_c = calc_twb_stull(
        temp_c = temperature_c,
        rh_percent = rh_percent
      )
    ) |>
    dplyr::filter(
      site == "UCR"
    )
  
  weather_dt <- data.table::as.data.table(weather_prepared)
  
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
      n_weather_records_night = .N,
      tmean_night = mean(temperature_c, na.rm = TRUE),
      tmin_night = min(temperature_c, na.rm = TRUE),
      tmax_night = max(temperature_c, na.rm = TRUE),
      rh_mean_night = mean(rh_percent, na.rm = TRUE),
      twb_mean_night = mean(twb_stull_c, na.rm = TRUE),
      twb_max_night = max(twb_stull_c, na.rm = TRUE)
    ),
    by = .(
      Id,
      site,
      sleep_date
    )
  ]
  
  weather_night <- tibble::as_tibble(weather_night)
  
} else {
  
  weather_night <- tibble::tibble(
    Id = character(),
    site = character(),
    sleep_date = as.Date(character()),
    n_weather_records_night = integer(),
    tmean_night = numeric(),
    tmin_night = numeric(),
    tmax_night = numeric(),
    rh_mean_night = numeric(),
    twb_mean_night = numeric(),
    twb_max_night = numeric()
  )
  
  message(
    "weather_hourly not found. Nightly weather aggregation was skipped."
  )
}

# 11) Create UCR person-night analysis dataset --------------------------------

analysis_sleep <- sleep_diary |>
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
    wake_time_diary_min,
    time_in_bed_diary_min,
    sleep_efficiency_diary,
    sleepquality,
    awakenings,
    daytype2
  ) |>
  dplyr::left_join(
    medi_day,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) |>
  dplyr::left_join(
    medi_evening,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) |>
  dplyr::left_join(
    pim_reported_sleep,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) |>
  dplyr::left_join(
    pim_fixed_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) |>
  dplyr::left_join(
    weather_night,
    by = c(
      "Id",
      "site",
      "sleep_date"
    )
  ) |>
  dplyr::filter(
    site == "UCR"
  )

# 12) Check and save the prepared dataset -------------------------------------

analysis_sleep_check <- analysis_sleep |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_ids = dplyr::n_distinct(Id),
    n_sites = dplyr::n_distinct(site),
    sites = paste(unique(site), collapse = ", "),
    first_sleep_date = min(sleep_date, na.rm = TRUE),
    last_sleep_date = max(sleep_date, na.rm = TRUE),
    n_with_sleep_efficiency = sum(!is.na(sleep_efficiency_diary)),
    n_with_medi_day = sum(!is.na(medi_mean_day)),
    n_with_medi_evening = sum(!is.na(medi_mean_evening)),
    n_with_pim_reported_sleep = sum(!is.na(pim_mean_reported_sleep)),
    n_with_pim_fixed_night = sum(!is.na(pim_mean_fixed_night)),
    n_with_temperature = sum(!is.na(tmean_night)),
    n_with_twb = sum(!is.na(twb_mean_night))
  )

readr::write_csv(
  analysis_sleep_check,
  "output/preprocessing/03_analysis_sleep_check.csv"
)

readr::write_csv(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.csv"
)

saveRDS(
  analysis_sleep,
  "data/processed/analysis_sleep_ucr_person_night.rds"
)

analysis_sleep_check

# 13) Clean up temporary objects ----------------------------------------------
# The original data_list and raw UCR datasets are kept in memory intentionally.

rm(
  packages_needed,
  packages_missing,
  modalities,
  selected_light_columns,
  light_dt,
  windows_dt
)

if (exists("weather_dt")) {
  rm(weather_dt)
}

gc()