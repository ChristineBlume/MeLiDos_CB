# ------------------------------------------------------------
# FUNCTION: Load and flatten MeLiDos data
# ------------------------------------------------------------
# PURPOSE:
# Load one MeLiDos modality for one site and convert it into
# a regular tibble.
#
# INPUT:
# modality_value: Name of the MeLiDos modality, for example
#                 "sleepdiaries", "wearlog" or "light_chest".
# site_value:     Site to be loaded, here always "UCR".
# tz_value:       Time zone used by flatten_data().
#
# OUTPUT:
# A tibble containing the requested modality.
# ------------------------------------------------------------

load_melidos_flat <- function(
    modality_value,
    site_value,
    tz_value
) {
  
  data_raw <- melidosData::load_data(
    modality = modality_value,
    site = site_value
  )
  
  if (inherits(data_raw, "melidos_data")) {
    data_raw <- melidosData::flatten_data(
      data_raw,
      tz = tz_value
    )
  }
  
  data_raw <- tibble::as_tibble(data_raw)
  
  # Add a site column if it is missing.
  # This keeps later joins consistent.
  if (!"site" %in% names(data_raw)) {
    data_raw <- data_raw %>%
      dplyr::mutate(
        site = site_value
      )
  }
  
  data_raw
}


# ------------------------------------------------------------
# FUNCTION: Select the best matching column
# ------------------------------------------------------------
# PURPOSE:
# Identify a variable in a dataset based on exact candidate
# names or a regular-expression pattern.
#
# INPUT:
# data:        Dataset in which to search.
# candidates: Possible exact column names.
# pattern:    Optional regular expression for broader matching.
# object_name: Name used in the error message.
#
# OUTPUT:
# The name of the first matching column.
#
# NOTE:
# This function stops with an informative error if no column is found.
# ------------------------------------------------------------

pick_col <- function(
    data,
    candidates,
    pattern = NULL,
    object_name = "data"
) {
  
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
  
  stop(
    "No matching column found in ",
    object_name,
    ". Checked candidates: ",
    paste(candidates, collapse = ", ")
  )
}


# ------------------------------------------------------------
# FUNCTION: Select an optional column
# ------------------------------------------------------------
# PURPOSE:
# Identify a variable if it exists, but return NA if it does not.
#
# INPUT:
# data:        Dataset in which to search.
# candidates: Possible exact column names.
# pattern:    Optional regular expression for broader matching.
#
# OUTPUT:
# The name of the first matching column, or NA_character_.
#
# USE CASE:
# PIM is desirable but may not be available in the light_chest data.
# ------------------------------------------------------------

pick_col_optional <- function(
    data,
    candidates,
    pattern = NULL
) {
  
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


# ------------------------------------------------------------
# FUNCTION: Convert date-time variables to analysis time zone
# ------------------------------------------------------------
# PURPOSE:
# Ensure that date-time variables are POSIXct and expressed in
# the UCR analysis time zone.
#
# INPUT:
# x:        Date-time variable.
# tz_value: Target time zone.
#
# OUTPUT:
# POSIXct date-time variable in the target time zone.
# ------------------------------------------------------------

as_posix_analysis_tz <- function(
    x,
    tz_value
) {
  
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


# ------------------------------------------------------------
# FUNCTION: Convert sleep duration variables to minutes
# ------------------------------------------------------------
# PURPOSE:
# Make sure that sleep duration, sleep delay and awake duration
# are all expressed in minutes before calculating sleep efficiency.
#
# INPUT:
# x:             Numeric or difftime duration variable.
# variable_name: Variable name used in diagnostic messages.
#
# OUTPUT:
# Numeric duration in minutes.
#
# ASSUMPTION:
# If the median value is <= 24, the variable is assumed to be in hours
# and is converted to minutes.
# ------------------------------------------------------------

to_minutes_if_needed <- function(
    x,
    variable_name
) {
  
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


# ------------------------------------------------------------
# FUNCTION: Calculate wet-bulb temperature using Stull approximation
# ------------------------------------------------------------
# PURPOSE:
# Approximate wet-bulb temperature from air temperature and
# relative humidity.
#
# INPUT:
# temp_c:     Air temperature in degrees Celsius.
# rh_percent: Relative humidity in percent, for example 65, not 0.65.
#
# OUTPUT:
# Approximate wet-bulb temperature in degrees Celsius.
# ------------------------------------------------------------

calc_twb_stull <- function(
    temp_c,
    rh_percent
) {
  
  temp_c * atan(
    0.151977 * sqrt(rh_percent + 8.313659)
  ) +
    atan(temp_c + rh_percent) -
    atan(rh_percent - 1.676331) +
    0.00391838 * rh_percent^1.5 * atan(0.023101 * rh_percent) -
    4.686035
}


# ------------------------------------------------------------
# FUNCTION: Combine a date with a diary clock time
# ------------------------------------------------------------
# PURPOSE:
# Reconstruct a POSIXct timestamp by combining:
# - a calendar date,
# - the reported clock time from a diary variable.
#
# INPUT:
# date_value: Date used as the calendar date.
# time_source: Diary variable from which the clock time is extracted.
# tz_value: Target time zone of the reconstructed timestamp.
# source_tz: Time zone used only to extract the displayed clock time.
#
# OUTPUT:
# POSIXct timestamp in the target time zone.
#
# IMPORTANT:
# The reported diary clock time is preserved.
# No UTC-to-local time shift is applied.
# ------------------------------------------------------------

combine_date_with_clock_time <- function(
    date_value,
    time_source,
    tz_value,
    source_tz = "UTC"
) {
  
  clock_time <- extract_diary_clock_time(
    x = time_source,
    source_tz = source_tz
  )
  
  lubridate::ymd_hms(
    paste(
      as.Date(date_value),
      clock_time
    ),
    tz = tz_value,
    quiet = TRUE
  )
}

# ------------------------------------------------------------
# FUNCTION: Assign sleep-night diary clock times to the correct date
# ------------------------------------------------------------
# PURPOSE:
# Reconstruct timestamps for sleep-night variables that may occur
# before or after midnight.
#
# INPUT:
# time_source: Diary variable containing the reported clock time.
# sleep_date: Date on which the sleep night began.
# wake_date: Date on which the participant woke up.
# tz_value: Target time zone of the reconstructed timestamp.
# source_tz: Time zone used only to extract the displayed clock time.
# cutoff_hour: Clock times before this hour are assigned to wake_date.
#
# OUTPUT:
# POSIXct timestamp in the target time zone.
#
# LOGIC:
# - Times before cutoff_hour, for example 00:30, are assigned to wake_date.
# - Times from cutoff_hour onwards, for example 23:15, are assigned to sleep_date.
#
# IMPORTANT:
# The reported diary clock time is preserved.
# No UTC-to-local time shift is applied.
# ------------------------------------------------------------

assign_sleep_night_time <- function(
    time_source,
    sleep_date,
    wake_date,
    tz_value,
    source_tz = "UTC",
    cutoff_hour = 12
) {
  
  clock_time <- extract_diary_clock_time(
    x = time_source,
    source_tz = source_tz
  )
  
  clock_hour <- suppressWarnings(
    as.integer(
      stringr::str_extract(
        clock_time,
        "^\\d{1,2}"
      )
    )
  )
  
  assigned_date <- dplyr::case_when(
    is.na(clock_hour) ~ as.Date(NA),
    clock_hour < cutoff_hour ~ wake_date,
    TRUE ~ sleep_date
  )
  
  combine_date_with_clock_time(
    date_value = assigned_date,
    time_source = time_source,
    tz_value = tz_value,
    source_tz = source_tz
  )
}

# ------------------------------------------------------------
# FUNCTION: Extract diary clock time without time-zone shifting
# ------------------------------------------------------------
# PURPOSE:
# Extract the reported clock time from a diary date-time variable.
#
# INPUT:
# x:         Diary date-time variable.
# source_tz: Time zone used only for displaying POSIXct values.
#
# OUTPUT:
# Character vector with clock times in HH:MM:SS format.
#
# IMPORTANT:
# This function does not convert the time point to another time zone.
# It only extracts the displayed clock time.
# ------------------------------------------------------------

extract_diary_clock_time <- function(
    x,
    source_tz = "UTC"
) {
  
  if (inherits(x, "POSIXt")) {
    
    return(
      format(
        x,
        format = "%H:%M:%S",
        tz = source_tz
      )
    )
  }
  
  x_character <- as.character(
    x
  )
  
  clock_time <- stringr::str_extract(
    x_character,
    "\\d{1,2}:\\d{2}(:\\d{2})?"
  )
  
  clock_time <- dplyr::if_else(
    !is.na(clock_time) &
      stringr::str_count(clock_time, ":") == 1,
    paste0(clock_time, ":00"),
    clock_time
  )
  
  clock_time
}