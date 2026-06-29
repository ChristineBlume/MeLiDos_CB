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
# FUNCTION: Extract diary date without time-zone shifting
# ------------------------------------------------------------
# PURPOSE:
# Extract the displayed diary date from a diary date-time variable.
#
# INPUT:
# x: Diary date-time variable.
#
# OUTPUT:
# Date vector.
#
# IMPORTANT:
# This function does not convert the time point to another time zone.
# It preserves the date as displayed by the diary variable itself.
# ------------------------------------------------------------

extract_diary_date <- function(
    x
) {
  
  if (inherits(x, "POSIXt")) {
    return(
      as.Date(
        format(
          x,
          format = "%Y-%m-%d"
        )
      )
    )
  }
  
  x_character <- as.character(
    x
  )
  
  date_value <- stringr::str_extract(
    x_character,
    "\\d{4}-\\d{2}-\\d{2}"
  )
  
  as.Date(
    date_value
  )
}


# ------------------------------------------------------------
# FUNCTION: Extract diary clock time without time-zone shifting
# ------------------------------------------------------------
# PURPOSE:
# Extract the displayed clock time from a diary date-time variable.
#
# INPUT:
# x: Diary date-time variable.
# source_tz: Deprecated argument kept for backward compatibility.
#
# OUTPUT:
# Character vector with clock times in HH:MM:SS format.
#
# IMPORTANT:
# This function deliberately does not use with_tz(), force_tz(),
# or format(..., tz = ...). It preserves the clock time as displayed
# by the diary variable itself.
# ------------------------------------------------------------

extract_diary_clock_time <- function(
    x,
    source_tz = NULL
) {
  
  if (inherits(x, "POSIXt")) {
    
    clock_time <- format(
      x,
      format = "%H:%M:%S"
    )
    
    return(
      clock_time
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


# ------------------------------------------------------------
# FUNCTION: Combine a date with a diary clock time
# ------------------------------------------------------------
# PURPOSE:
# Reconstruct a POSIXct timestamp by combining:
# - a calendar date,
# - the displayed clock time from a diary variable.
#
# INPUT:
# date_value: Date used as the calendar date.
# time_source: Diary variable from which the clock time is extracted.
# tz_value: Target time zone of the reconstructed timestamp.
# source_tz: Deprecated argument kept for backward compatibility.
#
# OUTPUT:
# POSIXct timestamp in the target time zone.
#
# IMPORTANT:
# The diary clock time is preserved. No time-zone conversion is applied
# to the reported clock time.
# ------------------------------------------------------------

combine_date_with_clock_time <- function(
    date_value,
    time_source,
    tz_value,
    source_tz = NULL
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
# time_source: Diary variable containing the displayed clock time.
# sleep_date: Date on which the sleep night began.
# wake_date: Date on which the participant woke up.
# tz_value: Target time zone of the reconstructed timestamp.
# source_tz: Deprecated argument kept for backward compatibility.
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
# The diary clock time is preserved. No time-zone conversion is applied
# to the reported clock time.
# ------------------------------------------------------------

assign_sleep_night_time <- function(
    time_source,
    sleep_date,
    wake_date,
    tz_value,
    source_tz = NULL,
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
# FUNCTION: Apply Cole-Kripke-style sleep/wake scoring
# ------------------------------------------------------------
# PURPOSE:
# Score 60-second activity epochs as sleep or wake using a
# Cole-Kripke-style weighted moving window.
#
# INPUT:
# activity: Numeric vector of 60-second activity counts.
# threshold: Sleep/wake threshold for the weighted sleep index.
#
# OUTPUT:
# Logical vector:
# TRUE  = scored sleep
# FALSE = scored wake
#
# IMPORTANT:
# This implementation is applied to wrist PIM aggregated to
# 60-second epochs. It is therefore a PIM-based actigraphy estimate,
# not a GGIR-derived estimate and not an ActStudio export.
#
# CLASSIFICATION:
# sleep_index < threshold  -> sleep
# sleep_index >= threshold -> wake
# ------------------------------------------------------------

score_cole_kripke_60s <- function(
    activity,
    threshold = 1
) {
  
  activity_scaled <- as.numeric(activity) / 100
  
  activity_scaled <- pmin(
    activity_scaled,
    300
  )
  
  activity_scaled[is.na(activity_scaled)] <- 0
  
  activity_lag4 <- dplyr::lag(
    activity_scaled,
    n = 4,
    default = 0
  )
  
  activity_lag3 <- dplyr::lag(
    activity_scaled,
    n = 3,
    default = 0
  )
  
  activity_lag2 <- dplyr::lag(
    activity_scaled,
    n = 2,
    default = 0
  )
  
  activity_lag1 <- dplyr::lag(
    activity_scaled,
    n = 1,
    default = 0
  )
  
  activity_lead1 <- dplyr::lead(
    activity_scaled,
    n = 1,
    default = 0
  )
  
  activity_lead2 <- dplyr::lead(
    activity_scaled,
    n = 2,
    default = 0
  )
  
  sleep_index <-
    0.001 *
    (
      106 * activity_lag4 +
        54 * activity_lag3 +
        58 * activity_lag2 +
        76 * activity_lag1 +
        230 * activity_scaled +
        74 * activity_lead1 +
        67 * activity_lead2
    )
  
  sleep_index < threshold
}


# ------------------------------------------------------------
# FUNCTION: Calculate interdaily stability
# ------------------------------------------------------------
# PURPOSE:
# Calculate classical interdaily stability from an activity time
# series.
#
# INPUT:
# data: Dataset containing participant, time and activity variables.
# id_col: Participant identifier.
# site_col: Site variable.
# datetime_col: Date-time variable.
# activity_col: Activity variable.
# epoch_minutes: Time-bin length in minutes used for the IS formula.
#
# OUTPUT:
# One row per participant and site with:
# - pim_interdaily_stability,
# - number of bins,
# - number of days.
#
# FORMULA:
# IS = n * sum((mean activity per clock bin - grand mean)^2) /
#      p * sum((activity value - grand mean)^2)
#
# where:
# n = total number of observations,
# p = number of clock bins per day,
# x_h = mean activity in clock bin h across days,
# x_bar = grand mean,
# x_i = individual activity value.
# ------------------------------------------------------------

calculate_interdaily_stability <- function(
    data,
    id_col = "Id",
    site_col = "site",
    datetime_col = "datetime",
    activity_col = "PIM",
    epoch_minutes = 60
) {
  
  data_prepared <- data %>%
    dplyr::transmute(
      Id = .data[[id_col]],
      site = .data[[site_col]],
      datetime = .data[[datetime_col]],
      activity = .data[[activity_col]]
    ) %>%
    dplyr::filter(
      !is.na(Id),
      !is.na(site),
      !is.na(datetime),
      !is.na(activity)
    ) %>%
    dplyr::mutate(
      datetime_bin =
        lubridate::floor_date(
          datetime,
          unit = paste(epoch_minutes, "minutes")
        ),
      
      date_bin =
        as.Date(
          datetime_bin
        ),
      
      clock_bin =
        lubridate::hour(datetime_bin) * 60 +
        lubridate::minute(datetime_bin)
    ) %>%
    dplyr::group_by(
      Id,
      site,
      datetime_bin,
      date_bin,
      clock_bin
    ) %>%
    dplyr::summarise(
      activity_value =
        mean(
          activity,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  data_with_summary <- data_prepared %>%
    dplyr::group_by(
      Id,
      site
    ) %>%
    dplyr::mutate(
      grand_mean =
        mean(
          activity_value,
          na.rm = TRUE
        ),
      n_total =
        sum(
          !is.na(activity_value)
        ),
      p_bins =
        dplyr::n_distinct(
          clock_bin
        ),
      n_days_is =
        dplyr::n_distinct(
          date_bin
        )
    ) %>%
    dplyr::ungroup()
  
  numerator <- data_with_summary %>%
    dplyr::group_by(
      Id,
      site,
      clock_bin
    ) %>%
    dplyr::summarise(
      clock_bin_mean =
        mean(
          activity_value,
          na.rm = TRUE
        ),
      grand_mean =
        dplyr::first(
          grand_mean
        ),
      n_total =
        dplyr::first(
          n_total
        ),
      .groups = "drop"
    ) %>%
    dplyr::group_by(
      Id,
      site
    ) %>%
    dplyr::summarise(
      numerator =
        dplyr::first(n_total) *
        sum(
          (clock_bin_mean - dplyr::first(grand_mean))^2,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  denominator <- data_with_summary %>%
    dplyr::group_by(
      Id,
      site
    ) %>%
    dplyr::summarise(
      denominator =
        dplyr::first(p_bins) *
        sum(
          (activity_value - dplyr::first(grand_mean))^2,
          na.rm = TRUE
        ),
      n_pim_bins_is =
        dplyr::n(),
      n_days_is =
        dplyr::first(
          n_days_is
        ),
      is_epoch_minutes =
        epoch_minutes,
      .groups = "drop"
    )
  
  numerator %>%
    dplyr::left_join(
      denominator,
      by = c(
        "Id",
        "site"
      )
    ) %>%
    dplyr::mutate(
      pim_interdaily_stability =
        dplyr::if_else(
          denominator > 0,
          numerator / denominator,
          NA_real_
        ),
      is_minimum_7_days =
        n_days_is >= 7
    ) %>%
    dplyr::select(
      Id,
      site,
      pim_interdaily_stability,
      n_pim_bins_is,
      n_days_is,
      is_epoch_minutes,
      is_minimum_7_days
    )
}

# ------------------------------------------------------------
# FUNCTION: Safely write a CSV file
# ------------------------------------------------------------
# PURPOSE:
# Write a CSV file while avoiding common failures caused by:
# - missing output directories,
# - files currently open in Excel,
# - OneDrive temporarily locking a file.
#
# INPUT:
# data: Data frame or tibble to save.
# path: Target file path.
#
# OUTPUT:
# Writes the CSV file to path.
# If writing to path fails, writes a timestamped fallback file
# in the same folder.
# ------------------------------------------------------------

write_csv_safely <- function(
    data,
    path
) {
  
  # Create the target directory if it does not yet exist.
  target_dir <- dirname(
    path
  )
  
  dir.create(
    target_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Try to write to the requested path first.
  tryCatch(
    {
      readr::write_csv(
        data,
        path
      )
      
      message(
        "CSV written to: ",
        path
      )
    },
    
    # If writing fails, create a timestamped fallback file.
    error = function(e) {
      
      fallback_path <- file.path(
        target_dir,
        paste0(
          tools::file_path_sans_ext(
            basename(
              path
            )
          ),
          "_",
          format(
            Sys.time(),
            "%Y%m%d_%H%M%S"
          ),
          ".csv"
        )
      )
      
      message(
        "Could not write CSV to: ",
        path,
        "\nReason: ",
        conditionMessage(
          e
        ),
        "\nTrying fallback path: ",
        fallback_path
      )
      
      readr::write_csv(
        data,
        fallback_path
      )
      
      message(
        "CSV written to fallback path: ",
        fallback_path
      )
    }
  )
  
  invisible(
    path
  )
}

# ------------------------------------------------------------
# FUNCTION: Safely write an Excel file
# ------------------------------------------------------------
# PURPOSE:
# Write an Excel file while avoiding common failures caused by:
# - missing output directories,
# - files currently open in Excel,
# - OneDrive temporarily locking a file.
#
# INPUT:
# data: Data frame or tibble to save.
# path: Target Excel file path.
#
# OUTPUT:
# Writes the Excel file to path.
# If writing to path fails, writes a timestamped fallback file
# in the same folder.
# ------------------------------------------------------------

write_xlsx_safely <- function(
    data,
    path
) {
  
  # Create the target directory if it does not yet exist.
  target_dir <- dirname(
    path
  )
  
  dir.create(
    target_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Try to write to the requested path first.
  tryCatch(
    {
      openxlsx::write.xlsx(
        x =
          data,
        file =
          path,
        overwrite =
          TRUE
      )
      
      message(
        "Excel file written to: ",
        path
      )
    },
    
    # If writing fails, create a timestamped fallback file.
    error = function(e) {
      
      fallback_path <- file.path(
        target_dir,
        paste0(
          tools::file_path_sans_ext(
            basename(
              path
            )
          ),
          "_",
          format(
            Sys.time(),
            "%Y%m%d_%H%M%S"
          ),
          ".xlsx"
        )
      )
      
      message(
        "Could not write Excel file to: ",
        path,
        "\nReason: ",
        conditionMessage(
          e
        ),
        "\nTrying fallback path: ",
        fallback_path
      )
      
      openxlsx::write.xlsx(
        x =
          data,
        file =
          fallback_path,
        overwrite =
          TRUE
      )
      
      message(
        "Excel file written to fallback path: ",
        fallback_path
      )
    }
  )
  
  invisible(
    path
  )
}
