# 00_inventory_sleep_actimetry.R
# Purpose: Create an overview of available MeLiDos data for sleep, wearables,
# light exposure, and potential actimetry-related variables.
# Output: CSV files with dataset-level and variable-level summaries.

# 1) Load required packages ---------------------------------------------------

packages_needed <- c(
  "melidosData",
  "dplyr",
  "purrr",
  "tidyr",
  "tibble",
  "stringr",
  "lubridate",
  "readr"
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

# 2) Create output folder -----------------------------------------------------

dir.create(
  "output/overview",
  recursive = TRUE,
  showWarnings = FALSE
)

# 3) Define site and modalities to inspect ------------------------------------
# The previous HTML analysis used chest-level light exposure data from UCR.
# Therefore, we only load the UCR light_chest dataset, plus sleep diaries and
# wear logs needed for sleep timing and wear-time annotation.

site <- "UCR"

modalities <- c(
  "sleepdiaries",
  "wearlog",
  "light_chest"
)

# 4) Define a safe data-loading function --------------------------------------
# This function loads each selected modality for the selected site.
# If loading fails, the function returns NULL and continues.

load_melidos_safe <- function(modality_value) {
  
  message("Loading: ", modality_value)
  
  data_raw <- tryCatch(
    melidosData::load_data(
      modality = modality_value,
      site = site
    ),
    error = function(e) {
      message("  not available: ", modality_value)
      return(NULL)
    }
  )
  
  if (is.null(data_raw)) {
    return(NULL)
  }
  
  data_flat <- tryCatch(
    {
      if (inherits(data_raw, "melidos_data")) {
        melidosData::flatten_data(
          data_raw,
          tz = "UTC"
        )
      } else {
        data_raw
      }
    },
    error = function(e) {
      message("  could not flatten: ", modality_value)
      return(NULL)
    }
  )
  
  if (!is.data.frame(data_flat)) {
    message("  not a data frame after loading: ", modality_value)
    return(NULL)
  }
  
  tibble::as_tibble(data_flat)
}

# 5) Load all selected modalities ---------------------------------------------

data_list <- modalities |>
  rlang::set_names() |>
  purrr::map(load_melidos_safe)

data_list <- data_list[
  !purrr::map_lgl(data_list, is.null)
]

# 6) Create dataset-level overview --------------------------------------------
# This table summarises the number of rows, columns, sites, and participants
# for each available modality.

dataset_overview <- tibble(
  modality = names(data_list),
  data = unname(data_list)
) |>
  mutate(
    n_rows = purrr::map_int(
      data,
      ~ nrow(.x)
    ),
    n_cols = purrr::map_int(
      data,
      ~ ncol(.x)
    ),
    has_site = purrr::map_lgl(
      data,
      ~ "site" %in% names(.x)
    ),
    has_id = purrr::map_lgl(
      data,
      ~ "Id" %in% names(.x)
    ),
    n_sites = purrr::map_int(
      data,
      ~ if ("site" %in% names(.x)) {
        dplyr::n_distinct(.x$site)
      } else {
        NA_integer_
      }
    ),
    n_ids = purrr::map_int(
      data,
      ~ if ("Id" %in% names(.x)) {
        dplyr::n_distinct(.x$Id)
      } else {
        NA_integer_
      }
    )
  ) |>
  select(-data)

readr::write_csv(
  dataset_overview,
  "output/overview/overview_01_datasets.csv"
)

dataset_overview

# 7) Extract variable names, classes, and labels -------------------------------
# This creates a searchable overview of all variables in all loaded datasets.

get_variable_overview <- function(data, modality_value) {
  
  tibble(
    modality = modality_value,
    variable = names(data),
    class = purrr::map_chr(data, ~ class(.x)[1]),
    label = purrr::map_chr(
      data,
      ~ {
        label_value <- attr(.x, "label")
        
        if (is.null(label_value)) {
          NA_character_
        } else {
          as.character(label_value)
        }
      }
    )
  )
}

variable_overview <- purrr::imap_dfr(
  data_list,
  get_variable_overview
)

readr::write_csv(
  variable_overview,
  "output/overview/overview_02_variables.csv"
)

View(variable_overview)

# 8) Identify candidate variables by domain -----------------------------------
# Candidate variables are selected based on names and labels.
# This does not replace manual inspection, but provides a useful first filter.

search_terms <- c(
  sleep = "sleep|bed|wake|awake|quality|duration|latency|delay|nap|night",
  actimetry = "act|activity|movement|pim|tat|zcm|accel|rest|counts",
  light = "medi|melanopic|edi|lux|illuminance|irradiance|light|photopic",
  temperature = "temp|temperature|climate|weather|ambient|outdoor"
)

variable_candidates <- purrr::imap_dfr(
  search_terms,
  ~ variable_overview |>
    filter(
      stringr::str_detect(
        stringr::str_to_lower(variable),
        .x
      ) |
        stringr::str_detect(
          stringr::str_to_lower(label),
          .x
        )
    ) |>
    mutate(domain = .y)
) |>
  select(domain, modality, variable, class, label) |>
  arrange(domain, modality, variable)

readr::write_csv(
  variable_candidates,
  "output/overview/overview_03_variable_candidates.csv"
)

View(variable_candidates)

# 9) Inspect sleep diary data --------------------------------------------------
# This section checks the number of observations, participants, sites, dates,
# and missing values in the sleep diary dataset.

sleep_diary <- data_list[["sleepdiaries"]]

sleep_overview <- sleep_diary |>
  summarise(
    n_rows = n(),
    n_ids = n_distinct(Id),
    n_sites = n_distinct(site),
    first_sleep_date = min(as.Date(sleep), na.rm = TRUE),
    last_sleep_date = max(as.Date(wake), na.rm = TRUE)
  )

sleep_by_site <- sleep_diary |>
  group_by(site) |>
  summarise(
    n_rows = n(),
    n_ids = n_distinct(Id),
    first_sleep_date = min(as.Date(sleep), na.rm = TRUE),
    last_sleep_date = max(as.Date(wake), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(site)

sleep_missing <- sleep_diary |>
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x)) * 100
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "percent_missing"
  ) |>
  arrange(desc(percent_missing))

readr::write_csv(
  sleep_overview,
  "output/overview/overview_04_sleep_overall.csv"
)

readr::write_csv(
  sleep_by_site,
  "output/overview/overview_05_sleep_by_site.csv"
)

readr::write_csv(
  sleep_missing,
  "output/overview/overview_06_sleep_missing.csv"
)

sleep_overview
sleep_by_site
sleep_missing

# 10) Inspect wearable-related modality ---------------------------------------
# The previous HTML analysis used chest-level light exposure data only.

wearable_modalities <- c(
  "light_chest"
)

wearable_overview <- tibble(
  modality = wearable_modalities
) |>
  filter(modality %in% names(data_list)) |>
  mutate(
    data = purrr::map(modality, ~ data_list[[.x]]),
    n_rows = purrr::map_int(data, nrow),
    n_cols = purrr::map_int(data, ncol),
    n_ids = purrr::map_int(data, ~ n_distinct(.x$Id)),
    n_sites = purrr::map_int(data, ~ n_distinct(.x$site))
  ) |>
  select(-data)

wearable_variables <- variable_overview |>
  filter(modality %in% wearable_modalities) |>
  arrange(modality, variable)

wearable_candidates <- variable_candidates |>
  filter(
    modality %in% wearable_modalities,
    domain %in% c("actimetry", "light", "temperature")
  ) |>
  arrange(domain, modality, variable)

readr::write_csv(
  wearable_overview,
  "output/overview/overview_07_wearables.csv"
)

readr::write_csv(
  wearable_variables,
  "output/overview/overview_08_wearable_variables.csv"
)

readr::write_csv(
  wearable_candidates,
  "output/overview/overview_09_wearable_candidates.csv"
)

wearable_overview
wearable_candidates

# 11) Clean up temporary objects ----------------------------------------------

rm(
  packages_needed,
  packages_missing,
  modalities,
  wearable_modalities,
  search_terms
)