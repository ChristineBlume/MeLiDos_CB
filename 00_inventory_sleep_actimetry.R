# 00_inventory_sleep_actimetry.R
# Ziel: Überblick über verfügbare MeLiDos-Daten für Schlaf, Wearables und Licht.
# Fokus: Welche Variablen liegen auf Personen-, Tages-, Nacht- oder Minutenebene vor?

# 1) Pakete laden -------------------------------------------------------------

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

# 2) Output-Ordner anlegen ----------------------------------------------------

dir.create(
  "output/overview",
  recursive = TRUE,
  showWarnings = FALSE
)

# 3) Modalitäten festlegen ----------------------------------------------------
# Die Liste enthält bewusst auch mögliche Wearable-/Lichtdaten.
# Falls eine Modalität nicht verfügbar ist, wird sie übersprungen.

modalities <- c(
  "sleepdiaries",
  "wearlog",
  "light_chest",
  "light_wrist",
  "light_glasses",
  "light_chest_1minute",
  "light_wrist_1minute",
  "light_glasses_1minute",
  "lightexposurediary",
  "exercisediary",
  "wellbeingdiary",
  "currentconditions",
  "demographics",
  "chronotype",
  "health",
  "trial_times"
)

# 4) Hilfsfunktion: Daten sicher laden ----------------------------------------

load_melidos_safe <- function(modality_value) {
  
  message("Loading: ", modality_value)
  
  data_raw <- tryCatch(
    melidosData::load_data(
      modality = modality_value,
      site = "all"
    ),
    error = function(e) {
      message("  not available: ", modality_value)
      NULL
    }
  )
  
  data_raw
}

# 5) Daten laden --------------------------------------------------------------

data_list <- modalities |>
  rlang::set_names() |>
  purrr::map(load_melidos_safe)

data_list <- data_list[
  !purrr::map_lgl(data_list, is.null)
]

# 6) Überblick pro Datensatz --------------------------------------------------

dataset_overview <- tibble(
  modality = names(data_list),
  data = unname(data_list)
) |>
  mutate(
    n_rows = purrr::map_int(data, nrow),
    n_cols = purrr::map_int(data, ncol),
    has_site = purrr::map_lgl(data, ~ "site" %in% names(.x)),
    has_id = purrr::map_lgl(data, ~ "Id" %in% names(.x)),
    n_sites = purrr::map_int(
      data,
      ~ if ("site" %in% names(.x)) dplyr::n_distinct(.x$site) else NA_integer_
    ),
    n_ids = purrr::map_int(
      data,
      ~ if ("Id" %in% names(.x)) dplyr::n_distinct(.x$Id) else NA_integer_
    )
  ) |>
  select(-data)

readr::write_csv(
  dataset_overview,
  "output/overview/overview_01_datasets.csv"
)

dataset_overview