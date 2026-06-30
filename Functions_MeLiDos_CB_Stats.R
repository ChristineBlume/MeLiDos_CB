
prepare_model_data <- function(model_config) {
  
  if (model_config$analysis_level == "night") {
    
    model_data <- analysis_sleep_gam %>%
      dplyr::transmute(
        Id = Id,
        outcome_model = as.numeric(.data[[model_config$outcome_variable]]),
        temperature_model = as.numeric(.data[[model_config$temperature_variable]]),
        light_model = as.numeric(.data[[model_config$light_variable]])
      ) %>%
      dplyr::filter(
        !is.na(Id),
        is.finite(outcome_model),
        is.finite(temperature_model),
        is.finite(light_model)
      )
    
  } else if (model_config$analysis_level == "person") {
    
    model_data <- analysis_person_gam %>%
      dplyr::transmute(
        Id = Id,
        outcome_model = as.numeric(.data[[model_config$outcome_variable]]),
        temperature_model = as.numeric(.data[[model_config$temperature_variable]]),
        light_model = as.numeric(.data[[model_config$light_variable]])
      ) %>%
      dplyr::filter(
        !is.na(Id),
        is.finite(outcome_model),
        is.finite(temperature_model),
        is.finite(light_model)
      )
    
  } else {
    
    stop(
      "Unknown analysis_level: ",
      model_config$analysis_level,
      call. = FALSE
    )
  }
  
  return(model_data)
}


build_gam_formula <- function(model_config) {
  
  if (model_config$analysis_level == "night") {
    
    model_formula <- outcome_model ~
      te(
        temperature_model,
        light_model,
        k = c(5, 5)
      ) +
      s(
        Id,
        bs = "re"
      )
    
  } else if (model_config$analysis_level == "person") {
    
    model_formula <- outcome_model ~
      te(
        temperature_model,
        light_model,
        k = c(4, 4)
      )
    
  } else {
    
    stop(
      "Unknown analysis_level: ",
      model_config$analysis_level,
      call. = FALSE
    )
  }
  
  return(model_formula)
}


fit_gam_model <- function(model_config) {
  
  model_data <- prepare_model_data(
    model_config = model_config
  )
  
  model_formula <- build_gam_formula(
    model_config = model_config
  )
  
  if (model_config$analysis_level == "night") {
    
    fitted_model <- mgcv::bam(
      formula = model_formula,
      data = model_data,
      family = gaussian(),
      method = "fREML",
      discrete = TRUE
    )
    
  } else {
    
    fitted_model <- mgcv::gam(
      formula = model_formula,
      data = model_data,
      family = gaussian(),
      method = "REML"
    )
  }
  
  saveRDS(
    fitted_model,
    file.path(
      models_dir,
      paste0(
        model_config$model_id,
        ".rds"
      )
    )
  )
  
  output <- list(
    config = model_config,
    data = model_data,
    formula = model_formula,
    model = fitted_model
  )
  
  rm(
    model_data,
    model_formula,
    fitted_model
  )
  
  return(output)
}


extract_model_results <- function(model_result) {
  
  model_config <- model_result$config
  fitted_model <- model_result$model
  model_data <- model_result$data
  model_summary <- summary(fitted_model)
  
  fit_summary <- tibble::tibble(
    model_id = model_config$model_id,
    analysis_level = model_config$analysis_level,
    outcome_label = model_config$outcome_label,
    exposure_family = model_config$exposure_family,
    n_rows = stats::nobs(fitted_model),
    n_ids = dplyr::n_distinct(model_data$Id),
    adjusted_r_squared = unname(model_summary$r.sq),
    deviance_explained = unname(model_summary$dev.expl),
    scale_estimate = unname(model_summary$scale),
    AIC = stats::AIC(fitted_model),
    REML_or_fREML_score = unname(fitted_model$gcv.ubre)
  )
  
  smooth_summary <- as.data.frame(
    model_summary$s.table
  ) %>%
    tibble::rownames_to_column(
      "smooth_term"
    ) %>%
    dplyr::mutate(
      model_id = model_config$model_id,
      analysis_level = model_config$analysis_level,
      outcome_label = model_config$outcome_label,
      exposure_family = model_config$exposure_family,
      .before = 1
    )
  
  parametric_summary <- as.data.frame(
    model_summary$p.table
  ) %>%
    tibble::rownames_to_column(
      "parametric_term"
    ) %>%
    dplyr::mutate(
      model_id = model_config$model_id,
      analysis_level = model_config$analysis_level,
      outcome_label = model_config$outcome_label,
      exposure_family = model_config$exposure_family,
      .before = 1
    )
  
  k_check <- as.data.frame(
    mgcv::k.check(
      fitted_model
    )
  ) %>%
    tibble::rownames_to_column(
      "smooth_term"
    ) %>%
    dplyr::mutate(
      model_id = model_config$model_id,
      analysis_level = model_config$analysis_level,
      outcome_label = model_config$outcome_label,
      exposure_family = model_config$exposure_family,
      .before = 1
    )
  
  concurvity_matrix <- mgcv::concurvity(
    fitted_model,
    full = TRUE
  )
  
  concurvity_summary <- as.data.frame(
    concurvity_matrix
  ) %>%
    tibble::rownames_to_column(
      "concurvity_metric"
    ) %>%
    tidyr::pivot_longer(
      cols = -concurvity_metric,
      names_to = "term",
      values_to = "concurvity"
    ) %>%
    dplyr::mutate(
      model_id = model_config$model_id,
      analysis_level = model_config$analysis_level,
      outcome_label = model_config$outcome_label,
      exposure_family = model_config$exposure_family,
      .before = 1
    )
  
  term_matrix <- stats::predict(
    fitted_model,
    type = "terms"
  )
  
  term_variance <- apply(
    term_matrix,
    2,
    stats::var,
    na.rm = TRUE
  )
  
  term_variance_summary <- tibble::enframe(
    term_variance,
    name = "term",
    value = "variance"
  ) %>%
    dplyr::mutate(
      model_id = model_config$model_id,
      analysis_level = model_config$analysis_level,
      outcome_label = model_config$outcome_label,
      exposure_family = model_config$exposure_family,
      variance_contribution = variance / sum(variance, na.rm = TRUE),
      variance_contribution_percent = 100 * variance_contribution,
      .before = 1
    )
  
  output <- list(
    fit_summary = fit_summary,
    smooth_summary = smooth_summary,
    parametric_summary = parametric_summary,
    k_check = k_check,
    concurvity_summary = concurvity_summary,
    term_variance_summary = term_variance_summary
  )
  
  rm(
    model_config,
    fitted_model,
    model_data,
    model_summary,
    concurvity_matrix,
    term_matrix,
    term_variance
  )
  
  return(output)
}


save_model_text_outputs <- function(model_result) {
  
  model_id <- model_result$config$model_id
  fitted_model <- model_result$model
  
  capture.output(
    summary(
      fitted_model
    ),
    file = file.path(
      text_dir,
      paste0(
        model_id,
        "_summary.txt"
      )
    )
  )
  
  capture.output(
    mgcv::gam.check(
      fitted_model
    ),
    file = file.path(
      text_dir,
      paste0(
        model_id,
        "_gam_check.txt"
      )
    )
  )
  
  capture.output(
    mgcv::concurvity(
      fitted_model,
      full = TRUE
    ),
    file = file.path(
      text_dir,
      paste0(
        model_id,
        "_concurvity.txt"
      )
    )
  )
  
  invisible(NULL)
}


save_model_diagnostic_plot <- function(model_result) {
  
  model_id <- model_result$config$model_id
  fitted_model <- model_result$model
  
  png(
    filename = file.path(
      plots_dir,
      paste0(
        model_id,
        "_gam_check.png"
      )
    ),
    width = 1200,
    height = 900,
    res = 150
  )
  
  mgcv::gam.check(
    fitted_model
  )
  
  dev.off()
  
  invisible(NULL)
}

# ------------------------------------------------------------
# FUNCTION: Shared theme for publication-style ggplots
# ------------------------------------------------------------
# PURPOSE:
# Apply a consistent plotting style across all effect-surface plots.
#
# STYLE:
# - colourblind-friendly palette used separately in the plot function
# - black panel border
# - short black axis tick marks
# - black axis labels and tick labels
# - clean white background
# ------------------------------------------------------------

theme_effect_surface <- function() {
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
    ggplot2::theme(
      panel.grid.minor =
        ggplot2::element_blank(),
      
      panel.grid.major =
        ggplot2::element_blank(),
      
      panel.border =
        ggplot2::element_rect(
          colour = "black",
          fill = NA,
          linewidth = 0.7
        ),
      
      axis.line =
        ggplot2::element_blank(),
      
      axis.ticks =
        ggplot2::element_line(
          colour = "black",
          linewidth = 0.4
        ),
      
      axis.ticks.length =
        grid::unit(
          2,
          "mm"
        ),
      
      axis.text =
        ggplot2::element_text(
          colour = "black"
        ),
      
      axis.title =
        ggplot2::element_text(
          colour = "black"
        ),
      
      plot.title =
        ggplot2::element_text(
          colour = "black",
          face = "bold"
        ),
      
      legend.title =
        ggplot2::element_text(
          colour = "black"
        ),
      
      legend.text =
        ggplot2::element_text(
          colour = "black"
        ),
      
      legend.key.height =
        grid::unit(
          3.5,
          "mm"
        ),
      
      legend.key.width =
        grid::unit(
          4,
          "mm"
        )
    )
}


# ------------------------------------------------------------
# FUNCTION: Shared effect-surface theme
# ------------------------------------------------------------
# PURPOSE:
# Apply the same publication-style theme to all effect-surface plots.
#
# STYLE:
# - black frame
# - short black axis tick marks
# - black axis labels and tick labels
# - no grid lines
# ------------------------------------------------------------

theme_effect_surface <- function() {
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
    ggplot2::theme(
      panel.grid.major =
        ggplot2::element_blank(),
      
      panel.grid.minor =
        ggplot2::element_blank(),
      
      panel.border =
        ggplot2::element_rect(
          colour = "black",
          fill = NA,
          linewidth = 0.7
        ),
      
      axis.ticks =
        ggplot2::element_line(
          colour = "black",
          linewidth = 0.4
        ),
      
      axis.ticks.length =
        grid::unit(
          2,
          "mm"
        ),
      
      axis.text =
        ggplot2::element_text(
          colour = "black"
        ),
      
      axis.title =
        ggplot2::element_text(
          colour = "black"
        ),
      
      plot.title =
        ggplot2::element_text(
          colour = "black",
          face = "bold"
        ),
      
      legend.title =
        ggplot2::element_text(
          colour = "black"
        ),
      
      legend.text =
        ggplot2::element_text(
          colour = "black"
        )
    )
}


# ------------------------------------------------------------
# FUNCTION: Create effect-surface plot
# ------------------------------------------------------------
# PURPOSE:
# Create one effect-surface plot for one fitted GAM.
#
# INPUT:
# model_result:
#   A list produced by fit_gam_model(), containing:
#   - config
#   - data
#   - formula
#   - model
#
# OUTPUT:
# Saves one PNG plot per model.
#
# IMPORTANT:
# This function is designed for:
# purrr::map(model_results, create_effect_surface_plot)
# ------------------------------------------------------------

create_effect_surface_plot <- function(model_result) {
  
  # Extract model components from the registry result object.
  model_config <- model_result$config
  fitted_model <- model_result$model
  model_data <- model_result$data
  
  # Define the central observed prediction range.
  # This avoids extrapolating the surface into sparse extremes.
  x_limits <- stats::quantile(
    model_data$temperature_model,
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  y_limits <- stats::quantile(
    model_data$light_model,
    probs =
      c(
        0.02,
        0.98
      ),
    na.rm =
      TRUE
  )
  
  # Create prediction grid.
  prediction_grid <- tidyr::expand_grid(
    temperature_model =
      seq(
        from =
          x_limits[[1]],
        to =
          x_limits[[2]],
        length.out =
          80
      ),
    
    light_model =
      seq(
        from =
          y_limits[[1]],
        to =
          y_limits[[2]],
        length.out =
          80
      )
  )
  
  # Add a valid participant level for person-night models.
  # The participant random effect is excluded from prediction below.
  if (model_config$analysis_level == "night") {
    
    prediction_grid <- prediction_grid %>%
      dplyr::mutate(
        Id =
          factor(
            levels(
              model_data$Id
            )[[1]],
            levels =
              levels(
                model_data$Id
              )
          )
      )
    
    predictions <- stats::predict(
      fitted_model,
      newdata =
        prediction_grid,
      type =
        "response",
      se.fit =
        TRUE,
      exclude =
        "s(Id)"
    )
    
  } else {
    
    predictions <- stats::predict(
      fitted_model,
      newdata =
        prediction_grid,
      type =
        "response",
      se.fit =
        TRUE
    )
  }
  
  # Store fitted values and approximate uncertainty.
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
  
  # Create effect-surface plot.
  effect_surface_plot <- ggplot2::ggplot(
    prediction_surface,
    ggplot2::aes(
      x =
        temperature_model,
      y =
        light_model,
      fill =
        fit
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_contour(
      ggplot2::aes(
        z =
          fit
      ),
      colour =
        "black",
      linewidth =
        0.25,
      alpha =
        0.65
    ) +
    ggplot2::scale_fill_viridis_c(
      option =
        "C",
      direction =
        1,
      name =
        "Predicted\noutcome",
      guide =
        ggplot2::guide_colorbar(
          frame.colour =
            "black",
          ticks.colour =
            "black"
        )
    ) +
    ggplot2::labs(
      title =
        paste0(
          model_config$outcome_label,
          ": fitted exposure surface"
        ),
      
      x =
        model_config$temperature_label,
      
      y =
        model_config$light_label
    ) +
    ggplot2::coord_cartesian(
      expand =
        FALSE
    ) +
    theme_effect_surface()
  
  # Save only the plot, not another check table.
  ggplot2::ggsave(
    filename =
      file.path(
        plots_dir,
        paste0(
          model_config$model_id,
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
    model_config,
    fitted_model,
    model_data,
    x_limits,
    y_limits,
    prediction_grid,
    predictions,
    effect_surface_plot
  )
  
  return(
    output
  )
}