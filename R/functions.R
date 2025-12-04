# load magrittr pipe
`%>%` <- magrittr::`%>%`

#' Create descriptive table
#'
#' @param data Lipidomics data
#'
#' @return A tibble
create_table_descriptive_stats <- function(data) {
  data %>%
    dplyr::summarise(
      dplyr::across(
        value,
        list(
          mean = mean, sd = sd,
          median = median, iqr = IQR
        )
      ),
      .by = metabolite
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(., 1)),
      mean_sd = glue::glue("{value_mean} ({value_sd})"),
      median_iqr = glue::glue("{value_median} ({value_iqr})")
    ) %>%
    dplyr::select(
      Metabolite = metabolite,
      `mean (SD)` = mean_sd,
      `median (IQR)` = median_iqr
    )
}

#' Histogram faceted by metabolite
#'
#' @param data Lipidomics data
#'
#' @returns A plot object
create_plot_distributions <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~metabolite, scales = "free") +
    ggplot2::theme_minimal()
}

#' Clean lipidomics data
#'
#' @param data Lipidomics data
#'
#' @returns a tibble
clean <- function(data) {
  data %>%
    dplyr::summarise(
      value = mean(value),
      .by = c(code, gender, age, class, metabolite)
    )
}

#' Preprocess by scaling
#'
#' @param data cleaned proteomics data
#'
#' @returns a tibble
preprocess <- function(data) {
  data %>%
    dplyr::mutate(
      class = factor(class),
      value = scale(value)
    )
}

#' Binomial mode
#'
#' @param data data frame with one metabolite
#' @param model formula
#'
#' @returns model results in tibble
fit_model <- function(data, model) {
  glm(model,
    data = data,
    family = binomial
  ) %>%
    broom::tidy(exponentiate = TRUE) %>%
    dplyr::mutate(.,
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = everything()
    )
}

#' Model results
#'
#' @param data lipidomics data
#'
#' @returns glm results
create_model_results <- function(data) {
  data %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(preprocess) %>%
    purrr::map(fit_all_models) %>%
    purrr::list_rbind()
}

#' Fit base and adjusted model
#'
#' @param data lipidomics
#'
#' @returns a tibble
fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + age + gender
  ) %>%
    purrr::map(\(model) fit_model(data, model = model)) %>%
    purrr::list_rbind()
}

#' Plot of model results
#'
#' @param results result data frame
#'
#' @returns plot
create_plot_model_results <- function(results) {
  results %>%
    dplyr::filter(term == "value" & std.error <= 2 & estimate <= 5) %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = metabolite)) +
    ggplot2::geom_pointrange(ggplot2::aes(
      xmin = estimate - std.error,
      xmax = estimate + std.error
    )) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::facet_grid(cols = ggplot2::vars(model))
}
