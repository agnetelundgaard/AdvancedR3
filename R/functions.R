# load magrittr pipe
`%>%` <- magrittr::`%>%`

#' Create descriptive table
#'
#' @param data Lipidomics data
#'
#' @return A tibble
create_table_descriptive_stats <- function(data) {
  data %>%
    dplyr::summarise(dplyr::across(value,
                            list(mean = mean, sd = sd,
                                 median = median, iqr = IQR)),
                            .by = metabolite) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 1)),
                  mean_sd = glue::glue("{value_mean} ({value_sd})"),
                  median_iqr = glue::glue("{value_median} ({value_iqr})")
                  ) %>%
    dplyr::select(Metabolite = metabolite,
                  `mean (SD)` = mean_sd,
                  `median (IQR)` = median_iqr)
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
clean <- function(data){
  data %>%
    dplyr::summarise(value = mean(value),
                     .by = c(code, gender, age, class, metabolite))
}
