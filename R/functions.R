# load magrittr pipe
`%>%` <- magrittr::`%>%`

#' Create descriptive table
#'
#' @param data Lipidomics data
#'
#' @return A tibble
create_table_descriptive_stats <- function(data) {
  data %>%
    dplyr::summarise(mean = mean(value), sd = sd(value), .by = metabolite) %>%
    dplyr::mutate(dplyr::across(c(mean, sd), ~ round(., 1)),
      mean_sd = glue::glue("{mean} ({sd})")
    ) %>%
    dplyr::select(Metabolite = metabolite, `mean (SD)` = mean_sd)
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
