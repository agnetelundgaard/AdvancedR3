#' Create descriptive table
#'
#' @param data Lipidomics data
#'
#' @return A tibble
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::summarise(mean = mean(value), sd = sd(value), .by = metabolite) |>
    dplyr::mutate(dplyr::across(c(mean, sd), ~ round(., 1)),
                  mean_sd = glue::glue("{mean} ({sd})")
    ) |>
    dplyr::select(Metabolite = metabolite, `mean (SD)` = mean_sd)
}

create_table_descriptive_stats(lipidomics)
