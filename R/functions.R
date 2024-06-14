create_gt_table <- function(table_profondeur) {
  table_profondeur |>
    gt() |>
    tab_header(
      title = md("**Température de l'eau à Hermance**"),
    ) |>
    tab_spanner_delim(delim = " ") %>%
    data_color(columns = -profondeur, palette = "Blues")  |>
    fmt(columns = profondeur,
        fns = \(x) paste(x, "m"))
}
