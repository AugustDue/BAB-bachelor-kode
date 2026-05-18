
# Sørg for at year_month er en rigtig dato
euribor_ois_plot <- euribor_ois %>%
  mutate(
    year_month = as.Date(paste0(year_month, "-01"))
  ) %>%
  arrange(year_month) %>%
  mutate(
    rolling_vol_12m = rollapply(
      euribor_ois,
      width = 12,
      FUN = sd,
      fill = NA,
      align = "right",
      na.rm = TRUE
    )
  )

plot_data <- euribor_ois_plot %>%
  select(year_month, euribor_ois, rolling_vol_12m) %>%
  pivot_longer(
    cols = c(euribor_ois, rolling_vol_12m),
    names_to = "serie",
    values_to = "værdi"
  ) %>%
  mutate(
    serie = recode(
      serie,
      euribor_ois = "EURIBOR-EONIA spread",
      rolling_vol_12m = "12-måneders rullende volatilitet"
    )
  )

ggplot(plot_data, aes(x = year_month, y = værdi)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ serie, scales = "free_y", ncol = 1) +
  labs(
    title = "EURIBOR-EONIA spread og rullende volatilitet",
    x = "Tid",
    y = ""
  ) +
  theme_minimal()
