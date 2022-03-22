# Load libraries ---------------------------------------------------------------

library(bounceR)
library(dplyr)
library(showtext)
library(ggplot2)

# Retrieve data ----------------------------------------------------------------

# Match 21: Australia vs. South Africa
data <- get_hawkeye_stats(id = 22526)

# Tidy data --------------------------------------------------------------------

meg_boundaries <- data %>%
  janitor::clean_names() %>%
  filter(batter == "Meg Lanning") %>%
  filter(batter_runs >= 4) %>%
  mutate(
    xend = as.numeric(field_x),
    yend = as.numeric(field_y),
    x = 50,
    y = 50)

# Prep for plotting ------------------------------------------------------------

# Import Google fonts
font_add_google("Barlow Condensed", "barlow")

# Initiate showtext
showtext_auto()

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  ggforce::geom_ellipse(
    aes(x0 = 50, y0 = 50, a = 38, b = 38, angle = 0),
    fill = "#141414", colour = "#292929") +
  geom_segment(
    data = meg_boundaries,
    aes(x = x, y = y, xend = xend, yend = yend, colour = batter_runs)) +
  geom_point(
    data = meg_boundaries,
    aes(x = xend, y = yend, colour = batter_runs),
    size = 2) +
  scale_colour_manual(
    values = c("#EDBA45", "#45B0AE"),
    labels = c("4 runs", "6 runs")) +
  labs(
    x = NULL, y = NULL,
    title = "Run machine: Boundaries struck by **Meg Lanning**",
    subtitle = "#CWC22: South Africa vs. Australia - 22 March 2022",
    caption = "Data sourced from the **ICC** via the {bounceR} R package | Plot by **@jacquietran**") +
  coord_equal() +
  ggdark::dark_theme_void() +
  theme(
    legend.position = c(0.925, 0.1),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = rel(5)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = ggtext::element_markdown(
      size = rel(10), family = "barlow",
      margin = margin(0, 0, 5, 0, unit = "pt")),
    plot.subtitle = ggtext::element_markdown(
      size = rel(7), family = "barlow",
      margin = margin(0, 0, 10, 0, unit = "pt")),
    plot.caption = ggtext::element_markdown(
      size = rel(5), family = "barlow",
      margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/20220322_lanners.png"),
  last_plot(), width = 15, height = 15, units = "cm", dpi = 600)
