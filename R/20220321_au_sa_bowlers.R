# Load libraries ---------------------------------------------------------------

# pak::pkg_install("robjhyndman/cricketdata")
library(cricketdata)
library(dplyr)
library(showtext)
library(ggplot2)

# Retrieve data ----------------------------------------------------------------

data <- fetch_cricsheet(type = "bbb", gender = "female", competition = "odis")

# Tidy data --------------------------------------------------------------------

# Extract match IDs for games played by Australia and South Africa
# up to 2022-03-21
selected_matches <- data %>%
  filter(start_date > "2022-03-03"& start_date < "2022-03-21") %>%
  filter(bowling_team %in% c("Australia", "South Africa")) %>%
  distinct(match_id) %>%
  pull()

data_tidy <- data %>%
  filter(match_id %in% selected_matches) %>%
  # Focus on bowling performances only
  filter(bowling_team %in% c("Australia", "South Africa")) %>%
  mutate(
    # Tidy up empty strings in wicket_type
    wicket_type = case_when(
      wicket_type == "" ~ NA_character_,
      TRUE             ~ wicket_type),
    # Create variable to count wickets taken by bowler (i.e., exclude run outs)
    wicket_by_bowler = case_when(
      is.na(wicket_type)       ~ NA_real_,
      wicket_type == "run out" ~ NA_real_,
      TRUE                     ~ 1)) %>%
  # Separate out over number into its own variable
  tidyr::separate(
    ball, sep = "\\.", into = c("over_in_match", "ball_in_over")) %>%
  mutate(
    # Add one because the first over is prefixed with 0 (e.g., 1st ball is 0.1)
    over_in_match = as.numeric(over_in_match) + 1,
    ball_in_over = as.numeric(ball_in_over))

# Summarise data: Australia ----------------------------------------------------

overs_bowled_australia <- data_tidy %>%
  filter(bowling_team == "Australia") %>%
  group_by(bowler, match_id) %>%
  distinct(over_in_match) %>%
  ungroup() %>%
  group_by(bowler) %>%
  summarise(overs_bowled = length(match_id)) %>%
  ungroup()

bowling_australia <- data_tidy %>%
  filter(bowling_team == "Australia") %>%
  group_by(bowler) %>%
  summarise(
    games_played = length(unique(match_id)),
    balls_bowled = length(match_id),
    wickets_total = sum(wicket_by_bowler, na.rm = TRUE),
    strike_rate_bowl = case_when(
      wickets_total > 0 ~ round(balls_bowled / wickets_total, 1),
      TRUE              ~ 0),
    runs_off_bat_conceded = sum(runs_off_bat),
    extras_conceded = sum(extras),
    runs_total_conceded = runs_off_bat_conceded + extras_conceded) %>%
  ungroup() %>%
  left_join(., overs_bowled_australia, by = "bowler") %>%
  mutate(
    runs_total_conceded_per_over = round(runs_total_conceded / overs_bowled, 2))

# Pivot to long format for plotting
bowling_australia_long <- bowling_australia %>%
  select(
    bowler, games_played, balls_bowled, wickets_total, strike_rate_bowl,
    runs_total_conceded, runs_total_conceded_per_over) %>%
  tidyr::pivot_longer(
    cols = -bowler,
    names_to = "key",
    values_to = "value") %>%
  mutate(
    key = factor(
      key, levels = c(
        "games_played", "balls_bowled", "wickets_total", "strike_rate_bowl",
        "runs_total_conceded", "runs_total_conceded_per_over")),
    bowler_mod = tidytext::reorder_within(bowler, value, key),
    label_xpos = case_when(
      key == "games_played"                 ~ value + 0.2,
      key == "balls_bowled"                 ~ value + 13,
      key == "wickets_total"                ~ value + 0.3,
      key == "strike_rate_bowl"             ~ value + 3.5,
      key == "runs_total_conceded"          ~ value + 10,
      key == "runs_total_conceded_per_over" ~ value + 0.4),
    value_label = case_when(
      value == 0 ~ NA_character_,
      TRUE       ~ as.character(value)))

# Summarise data: South Africa -------------------------------------------------

overs_bowled_south_africa <- data_tidy %>%
  filter(bowling_team == "South Africa") %>%
  group_by(bowler, match_id) %>%
  distinct(over_in_match) %>%
  ungroup() %>%
  group_by(bowler) %>%
  summarise(overs_bowled = length(match_id)) %>%
  ungroup()

bowling_south_africa <- data_tidy %>%
  filter(bowling_team == "South Africa") %>%
  group_by(bowler) %>%
  summarise(
    games_played = length(unique(match_id)),
    balls_bowled = length(match_id),
    wickets_total = sum(wicket_by_bowler, na.rm = TRUE),
    strike_rate_bowl = case_when(
      wickets_total > 0 ~ round(balls_bowled / wickets_total, 1),
      TRUE              ~ 0),
    runs_off_bat_conceded = sum(runs_off_bat),
    extras_conceded = sum(extras),
    runs_total_conceded = runs_off_bat_conceded + extras_conceded) %>%
  ungroup() %>%
  left_join(., overs_bowled_south_africa, by = "bowler") %>%
  mutate(
    runs_total_conceded_per_over = round(runs_total_conceded / overs_bowled, 2))

# Pivot to long format for plotting
bowling_south_africa_long <- bowling_south_africa %>%
  select(
    bowler, games_played, balls_bowled, wickets_total, strike_rate_bowl,
    runs_total_conceded, runs_total_conceded_per_over) %>%
  tidyr::pivot_longer(
    cols = -bowler,
    names_to = "key",
    values_to = "value") %>%
  mutate(
    key = factor(
      key, levels = c(
        "games_played", "balls_bowled", "wickets_total", "strike_rate_bowl",
        "runs_total_conceded", "runs_total_conceded_per_over")),
    bowler_mod = tidytext::reorder_within(bowler, value, key),
    label_xpos = case_when(
      key == "games_played"                 ~ value + 0.15,
      key == "balls_bowled"                 ~ value + 15,
      key == "wickets_total"                ~ value + 0.5,
      key == "strike_rate_bowl"             ~ value + 7,
      key == "runs_total_conceded"          ~ value + 10,
      key == "runs_total_conceded_per_over" ~ value + 0.35),
    value_label = case_when(
      value == 0 ~ NA_character_,
      TRUE       ~ as.character(value)))

# Prep for plotting ------------------------------------------------------------

# Set custom strip text labels
key_labels <- bowling_australia_long %>%
  distinct(key) %>%
  mutate(
    label = case_when(
      stringr::str_detect(key, "games")    ~ "Games played",
      stringr::str_detect(key, "bowled")   ~ "Balls bowled",
      stringr::str_detect(key, "wickets")  ~ "Wickets",
      stringr::str_detect(key, "strike")   ~ "Strike rate",
      key == "runs_total_conceded"         ~ "Runs conceded",
      stringr::str_detect(key, "per_over") ~ "Runs per over")) %>%
  tibble::deframe()

# Import Google fonts
font_add_google("Oswald", "oswald")
font_add_google("Barlow Condensed", "barlow")

# Initiate showtext
showtext_auto()

# Build plot: Australia --------------------------------------------------------

# Define custom colours
colours_australia <- c(
  "#579D43", "#7CB045", "#9FBD43", "#C4CA53", "#E8DC73", "#F1D755", "#FFD21F",
  "#F2B202", "#E09D00", "#B88100")

# Build plot
p <- ggplot() +
  geom_bar(
    data = bowling_australia_long,
    aes(x = value, y = bowler_mod, fill = bowler),
    width = 0.75, stat = "identity") +
  geom_text(
    data = bowling_australia_long,
    aes(x = label_xpos, y = bowler_mod, label = value_label),
    size = 15, family = "barlow") +
  facet_wrap(
    ~key, nrow = 2, scales = "free", labeller = labeller(key = key_labels)) +
  tidytext::scale_y_reordered() +
  scale_fill_discrete(
    type = colours_australia) +
  labs(
    x = NULL, y = NULL,
    title = "**Australia**'s bowling performances through 5 games of #CWC22",
    caption = "Data from **Cricsheet.org** via the {cricketdata} R package | Plot by **@jacquietran**") +
  ggdark::dark_mode() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    plot.title = ggtext::element_markdown(
      size = rel(10), family = "barlow",
      margin = margin(0, 0, 10, 0, unit = "pt")),
    strip.text = element_text(size = rel(7), family = "barlow"),
    axis.text = element_text(
      size = rel(5), family = "barlow", colour = "#FFFFFF"),
    axis.ticks = element_blank(),
    plot.caption = ggtext::element_markdown(
      size = rel(5), family = "barlow",
      margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/20220321_australia_bowling.png"),
  last_plot(), width = 5760, height = 3240, units = "px", dpi = 600)

# Build plot: South Africa -----------------------------------------------------

# Define custom colours
colours_south_africa <- c(
  "#00664D", "#008F6B", "#32AE55", "#62BE37", "#87D534", "#A1E326")

# Build plot
p <- ggplot() +
  geom_bar(
    data = bowling_south_africa_long,
    aes(x = value, y = bowler_mod, fill = bowler),
    width = 0.75, stat = "identity") +
  geom_text(
    data = bowling_south_africa_long,
    aes(x = label_xpos, y = bowler_mod, label = value_label),
    size = 18, family = "barlow") +
  facet_wrap(
    ~key, nrow = 2, scales = "free", labeller = labeller(key = key_labels)) +
  tidytext::scale_y_reordered() +
  scale_fill_discrete(
    type = colours_south_africa) +
  labs(
    x = NULL, y = NULL,
    title = "**South Africa**'s bowling performances through 4 games of #CWC22",
    caption = "Data from **Cricsheet.org** via the {cricketdata} R package | Plot by **@jacquietran**") +
  ggdark::dark_mode() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    plot.title = ggtext::element_markdown(
      size = rel(10), family = "barlow",
      margin = margin(0, 0, 10, 0, unit = "pt")),
    strip.text = element_text(size = rel(7), family = "barlow"),
    axis.text = element_text(
      size = rel(5), family = "barlow", colour = "#FFFFFF"),
    axis.ticks = element_blank(),
    plot.caption = ggtext::element_markdown(
      size = rel(5), family = "barlow",
      margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"))

# Export to file
ggsave(
  here::here("img/20220321_south_africa_bowling.png"),
  last_plot(), width = 5760, height = 3240, units = "px", dpi = 600)
