# Cool Data Story
# Creates a compact set of visual summaries from the data files in this folder.

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "gridExtra")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  stop(
    "Please install these packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(grid)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    if (file.exists(script_path)) {
      return(dirname(script_path))
    }
  }

  sourced_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(sourced_file)) {
    return(dirname(normalizePath(sourced_file)))
  }

  getwd()
}

find_data_dir <- function() {
  candidates <- unique(c(
    get_script_dir(),
    getwd(),
    "/Users/Travis/Desktop/spring 26/Data Matters /week 14"
  ))

  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "washPost.xlsx")) &&
        file.exists(file.path(candidate, "humanDataLab.csv"))
    ) {
      return(candidate)
    }
  }

  stop(
    "Could not find washPost.xlsx and humanDataLab.csv. ",
    "Put this R file in the same folder as the data, or set your working directory to that folder."
  )
}

data_dir <- find_data_dir()
setwd(data_dir)
out_dir <- file.path(data_dir, "r_outputs")
dir.create(out_dir, showWarnings = FALSE)

theme_story <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 5),
      plot.subtitle = element_text(color = "gray30", lineheight = 1.05),
      plot.caption = element_text(color = "gray45", size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

save_plot <- function(plot, filename, width = 11, height = 7) {
  ggsave(
    filename = file.path(out_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 320,
    bg = "white"
  )
}

wrap_label <- function(text, width = 95) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# ---------------------------------------------------------------------------
# Dataset 1: Washington Post police shootings workbook
# ---------------------------------------------------------------------------

wp <- read_excel(file.path(data_dir, "washPost.xlsx"), sheet = "data") %>%
  mutate(
    year = as.integer(year),
    race = case_when(
      race == "W" ~ "White",
      race == "B" ~ "Black",
      race == "A" ~ "Asian",
      race == "H" ~ "Hispanic",
      race == "N" ~ "Native American",
      race == "O" ~ "Other",
      race == "not reported" ~ "Not reported",
      is.na(race) | race == "" ~ "Unknown",
      TRUE ~ race
    ),
    armed_status = case_when(
      gun == "gun" ~ "Gun",
      gun == "unarmed" ~ "Unarmed",
      is.na(gun) | gun == "" ~ "Unknown",
      TRUE ~ "Other weapon"
    ),
    ageCat = if_else(is.na(ageCat) | ageCat == "", "Unknown", ageCat),
    threat_level = if_else(is.na(threat_level) | threat_level == "", "Unknown", threat_level),
    body_camera = if_else(is.na(body_camera), FALSE, body_camera)
  )

population_share <- read_excel(file.path(data_dir, "washPost.xlsx"), sheet = "perc") %>%
  mutate(race = if_else(race == "other", "Other", race))

race_summary <- wp %>%
  filter(race %in% population_share$race) %>%
  count(race, name = "fatal_count") %>%
  mutate(fatal_share = fatal_count / sum(fatal_count) * 100) %>%
  left_join(population_share, by = "race") %>%
  mutate(
    percentUsa19 = replace_na(percentUsa19, 0),
    representation_ratio = if_else(percentUsa19 > 0, fatal_share / percentUsa19, NA_real_),
    race = reorder(race, representation_ratio)
  )

write.csv(
  race_summary %>% arrange(desc(representation_ratio)),
  file.path(out_dir, "washpost_race_summary.csv"),
  row.names = FALSE
)

race_long <- race_summary %>%
  select(race, fatal_share, percentUsa19) %>%
  pivot_longer(
    cols = c(fatal_share, percentUsa19),
    names_to = "measure",
    values_to = "percent"
  ) %>%
  mutate(
    measure = recode(
      measure,
      fatal_share = "Share of recorded fatal shootings",
      percentUsa19 = "Share of U.S. population"
    )
  )

p_race <- ggplot(race_long, aes(x = percent, y = race, color = measure)) +
  geom_segment(
    data = race_summary,
    aes(x = fatal_share, xend = percentUsa19, y = race, yend = race),
    inherit.aes = FALSE,
    color = "gray78",
    linewidth = 1.1
  ) +
  geom_point(size = 4) +
  geom_text(
    data = race_summary,
    aes(
      x = pmax(fatal_share, percentUsa19) + 3.2,
      y = race,
      label = paste0(round(representation_ratio, 1), "x")
    ),
    inherit.aes = FALSE,
    color = "gray20",
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 75)) +
  scale_color_manual(values = c("#c0392b", "#2c7fb8")) +
  labs(
    title = "Representation gap by race",
    subtitle = wrap_label("Dots compare each race's share of recorded fatal police shootings with its share of the 2019 U.S. population. The label shows fatal-share divided by population-share."),
    x = "Percent of total",
    y = NULL,
    color = NULL,
    caption = "Source: washPost.xlsx"
  ) +
  theme_story()

save_plot(p_race, "washpost_race_representation_gap.png", width = 12, height = 7)

state_year <- wp %>%
  filter(!is.na(state), !is.na(year)) %>%
  count(state, year, name = "fatal_count")

top_states <- state_year %>%
  group_by(state) %>%
  summarize(total = sum(fatal_count), .groups = "drop") %>%
  slice_max(total, n = 15) %>%
  arrange(total)

state_year_top <- state_year %>%
  filter(state %in% top_states$state) %>%
  right_join(expand_grid(state = top_states$state, year = sort(unique(wp$year))), by = c("state", "year")) %>%
  mutate(
    fatal_count = replace_na(fatal_count, 0),
    state = factor(state, levels = top_states$state)
  )

p_heat <- ggplot(state_year_top, aes(x = year, y = state, fill = fatal_count)) +
  geom_tile(color = "white", linewidth = 0.45) +
  geom_text(aes(label = if_else(fatal_count == 0, "", as.character(fatal_count))), size = 3.1) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", labels = comma) +
  scale_x_continuous(breaks = sort(unique(state_year_top$year))) +
  labs(
    title = "Where the dataset is most concentrated",
    subtitle = wrap_label("Fatal police shooting records by year for the 15 states with the highest totals in this workbook."),
    x = NULL,
    y = NULL,
    fill = "Records",
    caption = "Source: washPost.xlsx"
  ) +
  theme_story() +
  theme(panel.grid = element_blank())

save_plot(p_heat, "washpost_top_states_year_heatmap.png", width = 12, height = 7.5)

body_camera_trend <- wp %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarize(
    records = n(),
    body_camera_share = mean(body_camera, na.rm = TRUE),
    .groups = "drop"
  )

p_camera <- ggplot(body_camera_trend, aes(x = year, y = body_camera_share)) +
  geom_area(fill = "#a6bddb", alpha = 0.7) +
  geom_line(color = "#045a8d", linewidth = 1.3) +
  geom_point(color = "#045a8d", size = 3) +
  scale_y_continuous(labels = label_percent(), limits = c(0, max(body_camera_trend$body_camera_share) * 1.2)) +
  scale_x_continuous(breaks = body_camera_trend$year) +
  labs(
    title = "Body-camera flag over time",
    subtitle = "Share of records marked TRUE for body camera by year.",
    x = NULL,
    y = "Share with body camera",
    caption = "Source: washPost.xlsx"
  ) +
  theme_story()

save_plot(p_camera, "washpost_body_camera_trend.png", width = 10, height = 6)

age_threat <- wp %>%
  filter(ageCat != "Unknown") %>%
  count(ageCat, threat_level, armed_status, name = "records") %>%
  group_by(ageCat, threat_level) %>%
  mutate(share = records / sum(records)) %>%
  ungroup()

p_bubbles <- ggplot(age_threat, aes(x = threat_level, y = ageCat, size = records, fill = armed_status)) +
  geom_point(shape = 21, color = "white", alpha = 0.88) +
  scale_size_area(max_size = 18, labels = comma) +
  scale_fill_manual(values = c("Gun" = "#b2182b", "Other weapon" = "#ef8a62", "Unarmed" = "#2166ac", "Unknown" = "gray60")) +
  labs(
    title = "Age, threat level, and weapon status",
    subtitle = wrap_label("Bubble size is the number of records for each age/threat combination; color separates weapon status."),
    x = "Threat level",
    y = "Age group",
    size = "Records",
    fill = "Armed status",
    caption = "Source: washPost.xlsx"
  ) +
  theme_story() +
  theme(panel.grid.major.x = element_line(color = "gray88"))

save_plot(p_bubbles, "washpost_age_threat_bubbles.png", width = 11, height = 7)

# ---------------------------------------------------------------------------
# Dataset 2: Human data lab CSV
# ---------------------------------------------------------------------------

human <- read.csv(file.path(data_dir, "humanDataLab.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    health_status = case_when(
      vaccinated & hadCovid ~ "Vaccinated + had COVID",
      vaccinated & !hadCovid ~ "Vaccinated + no COVID",
      !vaccinated & hadCovid ~ "Not vaccinated + had COVID",
      TRUE ~ "Not vaccinated + no COVID"
    ),
    majorCategory = if_else(is.na(majorCategory) | majorCategory == "", "Unknown", majorCategory)
  )

major_summary <- human %>%
  group_by(majorCategory) %>%
  summarize(
    students = n(),
    avg_missed_classes = mean(missedClasses, na.rm = TRUE),
    pct_vaccinated = mean(vaccinated, na.rm = TRUE),
    pct_had_covid = mean(hadCovid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_missed_classes))

write.csv(
  major_summary,
  file.path(out_dir, "human_major_summary.csv"),
  row.names = FALSE
)

p_human <- ggplot(human, aes(x = age, y = missedClasses, color = health_status)) +
  geom_jitter(width = 0.04, height = 0.12, size = 3.4, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "gray20", linewidth = 0.8) +
  facet_wrap(~ majorCategory, ncol = 2) +
  scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02", "#e7298a")) +
  labs(
    title = "Student absences by age and health status",
    subtitle = wrap_label("Each dot is one student. The line gives a quick overall trend, while facets show how patterns vary by major category."),
    x = "Age",
    y = "Missed classes",
    color = "Health status",
    caption = "Source: humanDataLab.csv"
  ) +
  theme_story(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray88")
  )

save_plot(p_human, "human_absence_health_facets.png", width = 11.5, height = 8.5)

# ---------------------------------------------------------------------------
# A one-page overview image
# ---------------------------------------------------------------------------

overview <- arrangeGrob(
  p_race + theme(legend.position = "bottom"),
  p_camera + theme(legend.position = "none"),
  p_human + theme(legend.position = "bottom"),
  ncol = 1,
  heights = c(1.05, 0.8, 1.25),
  top = textGrob("A quick data story from the week 14 files", gp = gpar(fontsize = 18, fontface = "bold"))
)

ggsave(
  filename = file.path(out_dir, "cool_data_story_overview.png"),
  plot = overview,
  width = 12,
  height = 18,
  dpi = 300,
  bg = "white"
)

if (interactive()) {
  grid.newpage()
  grid.draw(overview)
}

message("Done. Files written to: ", normalizePath(out_dir))
message("- cool_data_story_overview.png")
message("- washpost_race_representation_gap.png")
message("- washpost_top_states_year_heatmap.png")
message("- washpost_body_camera_trend.png")
message("- washpost_age_threat_bubbles.png")
message("- human_absence_health_facets.png")
message("- washpost_race_summary.csv")
message("- human_major_summary.csv")
