###################################
#
# R Café plot-a-thon submission
# Author: Claudiu Forgaci
# Date: 9 October 2023
#
###################################


# Packages ----------------------------------------------------------------
library(tidyverse)    # Wrangle and visualise data
library(here)         # Manage paths
library(jsonlite)     # Read JSON data
# library(fontawesome)  # Generate fontawesome icons
library(showtext)     # Customise fonts in plots
library(tidytext)     # Manipulate text
library(patchwork)    # Create layouts for ggplot
library(ggwordcloud)  # Create word clouds with ggplot
library(wordcloud)    # Create wordclouds
# library(magick)       #
library(ggtext)

# Data --------------------------------------------------------------------
## Load data from file
data <- fromJSON(here("data", "datasets.json"))
groups <- fromJSON(here("data", "groups.json"))
views <- read_tsv(here("data", "views_downloads.tsv"))

## Prepare data for visualisation
df <- data |>
  as.tibble() |>
  filter(!is.na(title)) |>
  left_join(groups, by = c("group_id" = "id")) |>
  left_join(views, by = c("uuid" = "uuid")) |>
  mutate(year = lubridate::year(published_date),
         has_doi = str_detect(string = resource_doi, pattern = "\\b(10[.])") |>
           replace_na(replace = FALSE),
         type = defined_type_name,
         doi = resource_doi,
         inst = str_remove_all(name, " students| Students| STUDENTS") |>
           str_replace_all(c("4TU.ResearchData" = "4TU.RD",
                             "Erasmus University Rotterdam" = "EUR",
                             "Delft University of Technology" = "TUD",
                             "University of Twente" = "UTwente",
                             "Wageningen University and Research" = "WUR",
                             "NIOZ Royal Netherlands Institute for Sea Research" = "NIOZ",
                             "Eindhoven University of Technology" = "TU/e")),
         is_4tu = if_else(inst %in% c("4TU.RD", "TUD", "WUR", "UTwente", "TU/e"), "4TU", "Other")) |>
  # select(-starts_with(c("embargo", "url", "defined_type")),
  #        -c(id, timeline, handle, resource_doi, thumb,
  #           published_date, resource_title, group_id))
  select(uuid, title, doi, has_doi, year, type, inst, is_4tu, views, downloads) |>
  filter(!duplicated(uuid), !is.na(year))  # Drop duplicate entries

## Check number of records removed due to incomplete information or duplication
n_records_removed <- nrow(data) - nrow(df)

# Words -------------------------------------------------------------------
## Unnest words from titles
### Note that ngrams with n>=2 are not useful only using the titles as input,
### as title repetitions can make ngram frequencies meaningless
df_words <- df |>
  select(uuid, title, inst) |>
  mutate(
    title_sub = str_sub(title, start = 1, end = 20)) |>
  filter(!duplicated(title_sub)) |>
  mutate(
    title = str_remove_all(title,
                           pattern = paste("underlying", "data", "Data",
                                           "code", "based", "publication",
                                           "study", "research", "with", "paper",
                                           "set", "analysis", "scripts",
                                           sep = "|")) |>
      str_replace_all("[:digit:]", "")) |>
  unnest_tokens(word, title, drop = FALSE) |>
  anti_join(stop_words)

## Check top word frequencies
table(df_words$word) |>
  sort(decreasing = TRUE) |>
  head(100)

## Create word count dataframe
df_words_count <- df_words |>
  count(word) |>
  arrange(desc(n)) |>
  filter(n > 20)

# Plots -------------------------------------------------------------------
## Prerequisites for visualisation
### Customise fonts
font_add_google("Source Sans Pro", family = "sans-serif")
showtext_auto()

### Customise colours
custom_colors_1 <- c("#f49120", "#925713")
custom_colors_2 <- c("#AAAAAA", "#f49120")
custom_colors_3 <- c("#AAAAAA", "#f49120", "#925713")
custom_colors_4 <- c("#F49120", "#925713", "#F3665E",
                     "#CA5887", "#885A94", "#4A5680",
                     "#2F4858", "#AAAAAA")

## Type of publications over the years
p1 <- ggplot(df) +
  geom_bar(aes(x = as.factor(year), fill = type)) +
  scale_fill_manual(name = "Publication Type",
                    values = custom_colors_1,
                    label = c("Dataset", "Software")) +
  labs(title = "<b style='font-family: sans-serif; color: #f49120'>Datasets </b>&nbsp;&nbsp;&nbsp;   and   &nbsp;&nbsp;&nbsp;<b style='font-family: sans-serif; color: #925713'> software </b> &nbsp;&nbsp;&nbsp; over the years",
       x = NULL, y = NULL) +
  theme(plot.title = element_markdown(),
        axis.ticks = element_line(size = 0.2),
        legend.position="none")

p1

p1 + facet_wrap(~ reorder(inst, is_4tu, decreasing = TRUE))

## Publications with DOI over the years
p2 <- ggplot(df) +
  geom_bar(aes(x = as.factor(year), fill = has_doi)) +
  scale_fill_manual(values = custom_colors_2) +
  labs(title = "Publications with DOI over the years",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(family="sans-serif"),
        axis.ticks = element_line(size = 0.2),
        legend.position="none")

p2

p2 + facet_wrap(~ inst)

# Publications by institution
p3 <- ggplot(df |> filter(!is.na(inst))) +
  geom_bar(aes(x = fct_relevel(fct_rev(fct_infreq(inst)),
                               "Other institutions") |>
                 fct_relevel("4TU"),
               fill = is_4tu)) +
  # scale_fill_manual(values = custom_colors) +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "",
                    values = custom_colors_2,
                    label = c("Other institutions", "4TU"),
                    breaks = c("Other", "4TU")) +
  labs(title = "Publications per institution",
       subtitle = "Within and outside the 4TU",
       x = NULL, y = "Count") +
  theme(plot.title = element_text(family="sans-serif"),
        axis.ticks = element_line(size = 0.2)) +
  coord_flip()

p3

# Word cloud
## Try wordcloud with word cloud package
wordcloud(df_words_count$word,
          df_words_count$n,
          min.freq = 20,
          colors = custom_colors_3)

## With ggwordcloud package
p4 <- ggplot(df_words_count |>
               filter(n > 20), aes(label = word, size = n), color = "#f49120") +
  geom_text_wordcloud_area(area_corr_power = 1, rm_outside = TRUE,
                           eccentricity = .35) +
  scale_size_area(max_size = 10) +
  theme_minimal()

p4

## Add 4TU.ResearchData logo to layout
url <- "https://data.4tu.nl/static/images/logo.png"

img <- magick::image_read(url) %>%
  grid::rasterGrob()

p5 <- ggplot() +
  annotation_custom(img) +
  theme_minimal() +
  labs(x = element_blank(),
       y = element_blank())

p5

## Create empty plot for layout
p6 <- ggplot() +
  theme_minimal()

p6

## Show composite plot
design <- c("AABBB
             CDDDD")

p1 + p2 + p3 + p4 +
  plot_layout(design = design) +
  plot_annotation(title = '<img src="https://data.4tu.nl/static/images/logo.png" width="200">',
                  subtitle = "\n                               in a nutshell!",
                  caption = "Data source: 4TU.ResearchData",
                  theme = theme(plot.title = element_markdown(lineheight = 1.1))) &
  theme(text = element_text(family="sans-serif"))

ggsave("4tu-stats.png", width = 1920, height = 1080, units = "px")
