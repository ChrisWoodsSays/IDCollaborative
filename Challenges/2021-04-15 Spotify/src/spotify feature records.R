# Load libraries
pacman::p_load(tidyverse, stringi, here,
               patchwork, showtext, ggtext, colorspace)

# Get Fonts
font_add_google("Open Sans", "open sans")
font_add_google("Share Tech Mono", "techmono")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Set Data Directories
challengeDir <- here::here("challenges", "2021-04-15 Spotify")
dataDir <- file.path(challengeDir, "data")
outputDir <- file.path(challengeDir, "output")

# Load Stephens' Data
audioFeatures <- readRDS(file.path(dataDir, "SpotifyAudioFeatures.RDS"))
artists <- readRDS(file.path(dataDir, "artists_by_genre_top10_final.RDS"))

# Join Audio Features to Artists and select genre and audio feature columns
tracks <- audioFeatures %>%
  inner_join(artists, by = c("artist_id" = "id"), suffix = c(".t", ".a")) %>%
  select(genre, danceability, loudness, energy, speechiness, acousticness, instrumentalness, instrumentalness, liveness, valence, tempo)

# Normalise all values to between 0 and 1 based on the max and min for each feature
normalise <- function(x, min, max){
  return(round((x-min(x)) / (max(x)-min(x)),2))
}

# Set value to exclude outliers
ss = 0.05

# Get audio features values in long format ready for charrting
rangesByGenre <- tracks %>%
  pivot_longer(cols=c(danceability, loudness, energy, speechiness, acousticness, instrumentalness, instrumentalness, liveness, valence, tempo),
               names_to = "measure",
               values_to = "value") %>%
  group_by(measure) %>%
  mutate(minForMeasure = min(value),
         maxForMeasure = max(value),
         valueAdjusted = normalise(value, minForMeasure, maxForMeasure)) %>%
  group_by(genre, measure) %>%
  summarise(min = min(valueAdjusted), max = max(valueAdjusted),
            lowerQuartile = quantile(valueAdjusted, ss),
            upperQuartile = quantile(valueAdjusted, 1 - ss)) %>%
  mutate(measureID = row_number(),
         genre = stringr::str_to_title(genre),
         measure = stringr::str_to_title(measure))

# Get palette
palette <- rainbow(9)

# Function to add record white lines as a legend
add_lines <- function(id){
  annotate("segment", x = 2.05 + id * 0.2, xend = 2.05 + id * 0.2, y = 0, yend = 0.995, size = 0.2, colour="#D8E6F2") 
}

# Plot
g <- ggplot(rangesByGenre) +
  add_lines(seq(1,9,1)) +
  geom_rect(aes(ymin=lowerQuartile, ymax=upperQuartile, xmin = 2 + 0.2 * measureID, xmax= 2.1 + 0.2 * measureID, fill = measure)) +
  scale_fill_manual(values = palette) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  ylim(c(0, 1)) +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~ genre, nrow = 3) +
  theme(plot.background = element_rect(fill = "#1A2E40", color = NA),
        strip.text.x = element_text(family = "open sans", face = "plain", size = 12, color = "#D8E6F2", margin = margin(0.2,0,0.2,0, "cm")),
        plot.margin = margin(30,0,0,0),
        legend.position = c(0.8, 0.15),
        legend.text = element_text(family = "open sans", face = "plain", size = 8, color = "#D8E6F2"),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol=3, override.aes = list(shape = 1))) +
  plot_annotation(
    title = "Spotify Genre Audio Features",
    subtitle = "Each circle represents the relative range* of values for one of nine Spotify audio features",
    caption = "* 5th to 95th percentile  |  Data Collective Challenge 1 - Spotify  |  @ChrisWoodsSays  | Data: SpotifyR",
    theme = theme(
      plot.background = element_rect(fill = "#1A2E40"),
      plot.title = element_text(family = "techmono", size = 18, color = "#D8E6F2", hjust = 0.5, face = "bold", margin = margin(10,0,5,0)),
      plot.subtitle = element_textbox_simple(family = "open sans", color = "#D8E6F2", size = 8, halign = 0.5),
      plot.caption = element_text(family = "techmono", color = "#D8E6F2", size = 10, hjust = 0.98)      
    )
  )

# Save plot fo posterity
ragg::agg_png(here::here("challenges", "2021-04-15 Spotify", "output", "images", paste0("Spotify", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 320, width = 29.65, height = 21, units = "cm")
g
dev.off()

