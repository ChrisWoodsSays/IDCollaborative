devtools::install_github('charlie86/spotifyr')
install.packages("genius")
library(spotifyr)
library(genius)
pacman::p_load(tidyverse, stringi, 
               here, spotifyr)

# Get Audio Features for all Abba albums
abbaAudioFeatures <- get_artist_audio_features('abba')
# Get Album Names
abbaAlbums <- abbaAudioFeatures %>%
  group_by(album_name) %>%
  summarise()
abbaAlbumsCharVector <- abbaAlbums[['album_name']]

# Get artist data
abbaID <- abbaAudioFeatures[1,]$artist_id
abbaArtist <- get_artist(abbaID)
# Get album data including lyrics for all Abba albums
abbaAlbumData <- get_album_data("ABBA", abbaAlbumsCharVector)


class(x)



Sys.setenv(SPOTIFY_CLIENT_ID = '04f1c6ca2dee40d389d7428cb9eb5478')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e131b15be80c4ce5995ad3c6e5c55c35')

access_token <- get_spotify_access_token()


beatles <- get_artist_audio_features('the beatles')
library(tidyverse)
library(knitr)

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

challengeDir <- here::here("challenges", "2021-04-15 Spotify")
dataDir <- file.path(challengeDir, "data")
outputDir <- file.path(challengeDir, "output")


spotifyAD <- readRDS(file.path(dataDir, "SpotifyAudioFeatures.RDS"))