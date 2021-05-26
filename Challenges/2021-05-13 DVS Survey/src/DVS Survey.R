# Load libraries
pacman::p_load(tidyverse, here, readxl, tidytext, wordcloud2)

# Set Data Directories
challengeDir <- here::here("challenges", "2021-05-13 DVS Survey")
dataDir <- file.path(challengeDir, "data")
outputDir <- file.path(challengeDir, "output")

# Load Stephens' DVS Outlier Survey Data
origSurveyData <- read_excel(file.path(dataDir, "Outlier 2021 - Meet the other attendees.xlsx")) %>%
  rename(jobTitle = headline,
         okToShare = 3,
         interests = 4,
         lookingFor = 5,
         bio = 6,
         somethingInteresting = 7) %>%
  filter(!is.na(okToShare) & okToShare == "yes") %>%
  # Replace name with unique Responder ID
  mutate(responderID = row_number()) %>%
  select(-name) %>%
  mutate(jobTitle = na_if(jobTitle, "."))

# Get words mentioned in Bios and remove stop words
bioWords <- origSurveyData %>%
  unnest_tokens(word, bio) %>%
  select(responderID, word) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word))

# Get respondents interests 
interestWords <- origSurveyData %>%
  unnest_tokens(word, interests) %>%
  select(responderID, word) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word))

# Get words mentioned in Tell us something interesting about you
somethingInterestingWords <- origSurveyData %>%
  unnest_tokens(word, somethingInteresting) %>%
  select(responderID, word) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word))

# Get the basic survey data and where people are looking:
# - to Collaborate
# - for freelancers
# - for Projects
surveyData <- origSurveyData %>% 
  mutate(lookingTocollaborate = replace_na(str_detect(lookingFor, "collab"), FALSE),
         lookingForfreelancers = replace_na(str_detect(lookingFor, "freelance"), FALSE),
         lookingForProjects = replace_na(str_detect(lookingFor, "projects"), FALSE)) %>%
  select(responderID, jobTitle, lookingTocollaborate, lookingForfreelancers, lookingForProjects)

# Words in Respondents Bios 
# Only include words mentioned at least 5 times
wordcloud2(bioWords %>% count(word) %>% filter (n > 4), size = 3, minRotation = -pi/2, maxRotation = -pi/2)

# Respondents Interests 
wordcloud2(interestWords %>% count(word) %>% filter (n > 4), size = 3, minRotation = -pi/2, maxRotation = -pi/2)

# Words in Tell us Something Interesting
wordcloud2(somethingInterestingWords %>% count(word) %>% filter (n > 4), size = 3, minRotation = -pi/2, maxRotation = -pi/2)

# The following (putting words into a shape based on a png) doesn't seem to work even if you download the latest version of wordcloud2
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

figPath = system.file(file.path(dataDir, "Data+Visualization+Society+icon+2019-05-transparent.png"), package = "wordcloud2")
figPath = system.file("examples/t.png", package = "wordcloud2")

wordcloud2(bioWords %>% count(word) %>% filter (n > 3), figPath = figPath, size = 1.5, color = "skyblue")

