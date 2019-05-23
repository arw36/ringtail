# load libraries 
library(tidyverse)
library(tuber)

# Conduct Youtube Search for videos 
p <- yt_oauth(app_id = "552330645856-jki3jjar0kpmbskkdodkkaok30r81002.apps.googleusercontent.com", 
         app_secret = "CfV2ydF6zP-20f1AdCk5aNVr", 
         token = "")
yt_videos1 <- yt_search(term = "ringtail") 
yt_videos2 <- yt_search(term = "miners cat")
yt_videos3 <- yt_search(term = "cacomistle")
yt_videos4 <- yt_search(term = "ring-tail cat")
# yt_videos5 <- yt_search(term = "ringtail cat")  # haven't included yet
# yt_videos6 <- yt_search(term = "Bassaricus astutus")  # haven't included yet
# yt_videos7 <- yt_search(term = "Bassaricus sumichrasti")  # haven't included yet

# merge all searches together and remove duplicates 
yt_videos <- bind_rows(yt_videos1, yt_videos2, yt_videos3, yt_videos4) %>% 
  unique()

# save videos then upload to google sheets for manual filtering
# write.csv(yt_videos, file = "data/youtube-results.csv")