library(dplyr)
library(rvest)
library(RCurl)

setwd("~/DH101/Final")

# READING IN EXISTING DATA (Billboard 1964 - 2015) ------------------------
### data source: https://github.com/walkerkq/musiclyrics/blob/master/billboard_lyrics_1964-2015.csv

# data from above + genres added w/ Spotify API
old_billboard <- read.csv("songs_with_genres_cleaned.csv")

# cleaning the data a bit more
old_billboard <- old_billboard %>% rename(Genre = genre)
old_billboard <- old_billboard[,-6]

# WEB SCRAPING NEW DATA (Billboard 2016 - 2023) ---------------------------
### sources: wikipedia.org, letras.com

new_billboard <- data.frame(
  Rank = rep(seq(1:100), 8)
)

##### Song and Artist from Billboard Wikipedia pages #####
songcol <- list()
artist <- list()
for (i in 2016:2023){
  url <- paste("https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_", i, sep="")
  html <- read_html(url)
  songcol <- c(songcol, html %>% html_elements(".wikitable td:nth-child(2)") %>% html_element("a") %>% html_text2())
  artist <- c(artist, html %>% html_elements(".wikitable td:nth-child(3)") %>% html_element("a") %>% html_text2())
}
# artists that charted twice in a row were not included. manually adding.
artist <- artist %>% append("Justin Bieber", after = 1) %>% append("Twenty One Pilots", after = 20)

new_billboard$Song <- unlist(songcol)
new_billboard$Artist <- unlist(artist)
new_billboard$Year <- rep(seq(2016, 2023), each = 100)
new_billboard$Lyrics <- rep("NA", 800)
new_billboard$Source <- rep("NA", 800)

##### Lyrics from letras.com #####
first.letter <- list()
artist.name <- list()
song.name <- list()
for (i in 1:length(artist)){
  first.letter <- c(first.letter, tolower(new_billboard$Artist[i]) %>% substr(1, 1))
  artist.name <- c(artist.name, gsub("[^[:alnum:]]", "-", tolower(new_billboard$Artist[i])))
  song.name <- c(song.name, gsub("[^[:alnum:]]", "-", tolower(new_billboard$Song[i])))
}
first.letter <- unlist(first.letter)
artist.name <- unlist(artist.name)
song.name <- unlist(song.name)

# fix special accent chars
song.name <- iconv(song.name, "latin1", "ASCII", sub="")
artist.name <- iconv(artist.name, "latin1", "ASCII", sub="")

for (i in 1:nrow(new_billboard)){
  url.l <- paste("https://www.letras.com/", artist.name[i], "/", song.name[i], ".html", sep="")
  if (!isTRUE(url.exists(url.l))){
    next
  }
  html.l <- read_html(url.l)
  new_billboard$Lyrics[i] <- html.l %>% html_elements(".lyric-original p") %>% html_text2()
}

#write.csv(new_billboard, "new_songs_no_genre.csv", row.names=FALSE)
new.genres <- read.csv("songs_with_genres_2.csv") # data from above + genres added w/ Spotify API
new_billboard <- cbind(new.genres, Lyrics = new_billboard$Lyrics)
new_billboard <- new_billboard[,-(5:6)] %>% select(1, 2, 3, 4, 6, 5) %>% rename(Genre = genre)
all_billboard <- rbind(old_billboard, new_billboard)

# GENRE GROUPS ------------------------------------------------------------

# removing [ ] and ' from genre column
all_billboard$Lyrics <- gsub(pattern="[", replacement="", all_billboard$Lyrics, fixed=TRUE) %>% 
  gsub(pattern="'", replacement="", all_billboard$Lyrics, fixed=TRUE) %>% 
  gsub(pattern="]", replacement="", all_billboard$Lyrics, fixed=TRUE)

# splitting string of genres into individual genres
genre.list <- unlist(strsplit(all_billboard$Lyrics, ","))

# takes input of sub-genre phrases and groups them into one major genre (e.g. indie rock to rock)
split_genres <- function(subgenre, maingenre){
  for (i in 1:length(genre.list)){
    for (l in 1:length(subgenre)){
      if (grepl(subgenre[l], genre.list[i]) == TRUE){
        maingenre <- c(maingenre, genre.list[i])
        maingenre <- unlist(maingenre[!duplicated(maingenre)]) # removes duplicates
      }
    }
  }
}
# replacing list of sub-genres with their main genre throughout the data drame
replace_genre <- function(maingenre, name = ""){
  for (i in 1:nrow(all_billboard)){
    for (l in 1:length(maingenre)){
      if (grepl(maingenre[l], all_billboard[i, 6]) == TRUE){
        all_billboard[i, 6] <- name
      }
    }
  }
}

# rock
allrock <- list()
sub.rock <- c("rock", "grunge", "british", "cosmic american", "karaoke", "spanish invasion", "metal")
split_genres(sub.rock, allrock)
replace_genre(allrock, "Rock")

# soul
allsoul <- list()
sub.soul <- c("soul", "disco", "classic girl group", "beach music", "motown")
split_genres(sub.soul, allsoul)
replace_genre(allsoul, "Soul")

# country
allcountry <- list()
sub.country <- c("country", "nashville sound", "banjo")
split_genres(sub.country, allcountry)
replace_genre(allcountry, "Country")

# pop
allpop <- list()
sub.pop <- c("pop", "boy band", "easy listening", "adult standards", "surf music", "afrobeats", "doo-wop", "vocal harmony group", "neo mellow", "girl group", "mellow gold", "new wave")
split_genres(sub.pop, allpop)
replace_genre(allpop, "Pop")

# hip hop
allhiphop <- list()
sub.hiphop <- c("hip hop", "trap", "rap", "miami bass", "hip house")
split_genres(sub.hiphop, allhiphop)
replace_genre(allhiphop, "Hip Hop")

# r&b
allrnb <- list()
sub.rnb <- c("r&b", "rhythm and blues", "new jack swing", "freestyle")
split_genres(sub.rnb, allrnb)
replace_genre(allrnb, "R&B")

# other 
genres <- c("Rock", "Soul", "Country", "Pop", "R&B", "Hip Hop", "Genre not found")
for (i in 1:nrow(all_billboard)){
  if (!(all_billboard[i, 6] %in% genres)){
    all_billboard[i, 6] <- "Other"
  }
}

#write.csv(all_billboard, "billboard_1965-2023.csv")