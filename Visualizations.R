library(dplyr)
library(ggplot2)

setwd("~/DH101/Final")

all_billboard <- read.csv("billboard_1965-2023.csv")
all_billboard <- all_billboard[,-1]

# Genre Distribution
all_billboard %>% ggplot(aes(x = Genre, fill = Genre)) + 
  geom_bar() + 
  ggtitle("Billboard Year-End Hot 100 Distribution of Genre") + 
  labs(y = "Frequency")

# Genre Distribution by Decade
decade <- all_billboard %>% mutate(Decade = floor(Year / 10) * 10) %>% group_by(Decade) %>% select(-Year)
decade %>% ggplot(aes(x = Decade, fill = Genre)) + 
  geom_bar() + ggtitle("Distribution of Genre per Decade")+ labs(y = "Frequency")

# Genre Trend Over Time
all_billboard %>% mutate(Year = as.factor(Year)) %>% 
  group_by(Year, Genre) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  ggplot(aes(x = Year, y = count, color = Genre, group = Genre)) + 
  geom_point() + geom_line() + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) + 
  ggtitle("Genre Trends from 1965 to 2023") + labs(y = "Frequency")

# Genre Proportions in Billboard Top 100 Before + After Start of Hip Hop
before73 <- all_billboard %>% filter(Year < 1973) %>% group_by(Genre) %>%
  summarize(count = n()) %>% summarize(Genre,prop = count/sum(count)) %>% mutate(before = 1)

after73 <- all_billboard %>% filter(Year >= 1973) %>% group_by(Genre) %>%
  summarize(count = n()) %>% summarize(Genre, prop = count/sum(count)) %>% mutate(before = 0)

rbind(before73, after73) %>% ggplot(aes(x = reorder(Genre,prop), y = prop, group = before, fill = before)) + 
  geom_col(position = "stack") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Genre", y="Proportion")+ coord_flip() + ggtitle("Genre Proportions Before and After 1973")
