library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

df <- read.csv('keywords_zedoaria_20211231.csv')

kw_years <- df %>% select(Year,Author.Keywords,Cited.by)

kw_years %>% group_by(Author.Keywords) %>% 
  summarise(cited = sum(Cited.by)) %>% 
  filter(cited > 10)



p <- kw_years %>% 
  separate_rows(Author.Keywords, sep = '; ') %>% 
  replace(.,is.na(.),0) %>% 
  group_by(Year, Author.Keywords) %>% 
  summarise(docs = n(),
            tot_cit = sum(Cited.by)) %>% 
  arrange(-docs) %>% filter(Author.Keywords != '') %>%
  filter(docs > 2) %>% 
  ggplot(aes(y = reorder(Author.Keywords, -docs), x = Year, fill = as.factor(docs))) +
  geom_tile();ggplotly(p)


p <- kw_years %>% 
  separate_rows(Author.Keywords, sep = '; ') %>% 
  replace(.,is.na(.),0) %>% 
  group_by(Year, Author.Keywords) %>% 
  summarise(docs = n(),
            tot_cit = sum(Cited.by)) %>% 
  arrange(-docs) %>% filter(Author.Keywords != '') %>% 
  ggplot(aes(y = docs, color = as.factor(Year), x = tot_cit, label = Author.Keywords)) +
  geom_point()
ggplotly(p)

p <- kw_years %>% 
  separate_rows(Author.Keywords, sep = '; ') %>% 
  replace(.,is.na(.),0) %>% 
  group_by(Author.Keywords) %>% 
  summarise(docs = n(),
            tot_cit = sum(Cited.by),
            myear = max(Year)) %>% 
  arrange(-docs) %>% filter(Author.Keywords != '') %>% 
  ggplot(aes(y = docs, color = as.factor(myear), x = tot_cit, label = Author.Keywords)) +
  geom_point();ggplotly(p)

p <- kw_years %>% 
  separate_rows(Author.Keywords, sep = '; ') %>% 
  replace(.,is.na(.),0) %>% 
  group_by(Year, Author.Keywords) %>% 
  summarise(docs = n(),
            tot_cit = sum(Cited.by)) %>% 
  arrange(-docs) %>% filter(Author.Keywords != '') %>%
  ggplot(aes(x = as.factor(Year), y = tot_cit)) +
  geom_boxplot(outlier.colour="red");ggplotly(p)
p

kw_years %>% 
  separate_rows(Author.Keywords, sep = '; ') %>%  
  mutate(Author.Keywords = tolower(Author.Keywords)) %>% 
  group_by(Author.Keywords) %>% 
  summarise(cont = n()) %>% 
  arrange(-cont) %>% 
  write.csv('keywords.csv')


df %>% separate_rows(Author.Keywords, sep = '; ') %>% 
  filter(Author.Keywords == 'Sesquiterpenes')
