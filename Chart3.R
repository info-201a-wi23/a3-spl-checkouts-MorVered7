library("dplyr")
library("ggplot2")


most_common <- book_checkouts %>% 
  group_by(Publisher, Creator) %>% 
  filter(Creator != "") %>% 
  summarise(Checkouts = sum(Checkouts)) %>% 
  arrange(desc(Checkouts))

top_5 <- head(most_common, 5)

ggplot(top_5, aes(x = Creator, y = Checkouts, color = Creator, fill = Creator)) +
  geom_bar(stat = "identity")+
  labs(title = "Top Authors by Number of Checkouts")
