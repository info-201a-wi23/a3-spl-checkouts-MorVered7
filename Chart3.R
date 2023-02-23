library("dplyr")
library("ggplot2")

book_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_book = mean(Checkouts)) %>% 
  filter(MaterialType == "BOOK")

audiobook_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_audiobook = mean(Checkouts)) %>% 
  filter(MaterialType == "AUDIOBOOK")

sounddisc_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_sounddisc = mean(Checkouts)) %>% 
  filter(MaterialType == "SOUNDDISC")

ebook_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_ebook = mean(Checkouts)) %>% 
  filter(MaterialType == "EBOOK")

cr_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_cr = mean(Checkouts)) %>% 
  filter(MaterialType == "CR")

videodisc_material <- book_checkouts %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(avg_videodisc = mean(Checkouts)) %>% 
  filter(MaterialType == "VIDEODISC")



most_common <- book_checkouts %>% 
  group_by(Publisher, Creator) %>% 
  filter(Creator != "") %>% 
  summarise(Checkouts = sum(Checkouts)) %>% 
  arrange(desc(Checkouts))

top_5 <- head(most_common, 5)
  
  
ggplot(top_5, aes(x = Creator, y = Checkouts, color = Creator, fill = Creator)) +
  geom_bar(stat = "identity")

ggplot() +
  geom_line(book_material, mapping = aes(x = CheckoutYear, y = avg_book), color = "Blue") +
  geom_line(audiobook_material, mapping = aes(x = CheckoutYear, y = avg_audiobook), color = "Red") +
  geom_line(sounddisc_material, mapping = aes(x = CheckoutYear, y = avg_sounddisc), color = "salmon1") +
  geom_line(ebook_material, mapping = aes(x = CheckoutYear, y = avg_ebook), color = "saddlebrown") +
  geom_line(cr_material, mapping = aes(x = CheckoutYear, y = avg_cr), color = "brown4") +
  geom_line(videodisc_material, mapping = aes(x = CheckoutYear, y = avg_videodisc), color = "hotpink1") 
