library("dplyr")
library("ggplot2")

freegal_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_freegal = mean(Checkouts)) %>% 
  filter(CheckoutType == "Freegal")

hoopla_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_hoopla = mean(Checkouts)) %>% 
  filter(CheckoutType == "Hoopla")

horizon_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_horizon = mean(Checkouts)) %>% 
  filter(CheckoutType == "Horizon")

overdrive_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_overdrive = mean(Checkouts)) %>% 
  filter(CheckoutType == "OverDrive")

zinio_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear,CheckoutType) %>% 
  summarise(avg_zinio = mean(Checkouts)) %>% 
  filter(CheckoutType == "Zinio")

ggplot() +
  geom_line(freegal_checkouts, mapping = aes(x = CheckoutYear, y = avg_freegal), color = "Blue") +
  geom_line(hoopla_checkouts, mapping = aes(x = CheckoutYear, y = avg_hoopla), color = "Red") +
  geom_line(horizon_checkouts, mapping = aes(x = CheckoutYear, y = avg_horizon), color = "saddlebrown") +
  geom_line(overdrive_checkouts, mapping = aes(x = CheckoutYear, y = avg_overdrive), color = "brown4") +
  geom_line(zinio_checkouts, mapping = aes(x = CheckoutYear, y = avg_zinio), color = "hotpink1") 
