library("dplyr")
library("ggplot2")

freegal_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_freegal = mean(Checkouts)) %>% 
  filter(CheckoutType == "Freegal") %>% 
  select(CheckoutYear, avg_freegal)

hoopla_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_hoopla = mean(Checkouts)) %>% 
  filter(CheckoutType == "Hoopla") %>% 
  select(CheckoutYear, avg_hoopla)

horizon_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_horizon = mean(Checkouts)) %>% 
  filter(CheckoutType == "Horizon") %>% 
  select(CheckoutYear, avg_horizon)

overdrive_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear, CheckoutType) %>% 
  summarise(avg_overdrive = mean(Checkouts)) %>% 
  filter(CheckoutType == "OverDrive") %>% 
  select(CheckoutYear, avg_overdrive)

zinio_checkouts <- book_checkouts %>% 
  group_by(CheckoutYear,CheckoutType) %>% 
  summarise(avg_zinio = mean(Checkouts)) %>% 
  filter(CheckoutType == "Zinio") %>% 
  select(CheckoutYear, avg_zinio)

all_checkouts <- freegal_checkouts %>% 
  left_join(hoopla_checkouts, by = "CheckoutYear") %>% 
  left_join(horizon_checkouts, by = "CheckoutYear") %>% 
  left_join(overdrive_checkouts, by = "CheckoutYear") %>% 
  left_join(zinio_checkouts, by = "CheckoutYear")

ggplot(all_checkouts) +
  geom_line(aes(x = CheckoutYear, y = avg_freegal, color = "Freegal")) +
  geom_line(aes(x = CheckoutYear, y = avg_hoopla, color = "Hoopla")) +
  geom_line(aes(x = CheckoutYear, y = avg_horizon, color = "Horizon")) +
  geom_line(aes(x = CheckoutYear, y = avg_overdrive, color = "Overdrive")) +
  geom_line(aes(x = CheckoutYear, y = avg_zinio, color = "Zinio")) +
  labs(title = "Types of Checkout Over A Decade", x = "Year", y = "Verage # of Books Checked Out Per System", color = "Type of System") + 
  scale_x_continuous(breaks = seq(2013, 2023, 2))

