library("dplyr")
library("ggplot2")

book_checkouts <- read.csv("C:/Users/mvere/OneDrive/Documents/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)


physical_values <- book_checkouts %>% 
  group_by(CheckoutYear) %>% 
  filter(UsageClass == "Physical") %>% 
  summarise(avg_physical = mean(Checkouts))
digital_values <- book_checkouts %>% 
  group_by(CheckoutYear) %>% 
  filter(UsageClass == "Digital") %>% 
  summarise(avg_digital = mean(Checkouts)) 
book_type <- left_join(physical_values, digital_values, join_by("CheckoutYear"))


ggplot(book_type) +
  geom_line(mapping = aes(x = CheckoutYear, y = avg_physical, color = "Physical")) +
  geom_line(mapping = aes(x = CheckoutYear, y = avg_digital, color = "Digital")) +
  labs(title = "Physical VS Digital Book Checkout Over A Decade", x = "Year", y = "# of Books Checked Out Per Person", color = "Type of Book") + 
  scale_x_continuous(breaks = seq(2013, 2023, 2))
  
  
