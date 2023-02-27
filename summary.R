library("dplyr")

book_checkouts <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)


most_ebook_checkouts <- book_checkouts %>% 
  mutate("Date" = paste(sep = "-", CheckoutYear, CheckoutMonth, "01")) %>% 
  group_by(MaterialType, Date) %>% 
  filter(MaterialType == "EBOOK") %>% 
  summarise(most_ebook = max(Checkouts)) %>% 
  slice(which.max(most_ebook)) %>% 
  pull(Date)

least_ebook_checkouts <- book_checkouts %>% 
  mutate("Date" = paste(sep = "-", CheckoutYear, CheckoutMonth, "01")) %>% 
  group_by(MaterialType, Date) %>% 
  filter(MaterialType == "EBOOK") %>% 
  summarise(least_ebook = min(Checkouts)) %>% 
  slice(which.min(least_ebook)) %>% 
  pull(Date)

kate_dicamillo <- book_checkouts %>% 
  filter(Creator == "Kate DiCamillo") %>% 
  mutate("Date" = paste(sep = "-", CheckoutYear, CheckoutMonth, "01")) %>% 
  group_by(Date, Title) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  top_n(1) %>% 
  pull(Title) 

kate_most<- kate_dicamillo[1]

kate_dicamillo_checkout <- book_checkouts %>% 
  filter(Creator == "Kate DiCamillo") %>% 
  mutate("Date" = paste(sep = "-", CheckoutYear, CheckoutMonth, "01")) %>% 
  group_by(Date, Title) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  top_n(1) %>% 
  pull(total_checkouts) 

kate_top <- kate_dicamillo_checkout[1]

print_books_2013 <- book_checkouts %>% 
  group_by(UsageClass) %>% 
  filter(CheckoutYear == "2013") %>% 
  filter(UsageClass == "Physical") %>% 
  summarise(phys_2013 = sum(Checkouts)) %>% 
  pull(phys_2013)

dig_books_2013 <- book_checkouts %>% 
  group_by(UsageClass) %>% 
  filter(CheckoutYear == "2013") %>% 
  filter(UsageClass == "Digital") %>% 
  summarise(dig_2013 = sum(Checkouts)) %>% 
  pull(dig_2013)

perc_phys_2013 <- (print_books_2013 / (dig_books_2013 + print_books_2013)) * 100

print_books_2022 <- book_checkouts %>% 
  group_by(UsageClass) %>% 
  filter(CheckoutYear == "2022") %>% 
  filter(UsageClass == "Physical") %>% 
  summarise(phys_2022 = sum(Checkouts)) %>% 
  pull(phys_2022)

dig_books_2022 <- book_checkouts %>% 
  group_by(UsageClass) %>% 
  filter(CheckoutYear == "2022") %>% 
  filter(UsageClass == "Digital") %>% 
  summarise(dig_2022 = sum(Checkouts)) %>% 
  pull(dig_2022)

perc_phys_2022 <- (print_books_2022 / (dig_books_2022 + print_books_2022)) * 100

diff_physical <- print_books_2013 - print_books_2022
