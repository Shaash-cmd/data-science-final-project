
library(ggplot2)
library(dplyr) 
library(readr)
library(tidyverse)
library(corrplot)

CSD <- read.csv("Customer_Sales.csv")
View(CSD)

#View first few rows of data
#head(CSD)

#Get summary statistic of data 
summary(CSD)
str(CSD)

#remove rows with NA value 
CSD <- na.omit(CSD)

#Identify duplicate row 
duplicate_rows <- duplicated(CSD)

#Rename columns 
colnames(CSD) [c(1,4,5,6,7,8,9)] <- c("ID", "Revenue", "Purchases", "Date", "Value", "Method", "Time")
#View (CSD)

sapply(CSD,class)


#convert data type of data
CSD_clean <- CSD %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%y"))
#View(CSD_clean)

sapply(CSD_clean,class)

CSD_clean <- CSD_clean %>%
  mutate(Age_Group = ifelse(Age < 28, "Young",ifelse(Age < 40, "Middle", "Old")))


CSD_male <- filter(CSD_clean, Gender == 0)  # filter out males

CSD_cleanedmale <- CSD_male %>% 
  count(Method) %>%
  mutate (pct = n / sum(n) * 100) # calculate percentages

#View(CSD_cleanedmale)



CSD_female <- filter(CSD_clean, Gender == 1) #filter out females

CSD_cleanedfemale <- CSD_female %>% 
  count(Method) %>%
  mutate (pct = n / sum(n) * 100) # calculate percentages

#View(CSD_cleanedfemale)


# filter out old people

CSD_old <- filter(CSD_clean, Age_Group == "Old")

CSD_cleanedold <- CSD_old %>% 
  count(Method) %>%
  mutate (pct = n / sum(n) * 100) # calculate percentages

#View(CSD_cleanedold)


# filter out middle people

CSD_middle <- filter(CSD_clean, Age_Group == "Middle")

CSD_cleanedmiddle <- CSD_middle %>% 
  count(Method) %>%
  mutate (pct = n / sum(n) * 100) # calculate percentages

#View(CSD_cleanedmiddle)


# filter out young people


CSD_young <- filter(CSD_clean, Age_Group == "Young")

CSD_cleanedyoung <- CSD_young %>% 
  count(Method) %>%
  mutate (pct = n / sum(n) * 100) # calculate percentages

#View(CSD_cleanedyoung)



# pie chart of male payment method

ggplot(CSD_cleanedmale) +
  geom_bar(aes(x = "", y = pct, fill = factor(Method)), stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Payment Method Used by Men") +
  scale_fill_manual(
    name = "Payment Method",
    values = c("0" = "#8B4A6B", "1" = "#5A7A5A", "2" = "#6B8CAE", "3" = "#B8860B"),
    labels = c("0" = "Digital Wallet", "1" = "Card", "2" = "Paypal", "3" = "Other")
  )

# asked AI for syntax of pie chart


# chart for payment method of women
ggplot(CSD_cleanedfemale) +
  geom_bar(aes(x = "", y = pct, fill = factor(Method)), stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Payment Method used by Women") +
  scale_fill_manual( name = "Payment Method",
    values = c("0" = "#8B4A6B", "1" = "#5A7A5A", "2" = "#6B8CAE", "3" = "#B8860B"),
    labels = c("0" = "Digital Wallet", "1" = "Card", "2" = "Paypal", "3" = "Other")
  )




#Chart for payment method of old people 

# chart old people payment method
ggplot (CSD_cleanedold)+
  geom_col(aes ( x = Method, y = pct, fill = factor (Method)))+
  theme(axis.text.x = element_text(size = 5 , angle = 45 , vjust = 0.5))+
  labs ( title = "Percentage of Payment Method used by people over 40",
         x = "Payment Method",
         y = " pct" ) +
  scale_fill_manual(
    name= "Payment Method",
    values = c("0" = "#8B4A6B", "1" = "#5A7A5A", "2"= "#6B8CAE", "3"="#B8860B"),
    labels = c("0" = "Digital Wallet", "1" = "Card", "2" ="Paypal","3"="Other")
  ) #ai fixed + on wrong line



# chart middle aged people payment method
ggplot (CSD_cleanedmiddle)+
  geom_col(aes ( x = Method, y = pct, fill = factor (Method)))+
  theme(axis.text.x = element_text(size = 5 , angle = 45 , vjust = 0.5))+
  labs ( title = "Percentage of Payment Method used by people between 28 and 40",
         x = "Payment Method",
         y = " pct" ) +
  scale_fill_manual(
    name= "Payment Method",
    values = c("0" = "#8B4A6B", "1" = "#5A7A5A", "2"= "#6B8CAE", "3"="#B8860B"),
    labels = c("0" = "Digital Wallet", "1" = "Card", "2" ="Paypal","3"="Other")
  ) #ai fixed + on wrong line


# chart young people payment method
ggplot (CSD_cleanedyoung)+
  geom_col(aes ( x = Method, y = pct, fill = factor (Method)))+
  theme(axis.text.x = element_text(size = 5 , angle = 45 , vjust = 0.5))+
  labs ( title = "Percentage of Payment Method used by people under 28",
         x = "Payment Method",
         y = " pct" ) +
  scale_fill_manual(
    name= "Payment Method",
    values = c("0" = "#8B4A6B", "1" = "#5A7A5A", "2"= "#6B8CAE", "3"="#B8860B"),
    labels = c("0" = "Digital Wallet", "1" = "Card", "2" ="Paypal","3"="Other")
  ) #ai fixed + on wrong line






