                    ## Libraries used for this Project ##

library(tidyverse)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(plotly)
library(zoo)
library(lubridate)
library(scales)
library(viridisLite)
library(highcharter)
library(chron)

                    ## Code for Chapter 3 ##

          ## 3.1 Data Cleaning

bank_statement <- read.csv("C:\\##\\##\\##\\bank_statement\\all_statements.csv")

          ## 3.1.1

bank_clean <- bank_statement %>% 
  select(Date, Description.1, Payments, Time)

bank_clean <- na.omit(bank_clean)

strings_to_remove <- c("C NWANKPA", "C NWANKPA 14MAY23", "C NWANKPA 29MAY23", "C NWANKPA 15APR23", "C NWANKPA 02APR23", 
                       "C NWANKPA 01APR23")

bank_clean <- bank_clean %>%
  filter(!Description.1 %in% strings_to_remove)


          ## 3.1.2 Data Transformation

bank_clean$Time <- times(bank_clean$Time)

          ## 3.1.3 Handling Missing Values

any(is.na(bank_clean))

          ## 3.2 Statistics of the Dataset


          ## Number of Records

nrow(bank_clean)

          ##Summary Statistics

min(bank_clean$Date)
max(bank_clean$Date)
summary(bank_clean$Payments)
sd(bank_clean$Payments)

                    ## Code for Chapter 4 ##


          ## Figure 4.1

## Grouping dataset into Categories
bank_clean$Description.1 <- ifelse(bank_clean$Description.1 %in%  c("STGCOACH/CTYLINK", "trainline", "FIRST HAMPSHIRE"), 
                                   "Transportation", bank_clean$Description.1)


bank_clean$Description.1 <- ifelse(bank_clean$Description.1 == "PROPERTY HOLDI", "Rent", bank_clean$Description.1)


bank_clean$Description.1 <- ifelse(bank_clean$Description.1 
                                   %in%  c("ICELAND", "TESCO STORES 6841", 
                                           "MADINA HALAL MEAT", "CO-OP GROUP FOOD", "LIDL GB PORTSMOUTH", 
                                           "SAINSBURY'S S/MKT", "POUNDLAND LTD 1210", "TESCO STORES 3048",
                                           "SOUTHERN CO-OP 000", "TESCO STORE 3186", "SAINSBURYS S/MKTS",
                                           "SALAM FRATTON FOOD", "SOUTHERN CO-OP 002", "TESCO STORES 4611", 
                                           "Portsmouth Interna",  "MAMA LIT SPECIAL F"), 
                                   "Groceries/Utility", bank_clean$Description.1)

bank_clean$Description.1 <- ifelse(bank_clean$Description.1 
                                   %in%  c("BURGER KING", "UBER * EATS PEND", "PAPA JOHNS SOUTHSE", "GREGGS PLC",
                                           "ULTIMATE TASTE", "DOMINO S PIZZA", "PORTLAND COFFEE SH", 
                                           "DOMINOS PIZZA"), 
                                   "Outside Food", bank_clean$Description.1)

bank_clean$Description.1 <- ifelse(bank_clean$Description.1 
                                   %in%  c("AMZNMktplace", "Amazon.co.uk*165Q5", "PAYPAL FUNDING", "CDKEYS.COM", 
                                           "H & M", "PAYPAL PAYMENT", "Microsoft*Store", 
                                           "Lycamobile UK", "Footasylum Limited", "Udemy", "BRITISH HEART FOUN", 
                                           "THE PERFUME SHOP"), 
                                   "Online Purchases", bank_clean$Description.1)

bank_clean$Description.1 <- ifelse(bank_clean$Description.1 
                                   %in%  c("ADOBE CREATIVE CLO","Microsoft*Store", "Amazon Prime*RM03E", 
                                           "Lycamobile UK", "LYCAMOBILE UK", "Amazon Prime*1X4SM", "Amazon Prime*SV913",
                                           "Amazon.co.uk*UT15Q"), 
                                   "Subscriptions", bank_clean$Description.1)

bank_clean$Description.1 <- ifelse(bank_clean$Description.1 == "Vue Entertainment", "Cinema", bank_clean$Description.1)

bank_clean$Description.1 <- ifelse(bank_clean$Description.1 %in%  c("MR", "", "", "", ""), 
                                   "To People", bank_clean$Description.1)


bank_clean$Description.1 <- ifelse(bank_clean$Description.1 == "LIFE CHANG", "Haircut", bank_clean$Description.1)

## Creating the tibble
category_totals <- bank_clean %>%
  group_by(Description.1) %>%
  summarize(Total_Payments = sum(Payments)) %>%
  arrange(desc(Total_Payments))

## Visualization
ggplot(data = category_totals, aes(x = Total_Payments, y = reorder(Description.1, Total_Payments))) +
  geom_bar(stat = "identity", fill = "#2E8B57", width = 0.5) +
  geom_text(aes(label = paste0("£", Total_Payments)), hjust = -0.2, vjust = 0.5, color = "black", fontface = "bold", size = 6) +
  labs(x = "Total Amount (£)", y = "Category") +
  ggtitle("Distribution by Category") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(category_totals$Description.1))) +
  theme(
    axis.text.y = element_text(hjust = 1, margin = margin(l = 10), size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )



          ## Figure 4.2

## Creating the tibble
eateries <- bank_clean %>%
  filter(Description.1 %in% c("BURGER KING", "UBER * EATS PEND", "PAPA JOHNS SOUTHSE", "GREGGS PLC",
                              "ULTIMATE TASTE", "DOMINO S PIZZA","PORTLAND COFFEE SH", 
                              "DOMINOS PIZZA")) %>%
  mutate(Description.1 = ifelse(Description.1 %in% c("DOMINO S PIZZA", "DOMINOS PIZZA"), 
                                "Domino's Pizza", Description.1)) %>%
  group_by(Description.1) %>%
  summarise(Total_Payments = sum(Payments))

## Categorizing the Eatery names
freq_stores$Description.1 <- gsub(".*PAPA JOHNS SOUTHSE.*", "Papa John's", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*GREGGS PLC.*", "Greggs", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*UBER * EATS PEND.*", "Top Wok", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*ULTIMATE TASTE.*", "Ultimate Taste", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*BURGER KING.*", "Burger King", freq_stores$Description.1)

## Extracting the top 5 eateries based on the amount spent
top_eateries <- eateries %>%
  top_n(5, Total_Payments)

## Visualization
ggplot(data = top_eateries, aes(x = Total_Payments, y = reorder(Description.1, Total_Payments))) +
  geom_bar(stat = "identity", fill = "#2E8B57", width = 0.5) +
  geom_text(aes(label = paste0("£", Total_Payments)), hjust = -0.2, vjust = 0.5, color = "black", fontface = "bold", size = 6) +
  labs(x = "Total Payments", y = "Category") +
  ggtitle("Top 5 Eateries (Past 6 Months)") +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(top_eateries$Description.1))) +
  theme(axis.text.y = element_text(hjust = 1, margin = margin(l = 10), size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")


          ## Figure 4.3

## Extracting the rows to be used
freq_stores <- bank_clean %>% 
  select(Description.1, Payments) %>% 
  filter(Description.1 %in% c("ICELAND", "TESCO STORES 6841", 
                              "MADINA HALAL MEAT", "CO-OP GROUP FOOD", "LIDL GB PORTSMOUTH", 
                              "SAINSBURY'S S/MKT", "POUNDLAND LTD 1210", "TESCO STORES 3048",
                              "SOUTHERN CO-OP 000", "TESCO STORE 3186", "SAINSBURYS S/MKTS",
                              "SALAM FRATTON FOOD", "SOUTHERN CO-OP 002", "TESCO STORES 4611"))

##Categorizing the Store names
freq_stores$Description.1 <- gsub(".*TESCO.*", "Tesco", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*ICELAND.*", "Iceland", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*CO-OP.*", "Co-op", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*SAINSBURY.*", "Sainsbury's", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*POUNDLAND.*", "Poundland", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*MAMA.*", "Mama Lit", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*LIDL.*", "Lidl", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*SALAM FRATTON FOOD.*", "Salam Food", freq_stores$Description.1)
freq_stores$Description.1 <- gsub(".*MADINA HALAL MEAT.*", "Madina Halal Meat", freq_stores$Description.1)

## Creating the tibble
freq_sum <- freq_stores %>%
  group_by(Description.1) %>%
  summarize(Total_payments = sum(Payments)) %>%
  arrange(Total_payments)

# Visualization
freq_sum$Description.1 <- factor(freq_sum$Description.1, levels = rev(freq_sum$Description.1))
ggplot(freq_sum, aes(x = reorder(Description.1, Total_payments), y = Total_payments)) +
  geom_bar(stat = "identity", fill = "#2E8B57", width = 0.5) +
  geom_text(aes(label = paste0("£", Total_payments)), hjust = -0.2, vjust = 0.5, color = "black", fontface = "bold", size = 6) +
  labs(x = "Stores", y = "Total Spent (£)") +
  ggtitle("Most Visited Stores") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, margin = margin(l = 10), size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  coord_flip()


        ## Figure 4.4 

## Creating the labels to be used
month_labels <- c("Dec", "Jan", "Feb", "Mar", "Apr", "May")

## Creating the tibble
per_month <- bank_clean %>%
  mutate(Month = format(Date, "%Y-%b")) %>%
  group_by(Month) %>%
  summarize(Total_Payments = sum(Payments)) %>%
  arrange(as.Date(paste0(Month, "-01"), "%Y-%b-%d")) %>%
  mutate(Month = factor(Month, levels = unique(Month)))

## Visualization
month_labels <- per_month$Month
ggplot(per_month, aes(x = Month, y = Total_Payments, group = 1)) +
  geom_line(color = "#2E8B57", size = 1) +
  geom_point(color = "black", size = 5) +
  geom_text(aes(label = paste0("£", Total_Payments)), hjust = 0, vjust = -0.5, size = 6, color = "black") +
  labs(x = "Month", y = "Amount Spent (£)") +
  ggtitle("Spending per Month") +
  scale_x_discrete(labels = month_labels) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.line = element_line(color = "black", size = 1),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  )


        ## Figure 4.5

## Extracting the needed Data
month_vs_month <- bank_clean %>%
  mutate(Year = year(Date), Month = month(Date, label = TRUE))

## Creating the January visualization tibble
january_2023 <- month_vs_month %>%
  filter(Month == "Jan" & Year == 2023) %>%
  group_by(Date) %>%
  summarise(Total_Payments = sum(Payments))

## Visualization
ggplot(january_2023, aes(x = as.numeric(format(Date, "%d")), y = Total_Payments)) +
  geom_line(color = "#2E8B57", size = 1) +
  geom_point(color = "black", size = 5) +
  geom_text(aes(label = paste0("£", Total_Payments)), vjust = -1.5, size = 6, color = "black") +
  labs(x = "Date", y = "Amount Spent (£)") +
  ggtitle("January 2023") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_line(color = "black", size = 1),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(breaks = seq(1, 31, by = 1), labels = seq(1, 31, by = 1), limits = c(1, 31)) +
  scale_y_continuous(breaks = seq(0, max(april_2023$Total_Payments), 50)) +
  guides(color = FALSE)


          ## Figure 4.6

## Creating the april visualization tibble
april_2023 <- month_vs_month %>%
  filter(Month == "Apr" & Year == 2023) %>%
  group_by(Date) %>%
  summarise(Total_Payments = sum(Payments))

## Visualization
ggplot(april_2023, aes(x = as.numeric(format(Date, "%d")), y = Total_Payments)) +
  geom_line(color = "#2E8B57", size = 1) +
  geom_point(color = "black", size = 5) +
  geom_text(aes(label = paste0("£", Total_Payments)), vjust = -1.5, size = 6, color = "black") +
  labs(x = "Date", y = "Amount Spent (£)") +
  ggtitle("April 2023") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_line(color = "black", size = 1),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(breaks = seq(1, 30, by = 1), labels = seq(1, 30, by = 1), limits = c(1, 30)) +
  scale_y_continuous(breaks = seq(0, max(april_2023$Total_Payments), 50)) +
  guides(color = FALSE)


          ## Figure 4.7

## Creating the tibbles
time_heatmap <- bank_clean %>%
  filter(Description.1 != "PROPERTY HOLDI") %>%
  mutate(Hour = format(strptime(Time, format = "%H:%M:%S"), format = "%H")) %>%
  group_by(Hour) %>%
  summarize(sum_per_hour = sum(Payments))

## Labels for the visualizations
hour_labelsI <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", 
                  "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")

## Colours used
num_colorsI <- length(hour_labelsI)
color_paletteI <- viridis(num_colorsI, option = "A")

# Find the indices for hours 09, 07, and 05
index_09 <- which(hour_labelsI == "09")
index_07 <- which(hour_labelsI == "07")
index_05 <- which(hour_labelsI == "05")

# Assign the colors of hours 09, 07, and 05 to hours 21, 22, and 23
color_paletteI[c(21, 22, 23)] <- color_paletteI[c(index_09, index_07, index_05)]

# Setting Hour 23 to be the same color as Hour 00
color_paletteI[24] <- color_paletteI[1]

# Define the custom image URL
background_image <- "https://i.ibb.co/2cHVgSP/edited-6.jpg"

hc <- highchart() %>%
  hc_chart(type = "column", plotBackgroundImage = background_image, style = list(transform = "scaleX(-1)")) %>%
  hc_xAxis(categories = hour_labelsI) %>%
  hc_yAxis(
    title = list(text = ""),
    labels = list(enabled = FALSE),
    gridLineWidth = 0
  ) %>%
  hc_add_series(
    data = time_heatmap$sum_per_hour,
    colorByPoint = TRUE,
    colors = color_paletteI,
    name = "Amount Spent",
    tooltip = list(
      pointFormat = "<span style='font-size: 12px'><b>Hour: {point.category}</b><br>Amount Spent: £{point.y}</span>"
    )
  ) %>%
  hc_title(text = "") %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(
        enabled = TRUE,
        format = "£{y}",
        style = list(
          color = "#FFFFFF",
          textOutline = "1px contrast",
          fontSize = "14px" 
        )
      )
    )
  ) %>%
  hc_tooltip(shared = FALSE) %>%
  hc_credits(enabled = FALSE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_chart(
    backgroundColor = "transparent",
    spacingBottom = 0,
    spacingTop = 0,
    spacingLeft = 0,
    spacingRight = 0
  )

hc
