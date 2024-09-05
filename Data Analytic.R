#set the work directory(my laptop)
setwd("C:/Users/26800/Desktop/work")

#install the directory
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('hrbrthemes')
install.packages('plotly')
install.packages('viridis')
install.packages("rlang")
install.packages('ggplot')
install.packages("cowplot")
install.packages("plotly")
install.packages("caret")
install.packages("corrplot")
install.packages("caTools")
install.packages("jtools")
install.packages("lattice")
install.packages("gfonts")
install.packages("RColorBrewer")
install.packages("trainControl")
update.packages()

# Visualizations
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(plotly)
library(ggthemes)
library(cowplot)
library(RColorBrewer)
library(reshape2)
library(caret)
library(corrplot)
library(caTools)
library(jtools)
library(dplyr)
library(lattice)
library(RColorBrewer)
library(reshape2)
vg <- read.csv("vgsale.csv", stringsAsFactors = FALSE)
#load the data

#observe at first and then check the type of data
View(vg)
head(vg)
str(vg)


#drop N/A year data to make it more accurate,delete unnecessary column
vg <- vg[vg$Year != "N/A" , ]
vg <- vg[vg$Global_Sales != "N/A" , ]
vg <- vg %>% select(-Developer)
vg <- vg %>% select(-Rating)
vg <- vg %>% select(-Name)

#same function of drop year na!
vg <- vg[!is.na(vg$Year), ]

vg$Year <- factor(vg$Year)
library(dplyr)
vg <- select(vg,Platform,Year,Genre,NA_Sales,EU_Sales,JP_Sales,
             Other_Sales,
             Global_Sales)
summary(vg)


AS_means <- data.frame(Mean = c(mean(vg$NA_Sales), mean(vg$EU_Sales), mean(vg$JP_Sales), mean(vg$Other_Sales), mean(vg$Global_Sales)))
row.names(AS_means) <- c("North America ", "Europe", "Japan", "Other ", "Worldwide")
AS_means$Mean_round <- round(AS_means$Mean ,digit=3)
AS_means

#bar chart(area sale markets)
library(ggplot2)
theme_set(theme_bw())
ggplot(data = AS_means, mapping = aes(x=row.names(AS_means), y=Mean_round), increasing =TRUE) + 
  geom_point(size=5) + 
  geom_segment(aes(x=row.names(AS_means), xend=row.names(AS_means), y=0, yend=Mean_round)) +
  geom_label(mapping = aes(label=Mean_round), fill = "darkgrey", size = 3.5, color = "white", fontface = "bold", hjust=.5) +
  ggtitle("Sales markets in different areas") + xlab("Sale Area") + ylab("Mean_Sales") +
  labs(caption="source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "plain"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "plain"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")

#except Global
AS2_means <- data.frame(Mean = c(mean(vg$NA_Sales), mean(vg$EU_Sales), mean(vg$JP_Sales), mean(vg$Other_Sales) ))
row.names(AS2_means) <- c("North America ", "Europe", "Japan", "Other ")
AS2_means$Mean_round <- round(AS2_means$Mean ,digit=3)
AS2_means

#pie char for sale market
b <- ggplot(data = AS2_means, mapping = aes(x = Mean, y = row.names(AS2_means))) +
  geom_line(group = 1, size = 1.2, linetype = "dashed", color = "gold") +
  geom_point(size = 5, stroke = 1.5, shape = 21, mapping = aes(fill = row.names(AS2_means))) +
  theme_minimal() +
  ylab("") +
  xlab("") +
  theme(plot.title = element_text(size = 16, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(color = "white"),
        legend.text = element_text(size = 12, face = "bold"))

plot_grid(b + coord_polar(), nrow = 1, ncol = 1)



#global sales each year
Gsales <- aggregate(list(Global_Sales = vg$Global_Sales), list(Year = vg$Year), sum)
Gsales <- Gsales[order(Gsales$Global_Sales),]
G <- c()
for(i in 1:nrow(Gsales)){
  G <- c(G, i)
}
row.names(Gsales) <- G
Gsales
# Using the subset() function to drop 2017 and 2020
Gsales <- subset(Gsales, Year != 2020 & Year != 2017)
#line plot
options(repr.plot.width = 12, repr.plot.height = 5)
ggplot(data = Gsales, mapping = aes(x = Year, y = Global_Sales)) +
  geom_segment(aes(xend=Year, yend=0, color = Year), size = 1.8, alpha = .8) +
  geom_point(mapping = aes(fill = Year), size = 3, shape = 21) +
  geom_line(group = 1, size = 0.8, linetype = 10, color ='darkblue') +
  ggtitle("Global sales each year") +
  xlab("Year") +
  ylab("Global_Sales") +
  labs(caption="source: B00892670") +
  theme(plot.margin = margin(1, 0.2, 1, 0.2, "cm"),
        plot.title = element_text(size = 25, face = "bold", hjust = .5),
        axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 10, angle = 0),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

# Subset the data for the desired years
vg_07_10 <- subset(vg, Year >= 2007 & Year <= 2010)

# View the corrected data
View(vg_07_10)

#genre 
table(vg$Genre)
# Create a frequency table of video game genres
quantity <- data.frame(cbind(Frequency = table(vg$Genre), Percent = prop.table(table(vg$Genre)) * 100))

# Order the frequency table by frequency in descending order
quantity <- quantity[order(quantity$Frequency, decreasing = TRUE), ]

# Subset the table to only include rows with a frequency greater than 100
quantity <- subset(quantity, Frequency > 100)

# Print the frequency table to the console
print(quantity)


ggplot(data = quantity, mapping = aes(x = reorder(row.names(quantity), -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(quantity), color = row.names(quantity)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "darkblue", size = 3.5, color = "white", fontface = "bold", hjust=.5) +
  ggtitle("Genre Frequency Distribution") +
  xlab("Genres") +
  ylab("Quantity") +
  coord_flip() +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")


#Number of sales by genre on each market 
# NA_Sales
genre_sales_NA <- aggregate(list(NA_Sales = vg$NA_Sales), list(Genre = vg$Genre), sum)
genre_sales_NA <- genre_sales_NA[order(genre_sales_NA$NA_Sales, decreasing = T), ]
# Number of sales per genre in NA
options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(data = genre_sales_NA, mapping = aes(x = Genre, y = NA_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 1.3, alpha = 1.5) +
  geom_point(mapping = aes(fill = Genre), size = 5, shape = 21, alpha = .7) +
  geom_line(group = 1, size = 0.7, linetype = 10, color = "red", alpha = .7) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Number of sales by genre in NA ") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none")
#top5 genre for NA
top5_genre_NA <- genre_sales_NA[1:5, ]
top5_genre_NA
#plot top5 genre for NA
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = top5_genre_NA, mapping = aes(x = Genre, y = NA_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre), alpha = 0.7) +
  geom_label(mapping = aes(label = round(NA_Sales, 2)), size = 4, fontface = "bold", vjust = 0.5) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Top 5 Genres by Sales in North America") +
  labs(caption = "Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
# NA_Sales
platform_sales_NA <- aggregate(list(NA_Sales = vg$NA_Sales), list(Platform = vg$Platform), sum)
platform_sales_NA <- platform_sales_NA[order(platform_sales_NA$NA_Sales, decreasing = T), ]
#Top5 platform sales for EU
top5_platfrom_sales_NA <- platform_sales_NA[1:5, ]
top5_platfrom_sales_NA
ggplot(data = top5_platfrom_sales_NA, 5, mapping = aes(x = Platform, y = NA_Sales)) +
  geom_line(size = 0.5, alpha = .7, group = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label = NA_Sales, fill = Platform), color = "black", size = 2.5, fontface = "bold", alpha = .5) +
  xlab("") +
  ylab("Sales in North America") +
  ggtitle("sales Top5 platform in NA") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")




# EU_Sales
genre_sales_EU <- aggregate(list(EU_Sales = vg$EU_Sales), list(Genre = vg$Genre), sum)
genre_sales_EU <- genre_sales_EU[order(genre_sales_EU$EU_Sales, decreasing = T), ]
# Number of sales per genre in EU
ggplot(data = genre_sales_EU, mapping = aes(x = Genre, y = EU_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 1.3, alpha = 1.5) +
  geom_point(mapping = aes(fill = Genre), size = 5, shape = 21, alpha = .7) +
  geom_line(group = 1, size = 0.7, linetype = 10, color = "red", alpha = .7) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Number of sales by genre in EU") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")
#top5 genre for EU
top5_genre_EU <- genre_sales_EU[1:5, ]
top5_genre_EU
#plot top5 genre for EU
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = top5_genre_EU, mapping = aes(x = Genre, y = EU_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre), alpha = 0.7) +
  geom_label(mapping = aes(label = round(EU_Sales, 2)), size = 4, fontface = "bold", vjust = 0.5) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Top 5 Genres by Sales in European") +
  labs(caption = "Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
# EU_Sales
platform_sales_EU <- aggregate(list(EU_Sales = vg$EU_Sales), list(Platform = vg$Platform), sum)
platform_sales_EU <- platform_sales_EU[order(platform_sales_EU$EU_Sales, decreasing = T), ]
#Top5 platform sales for EU
top5_platfrom_sales_EU <- platform_sales_EU[1:5, ]
top5_platfrom_sales_EU
ggplot(data = top5_platfrom_sales_EU, 5, mapping = aes(x = Platform, y = EU_Sales)) +
  geom_line(size = 0.5, alpha = .7, group = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label = EU_Sales, fill = Platform), color = "black", size = 2.5, fontface = "bold", alpha = .5) +
  xlab("") +
  ylab("Sales in European") +
  ggtitle("sales Top5 platform in EU") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")












# JP_Sales
genre_sales_JP <- aggregate(list(JP_Sales = vg$JP_Sales), list(Genre = vg$Genre), sum)
genre_sales_JP <- genre_sales_JP[order(genre_sales_JP$JP_Sales, decreasing = T), ]

# Number of sales per genre in JP
ggplot(data = genre_sales_JP, mapping = aes(x = Genre, y = JP_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 1.3, alpha = 1.5) +
  geom_point(mapping = aes(fill = Genre), size = 5, shape = 21, alpha = .7) +
  geom_line(group = 1, size = 0.7, linetype = 10, color = "red", alpha = .7) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Number of sales by genre in JP") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")

#top5 genre for JP
top5_genre_JP <- genre_sales_JP[1:5, ]
top5_genre_JP
#plot top5 genre for JP
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = top5_genre_JP, mapping = aes(x = Genre, y = JP_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre), alpha = 0.7) +
  geom_label(mapping = aes(label = round(JP_Sales, 2)), size = 4, fontface = "bold", vjust = 0.5) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Top 5 Genres by Sales in Japan") +
  labs(caption = "Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
# JP_Sales
platform_sales_JP <- aggregate(list(JP_Sales = vg$JP_Sales), list(Platform = vg$Platform), sum)
platform_sales_JP <- platform_sales_JP[order(platform_sales_JP$JP_Sales, decreasing = T), ]
platform_sales_JP 
#Top5 platform sales for JP
top5_platfrom_sales_JP <- platform_sales_JP[1:5, ]
top5_platfrom_sales_JP
ggplot(data = top5_platfrom_sales_JP, 5, mapping = aes(x = Platform, y = JP_Sales)) +
  geom_line(size = 0.5, alpha = .7, group = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label = JP_Sales, fill = Platform), color = "black", size = 2.5, fontface = "bold", alpha = .5) +
  xlab("") +
  ylab("Sales in Japan") +
  ggtitle("sales Top5 platform in JP") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")






# Other_Sales
genre_sales_Other <- aggregate(list(Other_Sales = vg$Other_Sales), list(Genre = vg$Genre), sum)
genre_sales_Other <- genre_sales_Other[order(genre_sales_Other$Other_Sales, decreasing = T), ]

#Number of sales per genre in other
ggplot(data = genre_sales_Other, mapping = aes(x = Genre, y = Other_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 1.3, alpha = 1.5) +
  geom_point(mapping = aes(fill = Genre), size = 5, shape = 21, alpha = .7) +
  geom_line(group = 1, size = 0.7, linetype = 10, color = "Black", alpha = .7) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Number of sales by genre in Other ") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")

#top5 genre for Other
top5_genre_Other <- genre_sales_Other[1:5, ]
top5_genre_Other
#plot top5 genre for JP
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = top5_genre_Other, mapping = aes(x = Genre, y = Other_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre), alpha = 0.7) +
  geom_label(mapping = aes(label = round(Other_Sales, 2)), size = 4, fontface = "bold", vjust = 0.5) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Top 5 Genres by Sales in Other") +
  labs(caption = "Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

# Other_Sales
platform_sales_Other <- aggregate(list(Other_Sales = vg$Other_Sales), list(Platform = vg$Platform), sum)
platform_sales_Other <- platform_sales_Other[order(platform_sales_Other$Other_Sales, decreasing = T), ]
#Top5 platform sales for Other
top5_platfrom_sales_Other <- platform_sales_Other[1:5, ]
top5_platfrom_sales_Other
ggplot(data = top5_platfrom_sales_Other, 5, mapping = aes(x = Platform, y = Other_Sales)) +
  geom_line(size = 0.5, alpha = .7, group = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label = Other_Sales, fill = Platform), color = "black", size = 2.5, fontface = "bold", alpha = .5) +
  xlab("") +
  ylab("Sales in Other") +
  ggtitle("sales Top5 platform in Other") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")

#top 5 platforms and top 5 genres
top5_platforms <- c("DS", "PS2", "PS3", "Wii", "X360")
top5_genre_Other
vg_filtered <- subset(vg, Platform %in% top5_platforms & Genre %in% top5_genres)
# Aggregate global sales by platform and genre
agg_sales <- aggregate(Global_Sales ~ Platform + Genre, data = vg_filtered, sum)

# Create a stacked bar chart
ggplot(agg_sales, aes(x = Platform, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Action" = "#FF5733", "Sports" = "#C70039", "Misc" = "#900C3F", "Role-Playing" = "#581845", "Shooter" = "#1C1C1C")) +
  ylab("Global Sales") +
  xlab("Platform") +
  ggtitle("Global Sales of Top5 Platforms for Top5 Genres") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "bottom")






# Global_Sales
genre_sales_Global <- aggregate(list(Global_Sales = vg$Global_Sales), list(Genre = vg$Genre), sum)
genre_sales_Global <- genre_sales_Global[order(genre_sales_Global$Global_Sales, decreasing = T), ]
genre_sales_Global

# Number of sales per genre in Other
ggplot(data = genre_sales_Other, mapping = aes(x = Genre, y = Other_Sales)) +
  geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 1.3, alpha = 1.5) +
  geom_point(mapping = aes(fill = Genre), size = 5, shape = 21, alpha = .7) +
  geom_line(group = 1, size = 0.7, linetype = 10, color = "red", alpha = .7) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Number of sales by genre in Global") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")


#top5 genre for Global
top5_genre_Global <- genre_sales_Global[1:5, ]
top5_genre_Global
#plot top5 genre for Global
options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = top5_genre_Global, mapping = aes(x = Genre, y = Global_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre), alpha = 0.7) +
  geom_label(mapping = aes(label = round(Global_Sales, 2)), size = 4, fontface = "bold", vjust = 0.5) +
  xlab("Genres") +
  ylab("Sales") +
  ggtitle("Top 5 Genres by Sales in Global") +
  labs(caption = "Source: B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

# Global_Sales
platform_sales_Global <- aggregate(list(Global_Sales = vg$Global_Sales), list(Platform = vg$Platform), sum)
platform_sales_Global <- platform_sales_Global[order(platform_sales_Global$Global_Sales, decreasing = T), ]
#Top5 platform sales for Global
top5_platfrom_sales_Global <- platform_sales_Global[1:5, ]
top5_platfrom_sales_Global
ggplot(data = top5_platfrom_sales_Global, 5, mapping = aes(x = Platform, y = Global_Sales)) +
  geom_line(size = 0.5, alpha = .7, group = 1, linetype = 2, color = "blue") +
  geom_label(mapping = aes(label = Global_Sales, fill = Platform), color = "black", size = 2.5, fontface = "bold", alpha = .5) +
  xlab("") +
  ylab("Sales in Global") +
  ggtitle("sales Top5 platform in Global") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")


#top 5 platforms and top 5 genres
top5_platforms <- c("DS", "PS2", "PS3", "Wii", "X360")
top5_genres <- c("Action", "Sports", "Misc", "Role-Playing", "Shooter")
vg_filtered <- subset(vg, Platform %in% top5_platforms & Genre %in% top5_genres)
# Aggregate global sales by platform and genre
agg_sales <- aggregate(Global_Sales ~ Platform + Genre, data = vg_filtered, sum)

# Create a stacked bar chart
ggplot(agg_sales, aes(x = Platform, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Action" = "#FF5733", "Sports" = "#C70039", "Misc" = "#900C3F", "Role-Playing" = "#581845", "Shooter" = "#1C1C1C")) +
  ylab("Global Sales") +
  xlab("Platform") +
  ggtitle("Global Sales of Top5 Platforms for Top5 Genres") +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "bottom")


#platform classify
platform <- data.frame(cbind(Frequency = table(vg$Platform), Percent = prop.table(table(vg$Platform)) * 100))
platform <- platform[order(platform$Frequency, decreasing = TRUE), ]
platform
#Comparison
ggplot(data = platform, mapping = aes(x = Frequency, y = row.names(platform))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(platform), color = row.names(platform)), alpha = 0.7, size = 0.5) +
  geom_label(mapping = aes(label=Frequency), fill = "darkblue", size = 2.4, color = "white", fontface = "bold", hjust=.5) +
  ggtitle("Platform Distribution") +
  xlab("Quantity") +
  ylab("Platform") +
  coord_flip() +
  labs(caption="Source: Haoran B00892670") +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 18, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "none")

#select TOP5 to analyse
top5_platform <- platform[1:5, ]
top5_platform
#Top5_platform
c <- ggplot(data = top5_platform, mapping = aes(x = row.names(top5_platform), y = Percent)) +
  geom_bar(stat = "identity", aes(fill = row.names(top5_platform)), size = 1, alpha = .5, color = "black") +
  geom_label(mapping = aes(label = round(Percent, 2)), fill = "green", color = "white", size = 6, fontface = "bold") +
  coord_flip() +
  theme_economist() +
  ylab("") +
  xlab("Top5 platform") +
  theme(plot.background = element_rect(fill = "#C0C0C0", color = "orange", size = 1),
        axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
        axis.title.x = element_text(size = 12, hjust = .5, vjust = -2, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")+
  labs(caption="Source: Haoran B00892670")
# Combine plots  c
plot_grid(c + coord_polar(),  ncol = 1, nrow = 1)

#combine the platfrom to analyze
platform$Platform = c('DS', 'PS2', 'PS3', 'Wii', 'X360', 'PSP', 'PS', 'PC', 'GBA', 'XB', 'GC', '3DS', 'PSV', 'PS4', 'N64', 'SNES', 'XOne', 'SAT', 'WiiU', '2600', 'NES', 'GB', 'DC', 'GEN', 'NG', 'SCD', 'WS', '3DO', 'TG16', 'GG', 'PCFX')
pc <- c("PC")
xbox <- c("X360", "XB", "XOne")
nintendo <- c("Wii", "WiiU", "N64", "GC", "NES", "3DS", "DS") 
playstation <- c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")
platforms_combine <- platform %>%
  mutate(Type = ifelse(Platform %in% pc, "PC",
                       ifelse(Platform %in% xbox, "Xbox",
                              ifelse(Platform %in% nintendo, "Nintendo", 
                                     ifelse(Platform %in% playstation, "Playstation", "Others")))))
ggplot(data = platforms_combine, mapping = aes(x = , y = Type)) +
  geom_bar(stat = "identity", mapping = aes(fill = Type, color = Type), alpha = 0.7, size = 0.25) +
  ggtitle("Combined platfrom Frequency Distribution") +
  xlab("Quantity") +
  ylab("Platform combined") +
  coord_flip() +
  labs(caption="Source: B00892670") +
  theme(
    plot.title = element_text(size = 16, hjust = .5, face = "bold"),
    axis.title.x = element_text(size = 12, hjust = .5, face = "italic"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none")









#publisher
table(vg$Publisher)
Company <- data.frame(cbind(Frequency = table(vg$Publisher), Percent = prop.table(table(vg$Publisher)) * 100))
Company <- Company[order(Company$Frequency, decreasing = TRUE), ]
#drop the few publisher
Company <- subset(Company, Frequency > 10)
Company
top10_Company <- Company[1:10, ]
top10_Company

#plot top10 company
c <- ggplot(data = top10_Company, mapping = aes(x = row.names(top10_Company), y = Percent)) +
  geom_bar(stat = "identity", aes(fill = row.names(top10_Company)), size = 1, alpha = .5, color = "black") +
  geom_label(mapping = aes(label = round(Percent, 2)), fill = "green", color = "white", size = 6, fontface = "bold") +
  coord_flip() +
  theme_economist() +
  ylab("") +
  xlab("Top10_Company") +
  theme(plot.background = element_rect(fill = "#F0E68C", color = "orange", size = 1),
        axis.title.y = element_text(size = 12, hjust = .5, face = "italic"),
        axis.title.x = element_text(size = 12, hjust = .5, vjust = -1, face = "italic"),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        legend.position = "none")+
  labs(caption="Source: Haoran B00892670")
# Combine plots  c
plot_grid(c + coord_polar(),  ncol = 1, nrow = 1)

options(repr.plot.width = 14, repr.plot.height = 10)
ggplot(data = top10_Company, mapping = aes(x = Frequency, y = row.names(top10_Company))) +
  geom_line(group = 1, size = 1, color = "blue", linetype = "dashed") +
  geom_label(mapping = aes(label=Frequency, fill = row.names(top10_Company)), size = 4, color = "white", fontface = "bold", hjust=.7) +
  ggtitle("Company distribution") +
  xlab("Quantity") +
  ylab("") +
  theme_economist() +
  theme(plot.background = element_rect(fill = "#F0E68C", color = "blue"),
        plot.title = element_text(size = 18, hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
        axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "none")+labs(caption="Source: Haoran B00892670")




library(ggplot2)
library(dplyr)

# Find the top 10 companies
top10_Company <- vg %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales + JP_Sales + Other_Sales, na.rm = TRUE)) %>%
  top_n(10, wt = Total_Sales)

# Filter the data for the top 10 companies and group by Genre
top10_genre_sales <- vg %>%
  filter(Publisher %in% top10_Company$Publisher) %>%
  group_by(Genre, Publisher) %>%
  summarise(NA_Sales = sum(NA_Sales, na.rm = TRUE),
            EU_Sales = sum(EU_Sales, na.rm = TRUE),
            JP_Sales = sum(JP_Sales, na.rm = TRUE),
            Other_Sales = sum(Other_Sales, na.rm = TRUE)) %>%
  mutate(Total_Sales = NA_Sales + EU_Sales + JP_Sales + Other_Sales) %>%
  arrange(desc(Total_Sales)) %>%
  group_by(Publisher) %>%
  top_n(5, wt = Total_Sales)

# Create a stacked bar plot
ggplot(data = top10_genre_sales, aes(x = Publisher, y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  ggtitle("Top 10 Companies global genre market") +
  xlab("Top 10 Companies") +
  ylab("Total Sales (in millions)") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Find the top 10 companies
top10_Company <- vg %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(NA_Sales + EU_Sales + JP_Sales + Other_Sales, na.rm = TRUE)) %>%
  top_n(10, wt = Total_Sales)

# Filter the data for the top 10 companies and group by Platform
top10_platform_sales <- vg %>%
  filter(Publisher %in% top10_Company$Publisher) %>%
  group_by(Platform, Publisher) %>%
  summarise(NA_Sales = sum(NA_Sales, na.rm = TRUE),
            EU_Sales = sum(EU_Sales, na.rm = TRUE),
            JP_Sales = sum(JP_Sales, na.rm = TRUE),
            Other_Sales = sum(Other_Sales, na.rm = TRUE)) %>%
  mutate(Total_Sales = NA_Sales + EU_Sales + JP_Sales + Other_Sales) %>%
  arrange(desc(Total_Sales)) %>%
  group_by(Publisher) %>%
  top_n(5, wt = Total_Sales)

# Create a stacked bar plot
ggplot(data = top10_platform_sales, aes(x = Publisher, y = Total_Sales, fill = Platform)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  ggtitle("Top 10 Companies global platform market") +
  xlab("Top 10 Companies") +
  ylab("Total Sales (in millions)") +
  theme_minimal() +
  theme(legend.position = "bottom")




#model1
Z_model <-  vg[,c('Platform',
                  'Year',
                  'Genre',
                  'Global_Sales',
                  'Publisher',
                  'Platform',
                  'Critic_Score',
                  'Critic_Count',
                  'User_Score',
                  'User_Count')]
library(caret)
install.packages("RANN")
library(RANN)
library(dplyr)
GPerc<-apply(Z_model,2, function(x){length(which(is.na(x)))/dim(Z_model)[1]})
GPerc

Z_model <- Z_model %>% select(-Genre)
Z_model <- Z_model %>% select(-Platform.1)
Z_model <- Z_model %>% select(-Platform)
preproc <- preProcess(Z_model[,-1], method = c("knnImpute")) 
# Convert 'Year' to numeric if it's not
if (!is.numeric(Z_model$Year)) {
  Z_model$Year <- as.numeric(Z_model$Year)
}

# Check for NA values in 'Year'
if (any(is.na(Z_model$Year))) {
  # Take action here - you could remove or impute these rows, or ignore them in the quantile calculation
}

X_train <- predict(preproc, Z_model[Z_model$Year <= quantile(Z_model$Year, .9),-1])
X_test <- predict(preproc, Z_model[Z_model$Year > quantile(Z_model$Year, .9),-1])
Y_test <- Z_model[1:dim(X_test)[1],1]
length(Y_test)==dim(X_test[1])[1]

Y_train <- Z_model[dim(X_test)[1]+1:dim(X_train)[1],1]
length(Y_train)==dim(X_train[1])[1]

Z_train <- cbind(Y_train,X_train)
train_indx <- which(Z_train$Year<quantile(Z_train$Year,0.875))

P_avg_Sales  <-Z_train%>%
  group_by(Publisher)%>% 
  mutate(avgSales = mean(Y_train))%>%ungroup()%>%
  select(Publisher,avgSales)%>% unique()
Z_train<- left_join(Z_train,P_avg_Sales,by="Publisher")
Z_train[is.na(Z_train)]<-0
Z_train <- Z_train%>% select(-Publisher)
X_test <- left_join(X_test,P_avg_Sales,by="Publisher")
X_test[is.na(X_test)]<-0
X_test <- X_test%>% select(-Publisher)

#data processing 
dp <- preProcess(Z_train[,c(4:7)],method=c("center","scale"))
Z_train <- cbind(Z_train[,c(1:3)], predict(dp, Z_train[,c(4:7)]))
dp <- preProcess(X_test[,c(4:6)],method=c("center","scale"))
X_test <- cbind(X_test[,c(1:3)], predict(dp, X_test[,c(4:6)])) 

head(Z_train)
library(corrplot)
cormax <- cor(Z_train[,c(1:7)])
corrplot(cormax,method="circle")
corrplot(cormax,method="number")

# Load the required package
library(stats)

# Perform PCA
pca <- prcomp(Z_train, scale. = TRUE)
# Print a summary of the PCA results
summary(pca)
# Access the rotation (result)
result <- pca$rotation
# Access the scores
scores <- pca$x
# Plot the variance explained by each principal component
screeplot(pca, type = "l", main = "Scree Plot")
# Plot the scores on the first two principal components
plot(scores[,1], scores[,2], 
     xlab = "", ylab = "", 
     main = "PCA value")

library(caret) 
library(caTools)
library(jtools)
sales__dataset1 = subset(vg, select = -c(Publisher))
set.seed(369)
split = sample.split(sales__dataset1$Global_Sales, SplitRatio = 0.7)
train_dataset1 = subset(sales__dataset1, split == TRUE)
test_dataset1 = subset(sales__dataset1, split == FALSE)


install.packages("Metrics")
library(Metrics)
### Exp 1 - Random Forest
# For numeric columns
for(name in names(train_dataset1)) {
  if(is.numeric(train_dataset1[[name]])) {
    train_dataset1[[name]][is.na(train_dataset1[[name]])] <- median(train_dataset1[[name]], na.rm = TRUE)
  }
}

set.seed(369)
rf_thr <- randomForest(Global_Sales~.,data = train_dataset1,
                       ntreeTry = 500,
                       mtry=2,
                       importance = TRUE,
                       proximity = TRUE)
print(rf_thr)
attributes(rf_thr)
p1010 <- predict(rf_thr, train_dataset1)

p1010
cm_train1 <- table (p1010, train_dataset1$Global_Sales)
cm_train1
train_accuracy1 = sum(diag(cm_train1)/sum(cm_train1))
train_accuracy1
p2020 <- predict(rf1, test_dataset1)
cm_test1 <- table(p2020, test_dataset1$Global_Sales)
cm_test1
test_accuracy1 = sum(diag(cm_test1)/sum(cm_test1))
test_accuracy1
plot(rf_thr)

RMSE(p1010, train_dataset1$Global_Sales)
# RMSE on test set
RMSE(p2020, test_dataset1$Global_Sales)
# MAE on training set
MAE(p1010, train_dataset1$Global_Sales)
# MAE on test set
MAE(p2020, test_dataset1$Global_Sales)

p2020 <- predict(rf_thr, test_dataset1)
library(ggplot2)
# Compute RMSE and MAE
train_rmse <- RMSE(p1010, train_dataset1$Global_Sales)
train_mae <- MAE(p1010, train_dataset1$Global_Sales)
test_rmse <- RMSE(p2020, test_dataset1$Global_Sales)
test_mae <- MAE(p2020, test_dataset1$Global_Sales)

# Create dataframe
df <- data.frame(
  Set = c("Train1", "Test2", "Train1", "Test2"),
  Metric = c("RMSE1", "RMSE2", "MAE1", "MAE2"),
  Value = c(train_rmse, test_rmse, train_mae, test_mae)
)

# Plot
ggplot(df, aes(x = Set, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Value") +
  xlab("") +
  ggtitle("Model Performance Metrics") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


Z_model <-  vg[, c(
  'Global_Sales',
  'Year',
  'Genre',
  'Critic_Score',
  'Critic_Count',
  'User_Score',
  'User_Count'
)]
library(lattice)
library(caret)
GPerc <-
  apply(Z_model, 2, function(x) {
    length(which(is.na(x))) / dim(Z_model)[1]
  })
GPerc

Z_model <-
  Z_model %>% filter(!is.na(Genre))

preproc <-
  preProcess(Z_model[, -1], method = c("knnImpute")) 
X_train <-
  predict(preproc, Z_model[Z_model$Year <= quantile(Z_model$Year, .9), -1])
X_test <-
  predict(preproc, Z_model[Z_model$Year > quantile(Z_model$Year, .9), -1])
Y_test <-
  Z_model[1:dim(X_test)[1], 1]
length(Y_test) ==
  dim(X_test[1])[1]
Y_train <-
  Z_model[dim(X_test)[1] + 1:dim(X_train)[1], 1]
length(Y_train) ==
  dim(X_train[1])[1]
Z_train <-
  cbind(Y_train, X_train)
train_indx <-
  which(Z_train$Year < quantile(Z_train$Year, 0.875))


#### Feature engineering

P_avg_Sales  <-
  Z_train %>%
  filter(Year_of_Release >=
           max(Year) - 3) %>%
  group_by(Genre) %>%
  mutate(avgSales = mean(Y_train)) %>%
  ungroup() %>%
  select(Genre, avgSales) %>% unique()

Z_train <-
  left_join(Z_train, P_avg_Sales, by = "Genre")
Z_train[is.na(Z_train)] <-
  0

X_test <-
  left_join(X_test, P_avg_Sales, by = "Genre")
X_test[is.na(X_test)] <-
  0


#### Centering and scaling


cs <-
  preProcess(D_train[, c(5:8)], method = c("center", "scale"))
D_train <-
  cbind(D_train[, c(1:4)], predict(cs, D_train[, c(5:8)]))
cs <-
  preProcess(X_test[, c(4:7)], method = c("center", "scale"))
X_test <-
  cbind(X_test[, c(1:3)], predict(cs, X_test[, c(4:7)]))


head(D_train)

cormax <-
  cor(D_train[, c(4:8)])
corrplot(cormax, method =
           "number")

#Linear regression

ctrl <-
  trainControl(method = 'LGOCV',
               index = list(TrainSet = train_indx))
set.seed(666)
M_base <-
  train(
    Y_train ~ Year_of_Release + Genre + Critic_Score +
      Critic_Count +
      User_Score + User_Count ,
    weights =
      exp(D_train$Year - min(D_train$Year) + 1),
    data =
      Z_train,
    trControl =
      ctrl,
    method =
      "lm"
  )
train_p_b <-
  predict(M_base, Z_train)
test_p_b <-
  predict(M_base, X_test)
RMSE(train_p_b, Z_train$Y_train) 

#Elastic net
print(dim(vg))
print(length(vg$Year_of_Release))
print(length(vg$Genre))
print(length(vg$Critic_Score))
print(length(vg$Critic_Count))
print(length(vg$User_Score))
print(length(vg$User_Count))
print(length(vg$Platform))

print(length(Y_train))

#Elastic net
glmnetgrid <-
  expand.grid(alpha = c(0.1, 0.55, 1), lambda = seq(0, 0.5, 0.1))
set.seed(666)
M_glmnet <-
  train(
    Y_train ~ Year + Genre + Critic_Score +
      Critic_Count +
      User_Score + User_Count  + avgSales,
    weights =
      exp(D_train$Year - min(Z_train$Year) + 1),
    data =
      Z_train,
    trControl =
      ctrl,
    method =
      "glmnet",
    tuneGrid =
      glmnetgrid
  )
train_p_glmnet <-
  predict(M_glmnet, Z_train)
test_p_glmnet <-
  predict(M_glmnet, X_test)
RMSE(train_p_glmnet, Z_train$Y_train) 
M_glmnet$results[row.names(M_glmnet$bestTune), 'RMSE']

#Support vector machine

set.seed(369)
M_svm <-
  train(
    Y_train ~ Year_of_Release + Genre + Critic_Score +
      Critic_Count +
      User_Score + User_Count  + avgSales,
    data =
      D_train,
    weights =
      exp(D_train$Year - min(D_train$Year) + 1),
    method =
      "svmRadial",
    trControl =
      ctrl
  )
train_p_svm <-
  predict(M_svm, D_train)
test_p_svm <-
  predict(M_svm, X_test)
RMSE(train_p_svm, Z_train$Y_train) 
M_svm$results[row.names(M_svm$bestTune), 'RMSE']


# Random forest



set.seed(369)
M_rf <-
  train(
    Y_train ~ Year_of_Release + Genre + Critic_Score +
      Critic_Count +
      User_Score + User_Count  + avgSales,
    weights =
      exp(Z_train$Year - min(D_train$Year) + 1),
    # more weights for recent data
    data =
      Z_train,
    method =
      "rf",
    tuneLength =
      2,
    trControl =
      ctrl
  )
train_p_rf <-
  predict(M_rf, Z_train)
test_p_rf <-
  predict(M_rf, X_test)
RMSE(train_p_rf, Z_train$Y_train) # training error
M_rf$results[row.names(M_rf$bestTune), 'RMSE']# validation error

#Model selection


modelList <-
  list(lm = M_base,
       enet =
         M_glmnet,
       svm =
         M_svm,
       rf =
         M_rf)
resamps <-
  resamples(modelList)
bwplot(resamps)



#Model testing


train_mean <- mean(Z_train$Y_train)
RMSE(rep(train_mean,length(Y_test)), Y_test)


RMSE(test_p_b, Y_test) #linear regression
RMSE(test_p_rf, Y_test) # random forest
RMSE(test_p_glmnet, Y_test) # elastic net
RMSE(test_p_svm, Y_test) # support vector machine


### Model stacking

test_stacked <-(test_p_b+test_p_glmnet+test_p_rf+test_p_svm)/4
RMSE(test_stacked, Y_test)
library(ggplot2)

test_stacked <-(test_p_b+test_p_glmnet+test_p_rf+test_p_svm)/4
RMSE(test_stacked, Y_test)


df_lm <- data.frame(Actual = Y_test, Predicted = test_p_b, Model = "Linear Regression")
df_rf <- data.frame(Actual = Y_test, Predicted = test_p_rf, Model = "Random Forest")
df_glmnet <- data.frame(Actual = Y_test, Predicted = test_p_glmnet, Model = "Elastic Net")
df_svm <- data.frame(Actual = Y_test, Predicted = test_p_svm, Model = "Support Vector Machine")
df_stacked <- data.frame(Actual = Y_test, Predicted = test_stacked, Model = "Stacked Model")

all_models <- rbind(df_lm, df_rf, df_glmnet, df_svm, df_stacked)

ggplot(all_models, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Model Predictions vs Actual Values",
       x = "Actual Values",
       y = "Predicted Values") +
  scale_color_discrete(name = "Model")