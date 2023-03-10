# Plotter for cleaned activity data.
library(ggplot2)
library("scales")

actDat <- read.csv("~/Documents/Fitabase Data 4.12.16-5.12.16/joined_activity_data_simplified.csv")

id <- actDat$CustomerID
minAsleep <- actDat$AvgMinAsleep
sd <- actDat$SD.from.ideal.sleep
eff <- actDat$SleepEfficiency

## Plot 1: density of sleep times.--------------------------

png(filename="avgSleepTime_2.png", width=2480, height=2480)

ggplot(actDat, aes(x=minAsleep)) + 
  geom_histogram(aes(y=after_stat(density)), 
                 colour="black", fill="white", linewidth=3) +
  geom_density(alpha=.2, fill="#FF6666", linewidth=3) + 
  annotate('rect', xmin=420, xmax=540, ymin=0, ymax=0.009, alpha=.5, fill="#99CCFF") +
  scale_x_continuous(expand = c(0,0), limits = c(0,800)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.009)) +
  labs(title="Distribution of sleep time",
       x="Time Asleep   (min)",y="Density") +
  theme(axis.text = element_text(size = 100),
        axis.title= element_text(size = 120),
        plot.title = element_text(size = 160),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=3),
        axis.ticks=element_line(linewidth=3),
        axis.ticks.length=unit(.7, "cm")) +
  scale_x_continuous(breaks = round(seq(0, 700, by = 100),1))

dev.off()

## Plot 2: bar plot with avg steps in ranges------------------------
tab1 <- table(cut(actDat$AvgSteps,breaks=seq.int(from=0,to=15000,by=2500)))
df1 <- as.data.frame((tab1))
lab <- c("0-2499","2500-4999","5000-7499","7500-9999","10,000-12,499","12,500-15,000")

png(filename="freqSteps.png", width=2480, height=2480)

ggplot(df1, aes(y=Var1,x=Freq)) + 
  geom_bar(stat="identity", aes(fill="#FF6666"), colour = "black", linewidth=3) +
  scale_y_discrete(labels=lab) +
  labs(title="Distribution of steps taken",
       x="Frequency",y="Step range") +
  theme(axis.text = element_text(size = 90),
        axis.text.y = element_text(angle = 45),
        axis.title= element_text(size = 120),
        plot.title = element_text(size = 140),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=3),
        axis.ticks=element_line(linewidth=3),
        axis.ticks.length=unit(.7, "cm"),
        legend.position = "none")

dev.off()


## Plot 3: correlation, vig act. vs. avgcalories?--------------------
png(filename="corrStepsCalories.png", width=2480, height=2480)

AvgAct <- actDat$AvgVigAct + actDat$AvgModAct

ggplot(actDat, aes(x=AvgAct,y=AvgCalories)) + 
  geom_point(size=20, aes(x=AvgAct,y=AvgCalories)) +
  geom_smooth(method = "lm", se = FALSE, color="red", linewidth=3) +
  annotate('rect', xmin=10, xmax=21, ymin=1500, ymax=3500, alpha=.5, fill="#99CCFF") +
  scale_x_continuous(expand = c(0,0), limits = c(0,max(AvgAct)+5)) +
  scale_y_continuous(expand = c(0,0), limits = c(1500,3500)) +
#  geom_point(size=20, aes(x=AvgModAct,y=AvgCalories, color="blue")) +
#  scale_color_manual(values = c("black","blue"), 
#                     labels = c("Vigorous activity","Moderate activity"), name = "") +
  labs(title="Amount of exercise\nvs. calories burned",
       x="Vigorous or moderate activity   (min/day)",y="Calories burned") +
  theme(axis.text = element_text(size = 100),
        axis.title= element_text(size = 110),
        plot.title = element_text(size = 140),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=3),
        axis.ticks=element_line(linewidth=3),
        axis.ticks.length=unit(.7, "cm"),
        plot.margin=unit(c(0,2,0,0), 'cm'))

dev.off()


## Plot 4: Most popular colours on request ---------------------------------

# import the colours
smCols <- read.csv("~/Documents/Fitabase Data 4.12.16-5.12.16/colour_comparison.csv")

# adjust colours to summarise other colours as "other"
smCols$Frequency[5] = sum(smCols$Frequency[5:24])
smCols$Unique.Colour[5] = "Others"

colDf <- data.frame(colItem = smCols$Unique.Colour[1:5],
                    colFreq = smCols$Frequency[1:5])

png(filename="colourFreq.png", width=2480, height=2480)

ggplot(colDf, aes(x="", y = colFreq, fill = factor(colItem))) +
  geom_col(color = "black") +
  geom_text(size=40,
            aes(label = colItem, x= 1.65),
            position = position_stack(vjust = 0.5)) +
  geom_text(size=30, aes(label = c("45%","7%","6%","5%","37%"),
                         x= 1.1),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#2E2E2E","#FFCC00","#3366FF","#CCCCCC","white")) + 
  labs(title="Most popular colours") +
  theme(legend.position = "none",
        axis.text = element_text(size = 0),
        axis.title= element_text(size = 0),
        plot.title = element_text(size = 140),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=3),
        axis.ticks=element_line(linewidth=3),
        axis.ticks.length=unit(.0, "cm"),
        plot.margin=unit(c(0,0,0,0), 'cm'))

dev.off()

## Plot 5: Plot whisker plot on expense vs. most popular brands---------------

# gather the data in one place. 
expDat <- read.csv("~/Documents/Fitabase Data 4.12.16-5.12.16/price_comparison.csv")

# group together price lists for each brand
prices <- data.frame(values = c(expDat$Apple,expDat$FitBit,expDat$Fossil,expDat$Garmin,expDat$Samsung),
                     group = c(rep("Apple",48),
                               rep("FitBit",48),
                               rep("Fossil",48),
                               rep("Garmin",48),
                               rep("Samsung",48)))

png(filename="priceDistribution.png", width=2480, height=2480)

ggplot(prices, aes(x=factor(reorder(group,desc(group))), y=values)) +
  geom_boxplot(linewidth=3, outlier.size=20, fill = "#FF6666") + 
  coord_flip() + 
  labs(title="Price range vs. brand",
       y="Price   (AUD)",x="Brand") +
  theme(axis.text = element_text(size = 100),
        axis.title= element_text(size = 120),
        plot.title = element_text(size = 160),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=3),
        axis.ticks=element_line(linewidth=3),
        axis.ticks.length=unit(.7, "cm"))

dev.off()

