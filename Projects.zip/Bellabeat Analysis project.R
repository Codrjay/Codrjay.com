library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

Daily_activities <- read.csv("C:/Users/stu/Desktop/IT/Data Analysis/archive (2)/Data/R Stuff/dailyActivity_merged.csv")

Daily_sleep <- read.csv("C:/Users/stu/Desktop/IT/Data Analysis/archive (2)/Data/sleepDay_merged.csv")

Daily_Calories <- read.csv("C:/Users/stu/Desktop/IT/Data Analysis/archive (2)/Data/dailyCalories_merged.csv")

weight <- read.csv("C:/Users/stu/Desktop/IT/Data Analysis/archive (2)/Data/weightLogInfo_merged.csv")


Daily_steps <- read.csv("C:/Users/stu/Desktop/IT/Data Analysis/archive (2)/Data/dailySteps_merged.csv")

colnames(Daily_activities)
colnames(Daily_sleep)
colnames(Daily_Calories)
colnames(weight)
colnames(Daily_steps)

Daily_activities <- Daily_activities %>% mutate(row_id = row_number())
Daily_sleep <- Daily_sleep %>% mutate(row_id = row_number())
merged_df1 <- merge(Daily_activities, Daily_sleep, by = "row_id")

merged_df1<- merged_df1 %>% mutate(row_id = row_number())

weight<- weight %>% mutate(row_id = row_number()) %>% drop_na(WeightKg)

master_df<- merge(merged_df1, weight, by = "row_id")

master_df

mutate(WeightClass = cut(WeightKg, breaks = seq(52.6, 133.5, by = 5),



ggplot(master_df[!is.na(master_df$WeightClass),], aes(x = WeightClass)) +
  geom_bar() +
  xlab("Weight Class (kg)") +
  ylab("Frequency") +
  ggtitle("Distribution of Weight Classes")



master_df$BMI_category <- ifelse(master_df$BMI < 18.5, "Underweight",
                          ifelse(master_df$BMI < 25, "Ideal",
                                 ifelse(master_df$BMI < 30, "Overweight", "Obese")))


ggplot(data=master_df, aes(x=TotalDistance,y=Calories,color=TotalDistance))+geom_point()+labs(x="Total Distance",y="Calories")+
  ggtitle("Comparison of Total Distance travelled to calories burned")
correlation <- cor(master_df$TotalDistance,master_df$Calories )
correlation

ggplot(data=master_df[!is.na(master_df$WeightClass),], aes(x=TotalSteps,y=Calories))+geom_point()+labs(x="Total Daily steps",y="Calories")+
  ggtitle("Comparison of Total steps taken to Calories burned")
correlation <- cor(master_df$TotalSteps,master_df$Calories )
#There is a moderate positive correlation between number of steps taken and calories burned.i.e when the higher the number of steps, the number of calories burned tends to increase.

ggplot(data=master_df[!is.na(master_df$WeightClass),], aes(x=TotalDistance,y=VeryActiveMinutes))+geom_point()+labs(x="Total Daily steps",y="weight")+
  ggtitle("Comparison of Active minutes to Calories burned ")
correlation <- cor(master_df$TotalDistance,master_df$VeryActiveMinutes)
correlation
# This could suggest that individuals who engage in more very active physical activity tend to be more active in general and are more likely to travel longer distances.








ggplot(master_df,aes(x=TotalMinutesAsleep, y=VeryActiveMinutes)) + geom_point()+
  geom_smooth(method = "lm", se=FALSE)
cor1<-cor(master_df$Calories,master_df$TotalMinutesAsleep)


#I used a box plot to compare the BMI category to see which category burns the most calories
ggplot(master_df[!is.na(master_df$BMI_category),], aes(BMI_category, Calories, fill=BMI_category)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Calories burned by BMI category", x="BMI category") +
  theme(legend.position="none", text = element_text(angle=360),plot.title = element_text(hjust = 0.5))
# We can see from the plot that the Ideal BMI category burns the most calories although some users in the Overweight category  burn more calories than the some in the ideal category

#I used a box plot to compare the BMI category to see which category sleeps the most
ggplot(master_df[!is.na(master_df$BMI_category),], aes(BMI_category, TotalMinutesAsleep, fill=BMI_category)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Minutes slept by BMI category", x="BMI category") +
  theme(legend.position="none", text = element_text(angle=360),plot.title = element_text(hjust = 0.5))
# We can see from the plot that the people in the Overweight category sleep longer than the people in the obese and ideal category.

ggplot(master_df[!is.na(master_df$BMI_category),], aes(BMI_category, TotalDistance, fill=BMI_category)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Total Steps Taken", x="BMI category") +
  theme(legend.position="none", text = element_text(angle=360),plot.title = element_text(hjust = 0.5))


ggplot(master_df[!is.na(master_df$BMI_category),], aes(BMI_category, VeryActiveMinutes, fill=BMI_category)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Total Steps Taken", x="BMI category") +
  theme(legend.position="none", text = element_text(angle=360),plot.title = element_text(hjust = 0.5))

#I want to know which BMI category is the most Active
master_df$TotalActiveMinutes <- rowSums(master_df[, c("VeryActiveMinutes", "LightlyActiveMinutes", "FairlyActiveMinutes")], na.rm = TRUE)
ggplot(master_df, aes(x=TotalActiveMinutes,y=Calories,color=TotalActiveMinutes))+geom_point()+labs(x="Total Active Minutes",y="Calories burned")+ggtitle("Comparison of Total Active Minutes to Calories burned")
aggregate(TotalActiveMinutes ~ BMI_category, data = master_df, FUN = mean)


ggplot(master_df[!is.na(master_df$BMI_category),], aes(x = BMI_category, y = TotalActiveMinutes, fill=BMI_category)) +
  geom_boxplot() +
  labs(x = "BMI Category", y = "Total Active Minutes")
#From our plot, the Ideal category is the most active once again, followed by the obese category, then the overweight , I'm really proud of the obese guy by the way.

correlation <- cor(master_df$TotalActiveMinutes,master_df$Calories)
correlation
