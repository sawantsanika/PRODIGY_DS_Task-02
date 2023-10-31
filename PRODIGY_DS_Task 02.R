'Exploratory Data Analysis (EDA) on the Titanic Dataset'

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)

titanic_data = read.csv("C:/Users/HP/Desktop/INTERNSHIP/PRODIGY/train.csv")

str(titanic_data)

"This summary shows us that the Titanic training dataset has 891 rows &
 12 columns. The columns include the passenger's ID, whether or not they
 survived, their ticket class, name, sex, age, number of siblings & spouses 
 on board, number of parents and children on board, ticket number, fare, 
 cabin number & port of embarkation"

sum(is.na(titanic_data))
'Since I have replaced blank spaces by 0'

summary(titanic_data)   #Summary Stats

head(titanic_data)




'Distribution of passengers by class'
class_plot = ggplot(titanic_data, aes(x = Pclass, fill = factor(Pclass))) +
  geom_bar() +
  labs(title = "Passenger Class Distribution",
  x = "Class",
  y = "Count") +
  scale_fill_discrete(name = "Class")

print(class_plot)

"INTERPRETATION"
'there are significantly more passengers in third class than in first 
 or second class. This is with the fact that third class 
 tickets were the cheapest, and therefore most affordable for the 
 majority of passengers.'




'distribution of passengers by gender'
gender_plot = ggplot(titanic_data, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  scale_fill_discrete(name = "Gender")

print(gender_plot)

"INTERPRETATION"
' The plot shows that there are significantly more male passengers 
 than female passengers on the Titanic. There are approximately 61.2% 
 male passengers and 38.8% female passengers.'




'age distribution of passengers'
age_plot = ggplot(titanic_data, aes(x = Age, fill = Sex)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count") +
  scale_fill_discrete(name = "Gender")

print(age_plot)

"INTERPRETATION"
'The age distribution plot of the Titanic passengers shows that the 
 majority of passengers were between the ages of 20 and 40.'


grid.arrange(class_plot, gender_plot, age_plot, ncol = 2)

'Survival rate by class'
survival_by_class = titanic_data %>%
  group_by(Pclass) %>%
  summarize(SurvivalRate = mean(Survived, na.rm = TRUE))
survival_by_class

"INTERPRETATION"
'The table shows that passengers in first class had the highest survival rate,
 followed by passengers in second class and then passengers in third class.'

'Survival rate by gender'
survival_by_gender = titanic_data %>%
  group_by(Sex) %>%
  summarize(SurvivalRate = mean(Survived, na.rm = TRUE))
survival_by_gender
"INTERPRETATION"
'The table shows that female passengers had a significantly higher
 survival rate than male passengers.'




'Box plot of fares by class'
box_plot_fare_class = ggplot(titanic_data, aes(x = Pclass, y = Fare, 
  fill = factor(Pclass))) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Class",
       x = "Class",
       y = "Fare") +
  scale_fill_discrete(name = "Class")
print(box_plot_fare_class)

"INTERPRETATION"
'The box plot of fares by class shows that there is a clear difference in 
 fare distribution between the three classes. First class passengers paid the
 highest fares, followed by second class passengers & then third class passengers.

 The median fare for first class passengers is approximately £59.40, while 
 the median fare for second class passengers is approximately £15.00 and the 
 median fare for third class passengers is approximately £8.05. This 
 suggests that first class passengers were significantly wealthier than
 second class passengers and third class passengers.'

#correlation matrix
numeric_vars = select_if(titanic_data, is.numeric)
correlation_matrix = cor(numeric_vars)


library(corrplot)
corrplot(correlation_matrix, col = c("red", "blue"))

'INTERPRETATION'

"Survived and Pclass: 
 There is a moderate negative correlation between survival and Pclass (r=-0.3385). 
 This means that passengers in higher classes were more likely to survive than 
 passengers in lower classes.
 
 Age and Survival: There is a weak negative correlation between age and survival
 (r=-0.0105). This means that younger passengers were slightly more likely to 
 survive than older passengers.

 SibSp and Survival: There is a weak negative correlation between the number 
 of siblings and spouses on board and survival (r=-0.0353). This means that 
 passengers with more siblings and spouses on board were slightly less likely 
 to survive.

 Parch and Survival: There is a weak positive correlation between the number 
 of parents and children on board and survival (r=0.0816). This means that 
 passengers with more parents and children on board were slightly more likely
 to survive.

 Fare and Survival: There is a moderate positive correlation between fare and 
 survival (r=0.2573). This means that passengers who paid higher fares were more
 likely to survive."


