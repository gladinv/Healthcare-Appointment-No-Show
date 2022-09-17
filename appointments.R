# load libraries
library(dplyr)
library(ggplot2)
library( lubridate)
library(gridExtra)

# load data
appointment_data <- read.csv("appointments.csv", stringsAsFactors
                                  = TRUE) ## snapshot data
print(head(appointment_data))

# explore the data to check for missing values/erroneous entries
# missing values
print(sapply(appointment_data, function(x) sum(is.na(x))))

# summary stats
summary(appointment_data)

# Check for anomalies in age data and rectify them. Identify and remove outliers
summary(appointment_data$Age)

# imputing with mean of age 
appointment_data[appointment_data$Age < 0, 'Age'] <- mean(appointment_data[!appointment_data$Age < 0, 'Age'])

# studying for outliers
ggplot(appointment_data, aes(y = '', x = Age))+ geom_boxplot()

# find upper limit and lower limit & remove outliers
lower_limit <- quantile(appointment_data$Age, 0.25) - 1.5 * IQR(appointment_data$Age)
upper_limit <- quantile(appointment_data$Age, 0.75) + 1.5 * IQR(appointment_data$Age)

appointment_data <- appointment_data %>% filter(Age < upper_limit & Age > lower_limit)

ggplot(appointment_data, aes(y = '', x = Age))+ geom_boxplot()

# split date and time
f <- function(x){
  d <- strsplit(x,"T")[[1]][2]
  return(gsub('Z', '', d))
}

appointment_data$AppointmentRegistration <- as.character(appointment_data$AppointmentRegistration)

time_of_regis <- sapply(appointment_data$AppointmentRegistration,f )

appointment_data$HourOfTheDay <- hour(hms(time_of_regis ))

appointment_data$DayOfTheWeek <- wday(as.Date(as.character(appointment_data$AppointmentRegistration)), label = TRUE)

# plot the no. of appointment based on hour
count <- data.frame(table(appointment_data$HourOfTheDay,
                          appointment_data$Status))
ggplot(count, aes(x = Var1, y = Freq, fill = Var2))+ geom_bar(stat = 'identity')+
  labs(x = 'Hour of the day', fill = 'Show/Noshow')+ theme(legend.position = 'bottom')

# find out duration between registration date and appointment date as 'duration'
appointment_data$Duration<- (as.Date(as.character(appointment_data$AppointmentDate)) -
                               
                               as.Date(as.character(appointment_data$AppointmentRegistration)))

# study the overall distribution of target variable
count <- table(appointment_data$Status)
pct <- round(count/sum(count)*100, 1)
lab <- paste(names(count),'-', pct, '%', sep = '')
pie(count, labels = lab,col = hcl.colors(2,'Set3'))

# no Show for booking hour/Day and duration
new_data <- appointment_data[appointment_data$Status == 'No-Show',] 
count <- data.frame(table(new_data$HourOfTheDay))

ggplot(count, aes(x = Var1, y = Freq))+ geom_bar(stat = 'identity')+ geom_line(group = 1, lwd = 2)+
  labs(x = 'Hour of the day')+ theme(legend.position = 'bottom')
count <- data.frame(table(new_data$DayOfTheWeek))

ggplot(count, aes(x = Var1, y = Freq))+ geom_bar(stat = 'identity')+ geom_line(group = 1, lwd = 2)+
  labs(x = 'DayOfTheWeek')+ theme(legend.position = 'bottom')
count <- data.frame(table(new_data$Duration))

ggplot(count, aes(x = Var1, y = Freq))+ geom_bar(stat = 'identity')+ geom_line(group = 1, lwd = 2)+
  labs(x = 'Duration')+ theme(legend.position = 'bottom')

# study based on presence of a condition
# diabetes
new_data <- appointment_data[appointment_data$Status == 'No-Show',]
col_list <- c("Diabetes" , "Alcoholism", "HyperTension", "Handicap",
              "Smokes", "Scholarship", "Tuberculosis" )
par(mfrow = c(4,2))
for (col in col_list)
  count <- table(new_data[col])
  names(count) <- c(paste(col,"yes"), paste(col,"no"))
  perc <- round(count/sum(count)*100,1)
  labs <- paste(names(count),'-',perc,'%', sep = '')
  pie(count, labels = labs,
      col = hcl.colors(2,'Set2'),
      main = paste('Percentage of', col, 'VS No Show', sep = ' '))
  
# model building and prediction
  
col_list <- c("Diabetes" , "Alcoholism", "HyperTension", "Handicap",
                "Smokes", "Scholarship", "Tuberculosis" )
appointment_data[col_list]<- lapply(appointment_data[col_list],
                                    as.factor)

# model logistic
# convert target to 1-0 show = 1& no show = 0
appointment_data$Status <-
  as.factor(ifelse(appointment_data$Status=='Show-Up', 1, 0))

# select data for analysis
appointment_data <- appointment_data %>% select(-AppointmentRegistration, -
                                                  AppointmentDate)

# split
train_indicez<- sample(1 : nrow(appointment_data),0.75 *
                         nrow(appointment_data) )
train <- appointment_data[train_indicez,]
test <- appointment_data[-train_indicez,]

# build model
logreg <- glm(Status~., data = train, family = 'binomial')

# predict 
pred <- predict(logreg,test, 'response')
pred_class <- as.factor(ifelse(pred>0.5, 1, 0))

#evaluate
caret::confusionMatrix(pred_class, test$Status, positive = '1' )
