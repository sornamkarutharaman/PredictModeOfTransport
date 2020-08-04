#======================================================
#     PredictModeOfTransport - EDA
#======================================================

# Load the Dataset -----------------------------
setwd("<directory>")

#Install the required packages
library(pacman)
p_load(readr,dplyr,ggplot2,tidyr)

#Load the Dataset
Cars_dataset <- read_csv("<dataset>")
View(Cars_dataset)


# Dimension of dataset 
dim(Cars_dataset)

colnames(Cars_dataset)
#Renaming column name Work Exp without spaces
colnames(Cars_dataset)[which(colnames(Cars_dataset)=='Work Exp')] <- "Work_Experience"


# Structure of the Dataset
Cars_dataset$Gender = as.factor(Cars_dataset$Gender)
Cars_dataset$Transport = as.factor(Cars_dataset$Transport)
Cars_dataset$Engineer = as.factor(Cars_dataset$Engineer)
Cars_dataset$MBA = as.factor(Cars_dataset$MBA)
Cars_dataset$license = as.factor(Cars_dataset$license)
str(Cars_dataset)
library(DataExplorer)
plot_str(Cars_dataset)

#Summary of Datset
summary(Cars_dataset)

#======================================================
#Univariate Analysis
#======================================================
# Variable Age------------------
par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))  # Sets outside margins: b, l, t, r
par(mar = c(4, 1, 4, 1))  # Sets plot margins: b, l, t, r
plot_histogram(Cars_dataset$Age)
plot_density()

# Histogram
hist(Cars_dataset$Age,
     prob = TRUE,             
     breaks = 20,
     col = "DarkGreen",       
     border = 0,
     xlab="Age",
     main = "Age of the Employees")

# Normal density curve
curve(dnorm(x, mean = mean(Cars_dataset$Age), sd = sd(Cars_dataset$Age)), 
      col = "darkred", 
      lwd = 2,
      add = TRUE)

# Histogram: Kernel density lines
lines(density(Cars_dataset$Age), col = "blue", lwd = 2)


qqnorm(Cars_dataset$Age, 
       xlab = "Age",
       main="Normal Q-Q plot of Age")

qqline(Cars_dataset$Age)

boxplot(Cars_dataset$Age, 
        xlab = "Cars_dataset$Age",
        main="Boxplot of Age")


# Variable Work_Experience------------------
par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))  # Sets outside margins: b, l, t, r
par(mar = c(4, 1, 4, 1))  # Sets plot margins: b, l, t, r
plot_histogram(Cars_dataset$Work_Experience)
plot_density()

# Histogram
hist(Cars_dataset$Work_Experience,
     prob = TRUE,             
     breaks = 20,
     col = "LightBlue",       
     border = 0,
     xlab="Age",
     main = "WorkExperience of the Employees")

# Normal density curve
curve(dnorm(x, mean = mean(Cars_dataset$Work_Experience), sd = sd(Cars_dataset$Work_Experience)), 
      col = "darkred", 
      lwd = 2,
      add = TRUE)

# Histogram: Kernel density lines
lines(density(Cars_dataset$Work_Experience), col = "blue", lwd = 2)


qqnorm(Cars_dataset$Work_Experience, 
       xlab = "Age",
       main="Normal Q-Q plot of WorkExperience")

qqline(Cars_dataset$Work_Experience)

boxplot(Cars_dataset$Work_Experience, 
        xlab = "Cars_dataset$Work_Experience",
        main="Boxplot of WorkExperience")

temp1 <- filter(Cars_dataset, Cars_dataset$Age >35)
View(temp1)



# Variable Salary------------------
par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))  # Sets outside margins: b, l, t, r
par(mar = c(4, 1, 4, 1))  # Sets plot margins: b, l, t, r
plot_histogram(Cars_dataset$Salary)
plot_density()

# Histogram
hist(Cars_dataset$Salary,
     prob = TRUE,             
     breaks = 20,
     col = "Pink",       
     border = 0,
     xlab="Salary",
     main = "Salary of the Employees")

# Normal density curve
curve(dnorm(x, mean = mean(Cars_dataset$Salary), sd = sd(Cars_dataset$Salary)), 
      col = "darkred", 
      lwd = 2,
      add = TRUE)

# Histogram: Kernel density lines
lines(density(Cars_dataset$Salary), col = "blue", lwd = 2)


qqnorm(Cars_dataset$Salary, 
       xlab = "Salary",
       main="Normal Q-Q plot of Salary")

qqline(Cars_dataset$Salary)

boxplot(Cars_dataset$Salary, 
        xlab = "Cars_dataset$Salary",
        main="Boxplot of Salary")

temp1 <- filter(Cars_dataset, Cars_dataset$Salary >17)
View(temp1)

# Variable Distance------------------
par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))  # Sets outside margins: b, l, t, r
par(mar = c(4, 1, 4, 1))  # Sets plot margins: b, l, t, r
plot_histogram(Cars_dataset$Distance)
plot_density()

# Histogram
hist(Cars_dataset$Distance,
     prob = TRUE,             
     breaks = 20,
     col = "Orange",       
     border = 0,
     xlab="Distance",
     main = "Distance from work-home")

# Normal density curve
curve(dnorm(x, mean = mean(Cars_dataset$Distance), sd = sd(Cars_dataset$Distance)), 
      col = "darkred", 
      lwd = 2,
      add = TRUE)

# Histogram: Kernel density lines
lines(density(Cars_dataset$Distance), col = "blue", lwd = 2)


qqnorm(Cars_dataset$Distance, 
       xlab = "Distance",
       main="Normal Q-Q plot of Distance")

qqline(Cars_dataset$Distance)

boxplot(Cars_dataset$Distance, 
        xlab = "Cars_dataset$Distance",
        main="Boxplot of Distance")

temp1 <- filter(Cars_dataset, Cars_dataset$Transport = 'Car')
View(temp1)

# BAR plot for categorical variables-------------------
par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))
par(mar=c(4, 1, 4, 1))

barplot(table(Cars_dataset$Gender)[order(table(Cars_dataset$Gender))],
        col= c("bisque1", "bisque2"),
        xlab="Gender",
        ylab="count",
        main="Number of Employees\n by Gender",
        ylim=c(0, 300))

barplot(table(Cars_dataset$Engineer)[order(table(Cars_dataset$Engineer))], 
        col= c("bisque1", "bisque2"),
        xlab="Engineer",
        main="Number of Engineers ", 
        ylim=c(0, 300))

barplot(table(Cars_dataset$MBA)[order(table(Cars_dataset$MBA))],
        col= c("bisque1", "bisque2"),
        xlab="MBA",
        main="Number of Employees by\n  holding MBA degree",
        ylim=c(0, 300))



par(mfrow=c(1,3))
par(oma = c(1, 1, 1, 1))
par(mar=c(4, 1, 4, 1))
library(plotrix)
GenderPie <- table(Cars_dataset$Gender)
GenderPieLabel <- paste(names(GenderPie), "\n", GenderPie, sep="")
pie3D(GenderPie,radius = 0.9, labels = GenderPieLabel,  col=c("darkgreen","red"),
      main="Pie Chart for Gender")

EngineerPie <- table(Cars_dataset$Engineer)
EngineerPieLabel <- paste(names(EngineerPie), "\n", EngineerPie, sep="")
pie3D(EngineerPie,radius = 0.9, labels = EngineerPieLabel,  col=c("orange","red"),
      main="Pie Chart for Engineers")

MBAPie <- table(Cars_dataset$MBA)
MBAPieLabel <- paste(names(MBAPie), "\n", MBAPie, sep="")
pie3D(MBAPie,radius = 0.9, labels = MBAPieLabel,  col=c("lightblue","red"),
      main="Pie Chart for MBA")


par(mfrow=c(1,2))
par(oma = c(1, 1, 1, 1))
par(mar=c(4, 1, 4, 1))
barplot(table(Cars_dataset$license)[order(table(Cars_dataset$license))], 
        col= c("bisque1", "bisque2"),
        xlab="license",
        main=" Employees having\n license",
        ylim=c(0, 400))

barplot(table(Cars_dataset$Transport)[order(table(Cars_dataset$Transport))],
        xlab="ModeofTransport", 
        col= c("bisque1", "bisque2"), 
        main="Mode of transport used\n by employees to commute",
        ylim=c(0, 400))



par(mfrow=c(1,2))
par(oma = c(1, 1, 1, 1))
par(mar=c(0,0,0,0))

licensePie <- table(Cars_dataset$license)
licensePieLabel <- paste(names(licensePie), "\n", licensePie, sep="")
pie3D(licensePie,radius = 0.9, labels = licensePieLabel,  col=c("darkgreen","red"),
      main="Pie Chart for license")

TransportPie <- table(Cars_dataset$Transport)
TransportPieLabel <- paste(names(TransportPie), "\n", TransportPie, sep="")
pie3D(TransportPie,radius = 0.9, labels = TransportPieLabel,  col=c("orange","red"),
      main="Pie Chart for Transport")


#======================================================
#Bivariate Analysis
#======================================================

par(mfrow=c(1, 2))
par(oma=c(1, 1, 1, 1))
par(mar=c(4, 1, 4, 1))
# age vs work experience


plot(Cars_dataset$Age,
     Cars_dataset$Work_Experience,
     col="Darkgreen", 
     pch=16, 
     main="Employees work experience\n by Age", 
     xlab="Employee's age", 
     ylab="Work Experience")


abline(lm(Cars_dataset$Work_Experience ~ Cars_dataset$Age),
       col="red",
       lwd=2)

lines(lowess (Cars_dataset$Age, Cars_dataset$Work_Experience), 
      col = "blue", 
      lwd = 2)  


plot(Cars_dataset$Age,
     Cars_dataset$Salary,
     col="Red", 
     pch=16, 
     main="Employees Salary\n by Age", 
     xlab="Employee's age", 
     ylab="Salary")


abline(lm(Cars_dataset$Salary ~ Cars_dataset$Age),
       col="DarkGreen",
       lwd=2)

lines(lowess (Cars_dataset$Age, Cars_dataset$Salary), 
      col = "orange", 
      lwd = 2)  

#======================================================
## continuous vs categorical  Transport vs all continous
#======================================================
library(ggplot2)
ggplot(Cars_dataset, aes(x=Cars_dataset$Transport, y= Cars_dataset$Age , colour = Cars_dataset$Transport)) + geom_jitter()

par(mfrow=c(1, 3))
par(oma=c(1, 1, 1, 1))
par(mar=c(4, 1, 4, 1))

ggplot(Cars_dataset, aes(x=Cars_dataset$Transport, y= Cars_dataset$Work_Experience , colour = Cars_dataset$Transport)) + geom_jitter()
ggplot(Cars_dataset, aes(x=Cars_dataset$Transport, y= Cars_dataset$Salary , colour = Cars_dataset$Transport)) + geom_jitter()
ggplot(Cars_dataset, aes(x=Cars_dataset$Transport, y= Cars_dataset$Distance , colour = Cars_dataset$Transport)) + geom_jitter()

#======================================================
# Category - category - Transport and gender
#======================================================
ggplot(Cars_dataset, 
       aes(x = Cars_dataset$Transport,
           fill = Cars_dataset$Gender)) +
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(Cars_dataset, 
       aes(x = Cars_dataset$Transport,
           fill = Cars_dataset$Engineer)) +
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(Cars_dataset, 
       aes(x = Cars_dataset$Transport,
           fill = Cars_dataset$MBA)) +
  geom_bar(position = position_dodge(preserve = "single"))


ggplot(Cars_dataset, 
       aes(x = Cars_dataset$Transport,
           fill = Cars_dataset$license)) +
  geom_bar(position = position_dodge(preserve = "single"))

#======================================================
# Check the missing values in the dataset
#======================================================
anyNA(Cars_dataset)
sum(is.na(Cars_dataset))
install.packages("Amelia")
library(Amelia)
missmap(Cars_dataset)
colnames(Cars_dataset)[apply(is.na(Cars_dataset), 2, any)] #find column names that have NA
unique (unlist (lapply (Cars_dataset, function (x) which (is.na (x)))))# unique rows of NA - 243
#replace with mode for NA value - categorical variables

library(modeest)
mfv(Cars_dataset$MBA) # 0 is the most frequent value
counts_MBA <- table(Cars_dataset$MBA)
# Another method to ensure 
barplot(counts_MBA, main="Car Distribution - MBA",
        xlab="Number of MBA Done") 
Cars_dataset[is.na(Cars_dataset)] = 0 
anyNA(Cars_dataset)
sum(is.na(Cars_dataset))

# Variable transformation - Transport 
Cars_dataset$Transport = ifelse(Cars_dataset$Transport=='Car',1,0)


Cars_dataset_Modify <- Cars_dataset
Cars_dataset_Modify$license <- replace(Cars_dataset_Modify$license,Cars_dataset_Modify$Transport == '1' , 1)


str(Cars_dataset)
Cars_dataset$Transport = as.factor(Cars_dataset$Transport)


