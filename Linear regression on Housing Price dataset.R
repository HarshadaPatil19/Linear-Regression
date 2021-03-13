# 
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(gridExtra)
library(corrplot)
library(GGally)
library(e1071)
library(DAAG)

# Read Data File
data = read.csv("C:/Users/harsh/Downloads/Property_Price_Train.csv", na.strings=c("","NA"))
data

# Data dim
dim(data)

# structure of the data
str(data)

# Summary
summary(data)

# Data View
View(data)

# Remove ID variable containing unique values
data <- data[-1]
data

# Detecting NA values
colSums(is.na(data))

# Missing value analysis
Missing_index <- data %>% is.na() %>% colSums() * 100
Missing_index <- sort(round(Missing_index[Missing_index > 0], 2), decreasing  = TRUE)
Missing_index

View(Missing_index)

# % of NA's in dataframe
sum(is.na(data))/prod(dim(data)) * 100

# % of NA's contains as rows
nrow(data[!complete.cases(data),]) / nrow(data) * 100

# Segregating numeric and factor data
data.numeric <- data[sapply(data, is.numeric)]
data.numeric

data.factor <- data[sapply(data, is.factor)]
data.factor

dim(data.numeric)
dim(data.factor)

# Numerical data analysis for cleaning
str(data.numeric)
summary(data.numeric)

# Need to convert datatype from numeic to factor
# 1. Overall_Material
# 2. House_Condition
# 3. Construction_Year
# 4. Remodel_Year
# 5. Kitchen_Above_Grade
# 6. Rooms_Above_Grade
# 7. Fireplaces
# 8. Garage_Size
# 9. Garage_Built_Year
# 10. Month_Sold
# 11. Year_Sold
# 12. "Underground_Full_Bathroom",
# 13. "Underground_Half_Bathroom",
# 14. "Full_Bathroom_Above_Grade",
# 15. "Half_Bathroom_Above_Grade",
# 16. "Bedroom_Above_Grade",   


data$Overall_Material <- as.factor(data$Overall_Material)
data$House_Condition <- as.factor(data$House_Condition)
data$Construction_Year <- as.factor(data$Construction_Year)
data$Remodel_Year <- as.factor(data$Remodel_Year)
data$Kitchen_Above_Grade <- as.factor(data$Kitchen_Above_Grade)
data$Rooms_Above_Grade <- as.factor(data$Rooms_Above_Grade)
data$Fireplaces <- as.factor(data$Fireplaces)
data$Garage_Size <- as.factor(data$Garage_Size)
data$Garage_Built_Year <- as.factor(data$Garage_Built_Year)
data$Month_Sold <- as.factor(data$Month_Sold)
data$Year_Sold <- as.factor(data$Year_Sold)
data$Underground_Full_Bathroom <- as.factor(data$Underground_Full_Bathroom)
data$Underground_Half_Bathroom <- as.factor(data$Underground_Half_Bathroom)
data$Full_Bathroom_Above_Grade <- as.factor(data$Full_Bathroom_Above_Grade)
data$Half_Bathroom_Above_Grade <- as.factor(data$Half_Bathroom_Above_Grade)
data$Bedroom_Above_Grade <- as.factor(data$Bedroom_Above_Grade)

## Again data segregation
data.numeric <- data[sapply(data, is.numeric)]
data.numeric
dim(data.numeric)

data.factor <- data[sapply(data, is.factor)]
data.factor
dim(data.factor)

# Factor data analysis for cleaning
str(data.factor)
summary(data.factor)

# Remove the highly biassed variables "Road_Type" and "Utility_Type"
data <- data[(!colnames(data) %in% c("Utility_Type","Road_Type"))]
dim(data)

# Again data segregation
data.numeric <- data[sapply(data, is.numeric)]
data.numeric
dim(data.numeric)

data.factor <- data[sapply(data, is.factor)]
data.factor
dim(data.factor)

# Checking NA values in target variable
any(is.na(data$Sale_Price))

#Imputation with mean
for(i in seq(data.numeric)) {
  data.numeric[i]<- ifelse(is.na(data.numeric[,i]), 
                           median(data.numeric[,i], na.rm = T), data.numeric[,i])
}

colSums(is.na(data.numeric))

# Imputation with mode
# Mode function
#mode function
getmode <- function(x) {
  x <- x[!is.na(x)]
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

#imputation with mode
for(i in seq(data.factor))
  data.factor[,i][is.na(data.factor[,i])] <- getmode(data.factor[,i])

colSums(is.na(data))

str(data.factor)
summary(data.factor)

# Analysing histogram of each numeric values
numplot <- function(column, data)
{
  ggplot(data, aes_string(x=column)) +
    geaom_histogram(aes(y=..density..), fill = "grey", color = "black") +
    geaom_density(fill = 'blue', alpha=0.2) +
    xlab(column)
}

np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
np
do.call("grid.arrange", np)

# Check with skewness
data.skewed <- apply(data.numeric, c(2), skewness)
data.skewed

drops <- data.numeric[c("Lot_Size",
                        "Brick_Veneer_Area",
                        "BsmtFinSF2", 
                        "Second_Floor_Area",
                        "LowQualFinSF",
                        "Three_Season_Lobby_Area",  
                        "Screen_Lobby_Area",        
                        "Pool_Area",                
                        "Miscellaneous_Value")] 
drops

np_1 <- lapply(colnames(drops), numplot, data=drops);np_1
do.call("grid.arrange", np_1)

#Outlier 
out_std_check = function(x){
  m=mean(x)
  s=sd(x)
  lc=m-3*s #lower cut-off
  uc=m+3*s
  n=list( val=sum(x>uc | x<lc), lc=lc, uc=uc)
  return(n)
}

np <- apply(data.numeric, c(2), out_std_check)
np

out_std_fix = function(x){
  m=mean(x)
  s=sd(x)
  lc=m-3*s #lower cut-off
  uc=m+3*s
  out_value <- which(x > uc | x < lc)
  x[out_value] <- m
  return(x)
}


data.numeric <- apply(data.numeric, c(2), out_std_fix)
data.numeric <- as.data.frame(data.numeric)
View(data.numeric)

np <- lapply(colnames(data.numeric), numplot, data=data.numeric)
do.call("grid.arrange", np)

apply(data.numeric, c(2), skewness) 

corrplot::corrplot(cor(data.numeric))
corrplot.mixed(cor(data.numeric), lower.col = "black", number.cex = .7)

colnames(data.numeric)
data.numeric <- data.numeric[!colnames(data.numeric) %in% c("Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area")]


#Now factor analysis

#bar plot for categorical varibale etc.

factplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_bar(fill = "blue", color = "black", alpha= 0.2)+
    xlab(column)
}

#calling all bar plot
fp <- lapply(colnames(data.factor), factplot, df=data.factor)
fp
do.call("grid.arrange", fp)


drps <- c("Land_Outline", "Property_Slope", "Condition1","Condition2" 
          ,"House_Type", "Roof_Quality","Heating_Type" ,
          "BsmtFinType2","Functional_Rate", "Kitchen_Above_Grade",
          "Garage_Quality","Garage_Condition")


data.factor <- data.factor[!colnames(data.factor) %in% drps]

factors <- c("Construction_Year", "Remodel_Year","Neighborhood","Garage_Built_Year",
             "Month_Sold")

data.dummies <- data.factor[!colnames(data.factor) %in% factors]
data.dummies

#Significance ananlysis

annova <- function(x) {
  y <- data.numeric$Sale_Price
  q <- list(summary(aov(y~x)))
  return(q)
}

y <- data.numeric$Sale_Price

q <- (summary(aov(y~data.factor$Sale_Condition)))
q
signify <- apply(data.factor, c(2),annova)
signify


#dealing with year factor.
#these will need transformation like life of house by substraction built & remodled year
#for now just converting them into numeric
data.factor$Construction_Year <- as.numeric(data.factor$Construction_Year)
data.factor$Remodel_Year <- as.numeric(data.factor$Remodel_Year)
data.factor$Garage_Built_Year <- as.numeric(data.factor$Garage_Built_Year)
data.factor$Garage_Finish_Year <- as.numeric(data.factor$Garage_Finish_Year)
data.factor$Month_Sold <- as.numeric(data.factor$Month_Sold)
data.factor$Year_Sold <- as.numeric(data.factor$Year_Sold)

library(dummies)
data.factor <- dummy.data.frame(data.factor)
data.factor
# dim(data.dummies)
# dim(aq)

# data.factor <- data.factor[colnames(data.factor) %in% factors]
# data.factor <- cbind(data.factor, aq)
# 

data<- cbind(data.numeric, data.factor)
data
str(data)
dim(data)

colnames(data)

### Model 1

#sampling
set.seed(10)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
dim(train)
dim(test)
colnames(test1)
test1 <- test[!colnames(test) %in% "Sale_Price"]


#Applying lm model
model <- lm(Sale_Price~., data=train)
summary(model)


pred <- predict(model, test1)
View(pred)



results <- cbind(pred,test$Sale_Price) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)



# Grab residuals
res <- residuals(model)
res <- as.data.frame(res)
head(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

#Stepwise modeling

#stepwise sleection:

nullModel<- lm(Sale_Price ~ 1, train)

#summary(nullModel)

fullmodel <- lm(Sale_Price~.,data = (train))
#summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit



model <- lm(formula = Sale_Price ~ Grade_Living_Area + Construction_Year + 
              Total_Basement_Area + Overall_Material8 + Overall_Material7 + 
              BsmtFinSF1 + Fireplaces0 + House_Condition7 + Overall_Material9 + 
              Kitchen_QualityGd + Overall_Material6 + Zoning_ClassRMD + 
              Sale_ConditionNormal + NeighborhoodTimber + NeighborhoodSomerst + 
              Building_Class160 + House_Condition2 + Exterior_ConditionEx + 
              Garage_Size0 + House_Condition8 + Rooms_Above_Grade12 + NeighborhoodCrawfor + 
              NeighborhoodBrkSide + House_Condition6 + House_Condition9 + 
              House_Condition5 + NeighborhoodNridgHt + Sale_TypeConLI + 
              Rooms_Above_Grade11 + Full_Bathroom_Above_Grade3 + Property_ShapeIR3 + 
              Underground_Full_Bathroom0 + Building_Class75 + Garage_Size4 + 
              NeighborhoodBrDale + Exterior_MaterialGd + Exposure_LevelAv + 
              NeighborhoodClearCr + Garage_Size1 + NeighborhoodNoRidge + 
              Rooms_Above_Grade9 + Foundation_TypeW + Exposure_LevelNo + 
              Zoning_ClassCommer + NeighborhoodIDOTRR + Bedroom_Above_Grade0 + 
              Exterior1stStone + Lot_ConfigurationC + Rooms_Above_Grade5 + 
              Bedroom_Above_Grade1 + Exterior1stHdBoard + Exterior2ndVinylSd + 
              Bedroom_Above_Grade8 + Building_Class90 + Underground_Full_Bathroom1 + 
              Overall_Material10 + Garage_Size2 + `Exterior2ndWd Shng` + 
              BsmtFinType1GLQ + Basement_HeightEx + NeighborhoodBlmngtn + 
              GarageAttchd + House_Design2.5Unf + Exterior1stBrkComm, data = train)



summary(model)
plot(model)

# 
# x <- factor(c("A","B","A","C","D","E","A","E","C"))
# x
# library(car)
# x <- recode(x, "c('A', 'B')='A+B';c('D', 'E') = 'D+E'")
# x

### Model 2
#sampling
set.seed(20)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
dim(train)
dim(test)
colnames(test1)
test1 <- test[!colnames(test) %in% "Sale_Price"]


#Applying lm model
model <- lm(Sale_Price~., data=train)
summary(model)


pred <- predict(model, test1)
View(pred)



results <- cbind(pred,test$Sale_Price) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)



# Grab residuals
res <- residuals(model)
res <- as.data.frame(res)
head(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

#Stepwise modeling

#stepwise sleection:

nullModel<- lm(Sale_Price ~ 1, train)

summary(nullModel)

fullmodel <- lm(Sale_Price~.,data = (train))
summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit



model <- lm(formula = Sale_Price ~ Grade_Living_Area + Construction_Year + 
              Total_Basement_Area + Overall_Material8 + Overall_Material7 + 
              BsmtFinSF1 + Fireplaces0 + House_Condition7 + Overall_Material9 + 
              Kitchen_QualityGd + Overall_Material6 + Zoning_ClassRMD + 
              Sale_ConditionNormal + NeighborhoodTimber + NeighborhoodSomerst + 
              Building_Class160 + House_Condition2 + Exterior_ConditionEx + 
              Garage_Size0 + House_Condition8 + Rooms_Above_Grade12 + NeighborhoodCrawfor + 
              NeighborhoodBrkSide + House_Condition6 + House_Condition9 + 
              House_Condition5 + NeighborhoodNridgHt + Sale_TypeConLI + 
              Rooms_Above_Grade11 + Full_Bathroom_Above_Grade3 + Property_ShapeIR3 + 
              Underground_Full_Bathroom0 + Building_Class75 + Garage_Size4 + 
              NeighborhoodBrDale + Exterior_MaterialGd + Exposure_LevelAv + 
              NeighborhoodClearCr + Garage_Size1 + NeighborhoodNoRidge + 
              Rooms_Above_Grade9 + Foundation_TypeW + Exposure_LevelNo + 
              Zoning_ClassCommer + NeighborhoodIDOTRR + Bedroom_Above_Grade0 + 
              Exterior1stStone + Lot_ConfigurationC + Rooms_Above_Grade5 + 
              Bedroom_Above_Grade1 + Exterior1stHdBoard + Exterior2ndVinylSd + 
              Bedroom_Above_Grade8 + Building_Class90 + Underground_Full_Bathroom1 + 
              Overall_Material10 + Garage_Size2 + `Exterior2ndWd Shng` + 
              BsmtFinType1GLQ + Basement_HeightEx + NeighborhoodBlmngtn + 
              GarageAttchd + House_Design2.5Unf + Exterior1stBrkComm, data = train)



summary(model)
plot(model)
