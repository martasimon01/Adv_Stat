#PACKAGES
library(dplyr)
library(gplots)
library(stringr)
library(corrplot)
library(caTools)
library(pROC)
library(readr)
library(ggplot2)
library(ggfortify)
library(dendextend)

#IMPORT DATASET AND PRELIMINARY CHECKS
data = read.csv('data.csv')
sapply(data, class)
sapply(data, summary)

#SELECT THE COLUMNS THAT WE CONSIDER  RELEVANT
data = data %>%
  select(Month, Age, Annual_Income:Credit_History_Age, Total_EMI_per_month,
         Amount_invested_monthly, Monthly_Balance, Credit_Score)

#CLEANING THE DATASET
data$Age = gsub('_', '', data$Age)
data$Age = as.numeric(data$Age) 

data$Annual_Income = gsub('_', '', data$Annual_Income)
data$Annual_Income = as.numeric(data$Annual_Income) 

data$Num_of_Loan = gsub('_', '', data$Num_of_Loan)
data$Num_of_Loan = as.numeric(data$Num_of_Loan) 

data$Num_of_Delayed_Payment = gsub('_', '', data$Num_of_Delayed_Payment)
data$Num_of_Delayed_Payment = as.numeric(data$Num_of_Delayed_Payment) 

data$Changed_Credit_Limit = gsub('_', 0, data$Changed_Credit_Limit)
data$Changed_Credit_Limit = as.numeric(data$Changed_Credit_Limit) 

data$Credit_Mix = gsub('_', NA, data$Credit_Mix)

data$Outstanding_Debt = gsub('_', '', data$Outstanding_Debt)
data$Outstanding_Debt = as.numeric(data$Outstanding_Debt)

anni_e_mesi <- str_match(data$Credit_History_Ag, "(\\d+) Years and (\\d+) Months")
anni <- as.numeric(anni_e_mesi[, 2])  
mesi <- as.numeric(anni_e_mesi[, 3])
Age_in_years <- anni + (mesi / 12)
data$Credit_History_Age = Age_in_years

data$Amount_invested_monthly = gsub('__10000__', NA, data$Amount_invested_monthly)
data$Amount_invested_monthly = as.numeric(data$Amount_invested_monthly) 

data$Monthly_Balance = as.numeric(data$Monthly_Balance)

#BOXPLOT OF EVERY VARIABLE
par(mfrow = c(3, 6))
boxplot(data$Age, main = "Age", col = "lightblue")
boxplot(data$Annual_Income, main = "Annual Income", col = "lightgreen")
boxplot(data$Monthly_Inhand_Salary, main = "Monthly Inhand Salary", col = "lightcoral")
boxplot(data$Num_Bank_Accounts, main = "Num Bank Accounts", col = "lightgoldenrodyellow")
boxplot(data$Num_Credit_Card, main = "Num Credit Card", col = "orange")
boxplot(data$Interest_Rate, main = "Interest Rate", col = "violet")
boxplot(data$Num_of_Loan, main = "Num of Loan", col = "pink")
boxplot(data$Delay_from_due_date, main = "Delay from due date", col = "purple")
boxplot(data$Num_of_Delayed_Payment, main = "Num of Delayed Payment", col = "brown")
boxplot(data$Changed_Credit_Limit, main = "Changed Credit Limit", col = "gray")
boxplot(data$Num_Credit_Inquiries, main = "Num Credit Inquires", col = "cyan")
boxplot(data$Outstanding_Debt, main = "Outstanding Debt", col = "magenta")
boxplot(data$Credit_Utilization_Ratio, main = "Credit Utilization Ratio", col = "lavender")
boxplot(data$Credit_History_Age, main = "Credit History Age", col = "turquoise")
boxplot(data$Total_EMI_per_month, main = "Total EMI per month", col = "beige")
boxplot(data$Amount_invested_monthly, main = "Amount invested monthly", col = "salmon")
boxplot(data$Monthly_Balance, main = "Monthly Balance", col = "plum")
dev.off()

#SINCE THERE ARE TO MANY OUTLIERS WE HAVE TO BETTER CLEAN OUR DATASET
data = subset(data, Age>0 & Age<100)
data = subset(data, Annual_Income<250000)
data = subset(data, Monthly_Inhand_Salary<10000)
data = subset(data, Num_Bank_Accounts>=0 & Num_Bank_Accounts<20)
data = subset(data, Num_Credit_Card<20)
data = subset(data, Interest_Rate<40)
data = subset(data, Num_of_Loan>=0 & Num_of_Loan<20)
data = subset(data, Num_of_Delayed_Payment>=0 & Num_of_Delayed_Payment<100)
data = subset(data, Changed_Credit_Limit<20)
data = subset(data, Num_Credit_Inquiries<50)
data = subset(data, Outstanding_Debt<4000)
data = subset(data, Total_EMI_per_month<400)

#CHECK AGAIN THE BOXPLOT OF OUR VARIABLES
par(mfrow = c(3, 6))
boxplot(data$Age, main = "Age", col = "lightblue")
boxplot(data$Annual_Income, main = "Annual Income", col = "lightgreen")
boxplot(data$Monthly_Inhand_Salary, main = "Monthly Inhand Salary", col = "lightcoral")
boxplot(data$Num_Bank_Accounts, main = "Num Bank Accounts", col = "lightgoldenrodyellow")
boxplot(data$Num_Credit_Card, main = "Num Credit Card", col = "orange")
boxplot(data$Interest_Rate, main = "Interest Rate", col = "violet")
boxplot(data$Num_of_Loan, main = "Num of Loan", col = "pink")
boxplot(data$Delay_from_due_date, main = "Delay from due date", col = "purple")
boxplot(data$Num_of_Delayed_Payment, main = "Num of Delayed Payment", col = "brown")
boxplot(data$Changed_Credit_Limit, main = "Changed Credit Limit", col = "gray")
boxplot(data$Num_Credit_Inquiries, main = "Num Credit Inquires", col = "cyan")
boxplot(data$Outstanding_Debt, main = "Outstanding Debt", col = "magenta")
boxplot(data$Credit_Utilization_Ratio, main = "Credit Utilization Ratio", col = "lavender")
boxplot(data$Credit_History_Age, main = "Credit History Age", col = "turquoise")
boxplot(data$Total_EMI_per_month, main = "Total EMI per month", col = "beige")
boxplot(data$Amount_invested_monthly, main = "Amount invested monthly", col = "salmon")
boxplot(data$Monthly_Balance, main = "Monthly Balance", col = "plum")
dev.off()

#CHECK AGAIN CLASS AND SUMMARY
sapply(data, class)
sapply(data, summary)
rownames(data) = NULL  #reset index

#AS SOME DATA WERE MISSING IN OUR SET OF DATA IN THE 
#COLUMNS Monthly_Inhand_Salary and Credit_Mix, WE GET 
#THEM FROM THE NEIGHBOURING ROWS BECAUSE THEY RELATE TO THE SAME PEOPLE.

for (i in 1:nrow(data)) {
  if (is.na(data[i,'Monthly_Inhand_Salary']) == TRUE){
    if (data[i, 'Annual_Income'] == data[i+1, 'Annual_Income'] & is.na(data[i+1,'Monthly_Inhand_Salary']) == FALSE){
      data[i,'Monthly_Inhand_Salary'] = data[i+1,'Monthly_Inhand_Salary']
    } else if (data[i, 'Annual_Income'] == data[i-1, 'Annual_Income'] & is.na(data[i-1,'Monthly_Inhand_Salary']) == FALSE){
      data[i,'Monthly_Inhand_Salary'] = data[i-1,'Monthly_Inhand_Salary']
    }
  }
  if (is.na(data[i,'Credit_Mix']) == TRUE){
    if (data[i, 'Annual_Income'] == data[i+1, 'Annual_Income'] & is.na(data[i+1,'Credit_Mix']) == FALSE){
      data[i,'Credit_Mix'] = data[i+1,'Credit_Mix']
    } else if (data[i, 'Annual_Income'] == data[i-1, 'Annual_Income'] & is.na(data[i-1,'Credit_Mix']) == FALSE){
      data[i,'Credit_Mix'] = data[i-1,'Credit_Mix']
    }
  }
}


#SINCE PEOPLE HAVE MULTIPLE RECORDS DURING THE YEAR, 
#WE DETERMINE THE MOST FREQUENT MONTH IN OUR DATASET 
#AND CONSIDER IT AS THE ONLY ONE.

table(data$Month)  # the most frequent is JANUARY
data = subset(data, Month == 'January')

#DELETE THE LAST ROWS THAT HAVE SOME NaN VALUE
colSums(is.na(data))
data = subset(data, is.na(Credit_History_Age) == FALSE)
data = subset(data, is.na(Credit_Mix) == FALSE)
data = subset(data, is.na(Amount_invested_monthly) == FALSE)
data = subset(data, is.na(Monthly_Balance) == FALSE)

colSums(is.na(data))

#LATEST CHANGES TO MAKE THE DATASET SUITABLE FOR LOGISTIC REGRESSION
rim="Standard"
reg_log=subset(data,!(data$Credit_Score %in% rim))
rownames(reg_log) = NULL
reg_log= reg_log[, -which(names(reg_log) == "Month")]
reg_log= reg_log[, -which(names(reg_log) == "Credit_Mix")]
reg_log= reg_log[, -which(names(reg_log) == "Type_of_Loan")]

reg_log <- reg_log %>%
  mutate(Credit_Score = recode(Credit_Score, "Good" = 1,
                               "Poor" = 0))
#CORRELATION AND HEATMAP
matrice_cor=cor(reg_log)
matrice_cor
corrplot(matrice_cor,method="number", tl.cex=0.7, number.cex = 0.7)
heatmap.2(matrice_cor, 
          col = colorRampPalette(c("blue", "white", "red"))(20),
          key = TRUE, 
          symkey = FALSE,  
          density.info = "none", 
          trace = "none", 
          main = "Heatmap della Correlazione con Numeri di Correlazione")

#LOGISTIC REGRESSION
set.seed(123)
split = sample.split(reg_log$Credit_Score, SplitRatio = 0.8)
train_set = subset(reg_log, split == TRUE)
test_set = subset(reg_log, split == FALSE)

re=glm(Credit_Score~ Age + Annual_Income + Monthly_Inhand_Salary + 
         Num_Bank_Accounts + Num_Credit_Card + Interest_Rate +
         Num_of_Loan + Delay_from_due_date + Num_of_Delayed_Payment +
         Changed_Credit_Limit + Num_Credit_Inquiries + Outstanding_Debt +
         Credit_Utilization_Ratio + Credit_History_Age + 
         Total_EMI_per_month+Amount_invested_monthly+ Monthly_Balance,
       data=train_set,family=binomial)
summary(re)

#repeat again the logistic regression without the non-significant variables
r=glm(Credit_Score~Monthly_Inhand_Salary + Num_Bank_Accounts +
        Num_Credit_Card+Interest_Rate+Delay_from_due_date+Num_of_Delayed_Payment+
        Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
      data=train_set,family=binomial)
summary(r)

#repeat again the logistic regression without the last non-significant variable
reg=glm(Credit_Score~Num_Bank_Accounts +
          Num_Credit_Card+Interest_Rate+Delay_from_due_date+Num_of_Delayed_Payment+
          Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
        data=train_set,family=binomial)
summary(reg)

#AIC and BIC
AIC(reg)
BIC(reg)

#PREDICTION WITH THRESHOLD = 0.5
reg.probs <- predict(reg,test_set, type = "response")
reg.probs
is.na(reg.probs)==FALSE

reg.pred <- rep("0", 451)
reg.pred
reg.pred[reg.probs > .5] <- "1"
reg.pred
reg.pred[1:10]
round(reg.probs[1:10],3)

#CONFUSION MATRIX
Credit_Score_test=test_set$Credit_Score
table(reg.pred, Credit_Score_test)

fn=22
fp=42
tp=130
tn=257
n=451

test_error_rate = ((fn+fp)/n)
accurancy = ((tn+tp)/n)
specificity = (tn/(tn+fp))
sensitivity = (tp/(fn+tp))
error_negative = (fp/(tp+fp))
error_positive = (fn/(fn+tp))

cat('test_error_rate =', test_error_rate, '\n',
    'accurancy =', accurancy, '\n',
    'specificity =', specificity, '\n',
    'sensitivity =', sensitivity, '\n',
    'error_negative =', error_negative, '\n',
    'error_positive =', error_positive, '\n')

#AREA UNDER ROC CURVE
reg.pred = as.numeric(reg.pred)
roc_curve <- roc(response = test_set$Credit_Score, predictor = reg.pred)
auc(roc_curve)

#FOR THE BANK IS VERY IMPORTANT TO HAVE AN HIGHTER SPECIFITY IN ORDER
#TO AVOID TO CLASSIFY A BAD CREDIT SCORE AS A GOOD ONE.
#TO DO THIS WE HAVE TO INCREMENT THE THRESHOLD.

#PREDICTION WITH THRESHOLD = 0.8
reg.pred <- rep("0", 451)
reg.pred
reg.pred[reg.probs > .8] <- "1"
reg.pred
reg.pred[1:10]
round(reg.probs[1:10],3)

#CONFUSION MATRIX
Credit_Score_test=test_set$Credit_Score
table(reg.pred, Credit_Score_test)

fn=68
fp=14
tp=84
tn=285
n=451

test_error_rate = ((fn+fp)/n)
accurancy = ((tn+tp)/n)
specificity = (tn/(tn+fp))
sensitivity = (tp/(fn+tp))
error_negative = (fp/(tp+fp))
error_positive = (fn/(fn+tp))

cat('test_error_rate =', test_error_rate, '\n',
    'accurancy =', accurancy, '\n',
    'specificity =', specificity, '\n',
    'sensitivity =', sensitivity, '\n',
    'error_negative =', error_negative, '\n',
    'error_positive =', error_positive, '\n')

#AREA UNDER ROC CURVE
reg.pred = as.numeric(reg.pred)
roc_curve <- roc(response = test_set$Credit_Score, predictor = reg.pred)
auc(roc_curve)

#OF COURSE CHANGING THE THRESHOLD IN ORDER TO INCREASE THE SPECIFICITY
#LEADS TO AN INCREMENT OF THE TEST ERROR RATE AND A REDUCTION IN
#SENSITIVITY AND AREA UNDER ROC CURVE

#HIERARCHICAL CLUSTERING
kl=subset(data)
kl = kl[, -which(names(kl) == "Month")]
kl = kl[, -which(names(kl) == "Credit_Mix")]
kl = kl[, -which(names(kl) == "Type_of_Loan")]
rownames(kl) = NULL

kl <- kl %>%
  mutate(Credit_Score = recode(Credit_Score, "Good" = 2,
                               "Standard"=1,
                               "Poor" = 0))

hc.complete = hclust(dist(kl[,-c(18)]), method="complete") 

cut_avg <- cutree(hc.complete, k = 3)
table(cut_avg)

avg_dend_obj <- as.dendrogram(hc.complete)
avg_col_dend <- color_branches(avg_dend_obj, k = 3)
plot(avg_col_dend)
abline(h = 60000, col = "red", lty = 2)

