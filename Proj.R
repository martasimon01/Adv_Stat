library('dplyr')
train = read.csv('train.csv')

# Drop two unnecessary columns
train <- select(train, -c('ID'))

# Identify numeric and non numeric features

numeric_feat <- names(train)[sapply(train, is.numeric)]
non_numeric_feat <- names(train)[!sapply(train, is.numeric)]
non_numeric_feat_toNumeric <- names(train[, c('Age', 'Annual_Income', 
                                              'Num_of_Loan', 'Num_of_Delayed_Payment',
                                              'Changed_Credit_Limit', 'Outstanding_Debt', 
                                              'Amount_invested_monthly', 'Monthly_Balance')])
non_numeric_feat_toNumeric

# Transform non_numeric_feat_toNumeric to numeric features

columnas <- c('Age', 'Annual_Income','Changed_Credit_Limit', 'Outstanding_Debt', 'Amount_invested_monthly', 'Monthly_Balance')

train[columnas] <- lapply(train[columnas], as.numeric)

categoric_feat <- names(train[, c('Credit_Mix', 'Payment_of_Min_Amount', 'Payment_Behaviour', 'Credit_Score')])
categoric_feat

colSums(is.na(train)) / nrow(train) * 100

#SELEZIONO SOLTANTO LE COLONNE CHE CI SERVONO
train = train %>%
  select(Month, Age, Occupation:Credit_Score)

#ELIMINO I TRATTINI BASSI DALLA COLONNA Age
train$Age = gsub('_', '', train$Age)
train$Age = as.numeric(train$Age) 
summary(train$Age)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Age ANOMALO
train = subset(train, Age>0 & Age<100)
summary(train$Age)

#CORREGGO LA COLONNA Occupation
train$Occupation = gsub('_______', NA, train$Occupation)

#ELIMINO I TRATTINI BASSI DALLA COLONNA Annual_Income
train$Annual_Income = gsub('_', '', train$Annual_Income)
train$Annual_Income = as.numeric(train$Annual_Income) 

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Annual_Income ANOMALO
train = subset(train, Annual_Income<1000000)
summary(train$Annual_Income)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_Bank_Accounts ANOMALO
train = subset(train, Num_Bank_Accounts>=0, Num_Bank_Accounts<20)
summary(train$Num_Bank_Accounts)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_Credit_Card ANOMALO
train = subset(train, Num_Credit_Card<20)
summary(train$Num_Credit_Card)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Interest_Rate ANOMALO
train = subset(train, Interest_Rate<40)
summary(train$Interest_Rate)

#ELIMINO I TRATTINI BASSI DALLA COLONNA Num_of_Loan
train$Num_of_Loan = gsub('_', '', train$Num_of_Loan)
train$Num_of_Loan = as.numeric(train$Num_of_Loan) 
summary(train$Num_of_Loan)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_of_Loan ANOMALO
train = subset(train, Num_of_Loan>=0 & Num_of_Loan<20)
summary(train$Num_of_Loan)

#ELIMINO I TRATTINI BASSI DALLA COLONNA Num_of_Delayed_Payment
train$Num_of_Delayed_Payment = gsub('_', '', train$Num_of_Delayed_Payment)
train$Num_of_Delayed_Payment = as.numeric(train$Num_of_Delayed_Payment) 
summary(train$Num_of_Delayed_Payment)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_of_Delayed_Payment ANOMALO
train = subset(train, Num_of_Delayed_Payment>=0 & Num_of_Delayed_Payment<100)
summary(train$Num_of_Delayed_Payment)

#TRASFORMO I TRATTINI BASSI DALLA COLONNA Changed_Credit_Limit IN ZERI
train$Changed_Credit_Limit = gsub('_', 0, train$Changed_Credit_Limit)
train$Changed_Credit_Limit = as.numeric(train$Changed_Credit_Limit) 
summary(train$Changed_Credit_Limit)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_Credit_Inquiries ANOMALO
train = subset(train, Num_Credit_Inquiries<50)
summary(train$Num_Credit_Inquiries)

#CORREGGO LA COLONNA Credit_Mix
train$Credit_Mix = gsub('_', NA, train$Credit_Mix)

#ELIMINO I TRATTINI BASSI DALLA COLONNA Outstanding_Debt
train$Outstanding_Debt = gsub('_', '', train$Outstanding_Debt)
train$Outstanding_Debt = as.numeric(train$Outstanding_Debt) 
summary(train$Outstanding_Debt)

#MODIFICA LA COLONNA Credit_History_Age
library(stringr)
anni_e_mesi <- str_match(train$Credit_History_Ag, "(\\d+) Years and (\\d+) Months")
anni <- as.numeric(anni_e_mesi[, 2])  
mesi <- as.numeric(anni_e_mesi[, 3])
Age_in_years <- anni + (mesi / 12)
train$Credit_History_Age = Age_in_years
summary(train$Credit_History_Age)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Total_EMI_per_month ANOMALO
train = subset(train, Total_EMI_per_month<500)
summary(train$Total_EMI_per_month)

#CORREGGO LA COLONNA Amount_invested_monthly
train$Amount_invested_monthly = gsub('__10000__', NA, train$Amount_invested_monthly)
train$Amount_invested_monthly = as.numeric(train$Amount_invested_monthly) 
summary(train$Amount_invested_monthly)

#CORREGGO LA COLONNA Payment_Behaviour
train$Payment_Behaviour = gsub('!@9#%8', NA, train$Payment_Behaviour)

#CORREGGO LA COLONNA Monthly_Balance
train$Monthly_Balance = as.numeric(train$Monthly_Balance)
summary(train$Monthly_Balance)

#RICONTROLLO LE CLASSI ED IL SUMMARY DI OGNI COLONNA
sapply(train, class)
sapply(train, summary)

########################################

#ELIMINO COLONNE INDESIDERATE
train$Occupation = NULL 
train$Payment_of_Min_Amount = NULL 


#We are going to transform various categorical features into numerical ones.

#But first we have to adjust certain irregular values that some features have, like:
  
#  "_"

#"NM"

#"!@9#%8"

#Encoding Payment_Behaviour
train <- train[!train$Payment_Behaviour %in% "!@9#%8", ]

nrow(train[train$Payment_Behaviour == "!@9#%8", ])

#"Low_spent_Small_value_payments" = 1,
#"Low_spent_Medium_value_payments" = 2,
#"Low_spent_Large_value_payments" = 3,
#"High_spent_Small_value_payments" = 4,
#"High_spent_Medium_value_payments" = 5,
#"High_spent_Large_value_payments" = 6

# Transforming into numbers its categories

train <- train %>%
  mutate(Payment_Behaviour = recode(Payment_Behaviour, "Low_spent_Small_value_payments" = 1, 
                                    "Low_spent_Medium_value_payments" = 2, 
                                    "Low_spent_Large_value_payments" = 3, 
                                    "High_spent_Small_value_payments" = 4, 
                                    "High_spent_Medium_value_payments" = 5, 
                                    "High_spent_Large_value_payments" = 6))

#Encoding Credit_Score
#The encoding will be:
# 'Good' = 1
#'Standard' = 2
#'Poor' = 3
train <- train %>%
  mutate(Credit_Score = recode(Credit_Score, "Good" = 1, 
                               "Standard" = 2,
                               "Poor" = 0))

#RIORDINO GLI INDICI DA 1 A N
rownames(train) = NULL

#DAL MOMENTO IN CUI NEL NOSTRO DATASET MANCAVANO ALCUNI DATI NELLE
#COLONNE Monthly_Inhand_Salary e Credit_Mix, LI ABBIAMO REPERITI DALLE RIGHE 
#SOVRASTANTI O SOTTOSTANTI IN QUANTO ERANO RIFERITI ALLE STESSE PERSONE

for (i in 1:nrow(train)) {
  if (is.na(train[i,'Monthly_Inhand_Salary']) == TRUE){
    if (train[i, 'Annual_Income'] == train[i+1, 'Annual_Income'] & is.na(train[i+1,'Monthly_Inhand_Salary']) == FALSE){
      train[i,'Monthly_Inhand_Salary'] = train[i+1,'Monthly_Inhand_Salary']
    } else if (train[i, 'Annual_Income'] == train[i-1, 'Annual_Income'] & is.na(train[i-1,'Monthly_Inhand_Salary']) == FALSE){
      train[i,'Monthly_Inhand_Salary'] = train[i-1,'Monthly_Inhand_Salary']
    }
  }
  if (is.na(train[i,'Credit_Mix']) == TRUE){
    if (train[i, 'Annual_Income'] == train[i+1, 'Annual_Income'] & is.na(train[i+1,'Credit_Mix']) == FALSE){
      train[i,'Credit_Mix'] = train[i+1,'Credit_Mix']
    } else if (train[i, 'Annual_Income'] == train[i-1, 'Annual_Income'] & is.na(train[i-1,'Credit_Mix']) == FALSE){
      train[i,'Credit_Mix'] = train[i-1,'Credit_Mix']
    }
  }
}

#DATO CHE PER OGNI PERSONA SONO PRESENTI PIU' RILEVAZIONI DURANTE L'ANNO,
#CONTROLLIAMO QUAL E' IL MESE PIU' FREQUENTE NEL NOSTRO DATASET IN MODO
#DA PRENDERE IN ANALISI SOLO QUEST ULTIMO
table(train$Month)  #---> JANUARY

train = subset(train, Month == 'January')

#CONTROLLO QUALI RIGHE PRESENTANO VALORI NA E LE ELIMINO
colSums(is.na(train))
train = subset(train, is.na(Credit_History_Age) == FALSE)
train = subset(train, is.na(Monthly_Inhand_Salary) == FALSE)
train = subset(train, is.na(Credit_Mix) == FALSE)
train = subset(train, is.na(Amount_invested_monthly) == FALSE)
train = subset(train, is.na(Monthly_Balance) == FALSE)
train = subset(train, is.na(Payment_Behaviour) == FALSE)
train = subset(train, is.na(Changed_Credit_Limit) == FALSE)
train = subset(train, is.na(Outstanding_Debt) == FALSE)

colSums(is.na(train))
#RIORDINO GLI INDICI DA 1 A N
rownames(train) = NULL

############################
#ELIMINO GLI STANDARD DALLA COLONNA Credit_Score PER POI SVOLGERE LA REGRESSIONE LOGISTICA
rim="Standard"
train=subset(train,!(train$Credit_Score %in% rim))

#RIORDINO GLI INDICI DA 1 A N
rownames(train) = NULL


#ELIMINO GLI STANDARD DALLA COLONNA Credit_Score PER POI SVOLGERE LA REGRESSIONE LOGISTICA
rim="2"
train=subset(train,!(train$Credit_Score %in% rim))
names(train)

# Drop unnecessary columns
train <- select(train, -c('Month',"Type_of_Loan","Credit_Mix"))

names(train)

# Correlation matrix 

corr <- cor(train[, sapply(train, is.numeric)], use = "complete.obs", method = "spearman")

corr["Credit_Score",]

# Creazione dei boxplot per tutte le variabili
par(mfrow = c(3, 6))
boxplot(train$Age, main = "Age", col = "lightblue")
boxplot(train$Annual_Income, main = "Annual Income", col = "lightgreen")
boxplot(train$Monthly_Inhand_Salary, main = "Monthly Inhand Salary", col = "lightcoral")
boxplot(train$Num_Bank_Accounts, main = "Num Bank Accounts", col = "lightgoldenrodyellow")
boxplot(train$Num_Credit_Card, main = "Num Credit Card", col = "orange")
boxplot(train$Interest_Rate, main = "Interest Rate", col = "violet")
boxplot(train$Num_of_Loan, main = "Num of Loan", col = "pink")
boxplot(train$Delay_from_due_date, main = "Delay from due date", col = "purple")
boxplot(train$Num_of_Delayed_Payment, main = "Num of Delayed Payment", col = "brown")
boxplot(train$Changed_Credit_Limit, main = "Changed Credit Limit", col = "gray")
boxplot(train$Num_Credit_Inquiries, main = "Num Credit Inquires", col = "cyan")
boxplot(train$Outstanding_Debt, main = "Outstanding Debt", col = "magenta")
boxplot(train$Credit_Utilization_Ratio, main = "Credit Utilization Ratio", col = "lavender")
boxplot(train$Credit_History_Age, main = "Credit History Age", col = "turquoise")
boxplot(train$Total_EMI_per_month, main = "Total EMI per month", col = "beige")
boxplot(train$Amount_invested_monthly, main = "Amount invested monthly", col = "salmon")
boxplot(train$Monthly_Balance, main = "Monthly Balance", col = "plum")
boxplot(train$Payment_Behaviour, main = "Payment Behaviour", col = "plum")
dev.off()


###########INIZIO REGRESSIONE LOGISITCA##########################
#MATRICE DI CORRELAZIONE
library(corrplot)
matrice_cor=cor(train)
matrice_cor
corrplot(matrice_cor,method="number")
heatmap.2(matrice_cor, 
          col = colorRampPalette(c("blue", "white", "red"))(20),
          key = TRUE,   # Aggiunge la chiave di colori
          symkey = FALSE,  # Usa la scala di colori simmetrica
          density.info = "none",  # Non mostrare la densità
          trace = "none",  # Non mostrare le tracce
          main = "Heatmap della Correlazione con Numeri di Correlazione")

##DIVIDO IN TEST E TRAIN SET##
library(caTools)
set.seed(123)
split = sample.split(train$Credit_Score, SplitRatio = 0.8)
train_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)

nrow(train_set)
nrow(test_set)

names(train)


#####INIZIO REGRESSIONE LOG####
#install.packages("nnet")
library(nnet)

#re=multinom(Credit_Score~ Age +Annual_Income+Monthly_Inhand_Salary+Num_Bank_Accounts+Num_Credit_Card+Interest_Rate+Num_of_Loan+Delay_from_due_date+Num_of_Delayed_Payment+Changed_Credit_Limit+Num_Credit_Inquiries+Outstanding_Debt+Credit_Utilization_Ratio+Credit_History_Age+Total_EMI_per_month+Amount_invested_monthly+Payment_Behaviour+Monthly_Balance,
#            data=train_set)
re=glm(Credit_Score~ Age +Annual_Income+Monthly_Inhand_Salary+Num_Bank_Accounts+Num_Credit_Card+Interest_Rate+Num_of_Loan+Delay_from_due_date+Num_of_Delayed_Payment+Changed_Credit_Limit+Num_Credit_Inquiries+Outstanding_Debt+Credit_Utilization_Ratio+Credit_History_Age+Total_EMI_per_month+Amount_invested_monthly+Payment_Behaviour+Monthly_Balance,
        data=train_set,family=)

#re$Credit_Score <- relevel(as.factor(re$Credit_Score), ref = 1)

summary(re)

#RIFACCIO IL MODELLO ELIMINANDO LE VARIABILI NON SIGNIFICATIVE
r=glm(Credit_Score~Age+Num_Credit_Card+Interest_Rate+Delay_from_due_date+Changed_Credit_Limit+Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
       data=train_set,family=binomial)

summary(r)
#RIFACCIO IL MODELLO ELIMINANDO LE VARIABILI NON SIGNIFICATIVE
reg=glm(Credit_Score~Age+Num_Credit_Card+Interest_Rate+Delay_from_due_date+Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
        data=train_set,family=binomial)

summary(reg)

#FACCIO GRAFICI PER CAPIRE DISTRIBUZIONE REG
par(mfrow=c(2,2))
plot(reg)
dev.off()

# USIAMO predict() PER PREDIRE LA PROBABILITA' CHE IL CREDIT SCORE SARA' GOOD.
#PRINTIAMO SOLO 10 DELLE PROBABILITA'
reg.probs <- predict(reg,test_set, type = "response")
reg.probs
is.na(reg.probs)==FALSE

#CREIAMO UN VETTORE DI CLASS PREDICTIONS BASATO SE LA PROBABILITA' PREDETTA SARA' MAGGIORE O 
#MINORE DI 0.5. if prob(Good) > 0.5 --> "Good"
reg.pred <- rep("Poor", 508)
reg.pred
reg.pred[reg.probs > .5] <- "Good"
reg.pred

reg.pred[1:10]
round(reg.probs[1:10],3)

#USIAMO TABLE() PER CREARE UNA CONFUSION MATRIX PER DETERMINARE QUANTE OSSERVAZIONI
#SONO STATE CLASSIFICATE GIUSTE
Credit_Score_test=test_set$Credit_Score
table(reg.pred, Credit_Score_test)

#TEST ERROR RATE
e=((47+13)/508)
e

#ACCURACY
a=((289+159)/508)
a

#SPECIFICITY (TN/TN+FP)
sp=(289/(289+47))
sp

#SENSITIVITY (TP/FN+TP)
se=(159/(159+13))
se

#NON CAPISCO
#The mean() function can be used to compute how many credit score prediction was correct. 
mean(reg.pred == test_set$Credit_Score)

typeof(reg.pred)

# Test error rate
mean(reg.pred != Credit_Score_test)

