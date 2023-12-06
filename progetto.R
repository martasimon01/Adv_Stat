library('dplyr')
library('gplots')
train = read.csv('train.csv')

######TRAIN SET#####
#STAMPO LE CLASSI ED IL SUMMARY DI OGNI COLONNA
sapply(train, class)
sapply(train, summary)

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
train = subset(train, Annual_Income<250000)
summary(train$Annual_Income)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Monthly_Inhand_Salary ANOMALO
train = subset(train, Monthly_Inhand_Salary<10000)
summary(train$Monthly_Inhand_Salary)

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Num_Bank_Accounts ANOMALO
train = subset(train, Num_Bank_Accounts>=0 & Num_Bank_Accounts<20)
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

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Changed_Credit_Limit ANOMALO
train = subset(train, Changed_Credit_Limit<20)
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

#SCARTO TUTTE LE RIGHE CHE HANNO UN VALORE Outstanding_Debt ANOMALO
train = subset(train, Outstanding_Debt<4000)
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
train = subset(train, Total_EMI_per_month<400)
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
train$Payment_Behaviour = NULL 
train$Payment_of_Min_Amount = NULL 

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

colSums(is.na(train))
#RIORDINO GLI INDICI DA 1 A N
rownames(train) = NULL

############################
#ELIMINO GLI STANDARD DALLA COLONNA Credit_Score PER POI SVOLGERE LA REGRESSIONE LOGISTICA
rim="Standard"
reg_log=subset(train,!(train$Credit_Score %in% rim))

#RIORDINO GLI INDICI DA 1 A N
rownames(reg_log) = NULL

#ELIMINO MONTH,CREDIT_MIX E TYPE_OF_LOAN COME VARIABILI
reg_log= reg_log[, -which(names(reg_log) == "Month")]
reg_log= reg_log[, -which(names(reg_log) == "Credit_Mix")]
reg_log= reg_log[, -which(names(reg_log) == "Type_of_Loan")]
 
#TRASFORMO LA COLONNA CREDIT_SCORE IN UNA VARIABILE BINARIA
library(dplyr)

reg_log <- reg_log %>%
  mutate(Credit_Score = recode(Credit_Score, "Good" = 1,
                               "Poor" = 0))


# Creazione dei boxplot per tutte le variabili
par(mfrow = c(3, 6))
boxplot(reg_log$Age, main = "Age", col = "lightblue")
boxplot(reg_log$Annual_Income, main = "Annual Income", col = "lightgreen")
boxplot(reg_log$Monthly_Inhand_Salary, main = "Monthly Inhand Salary", col = "lightcoral")
boxplot(reg_log$Num_Bank_Accounts, main = "Num Bank Accounts", col = "lightgoldenrodyellow")
boxplot(reg_log$Num_Credit_Card, main = "Num Credit Card", col = "orange")
boxplot(reg_log$Interest_Rate, main = "Interest Rate", col = "violet")
boxplot(reg_log$Num_of_Loan, main = "Num of Loan", col = "pink")
boxplot(reg_log$Delay_from_due_date, main = "Delay from due date", col = "purple")
boxplot(reg_log$Num_of_Delayed_Payment, main = "Num of Delayed Payment", col = "brown")
boxplot(reg_log$Changed_Credit_Limit, main = "Changed Credit Limit", col = "gray")
boxplot(reg_log$Num_Credit_Inquiries, main = "Num Credit Inquires", col = "cyan")
boxplot(reg_log$Outstanding_Debt, main = "Outstanding Debt", col = "magenta")
boxplot(reg_log$Credit_Utilization_Ratio, main = "Credit Utilization Ratio", col = "lavender")
boxplot(reg_log$Credit_History_Age, main = "Credit History Age", col = "turquoise")
boxplot(reg_log$Total_EMI_per_month, main = "Total EMI per month", col = "beige")
boxplot(reg_log$Amount_invested_monthly, main = "Amount invested monthly", col = "salmon")
boxplot(reg_log$Monthly_Balance, main = "Monthly Balance", col = "plum")
dev.off()

###########INIZIO REGRESSIONE LOGISITCA##########################
#RIVEDERE LEZIONE DI R GIOVEDI 9 
#MATRICE DI CORRELAZIONE
library(corrplot)
matrice_cor=cor(reg_log)
matrice_cor
corrplot(matrice_cor,method="number")
heatmap.2(matrice_cor, 
          col = colorRampPalette(c("blue", "white", "red"))(20),
          key = TRUE,   # Aggiunge la chiave di colori
          symkey = FALSE,  # Usa la scala di colori simmetrica
          density.info = "none",  # Non mostrare la densità
          trace = "none",  # Non mostrare le tracce
          main = "Heatmap della Correlazione con Numeri di Correlazione")

#DIVIDO IN TEST E TRAIN SET
library(caTools)
set.seed(123)
split = sample.split(reg_log$Credit_Score, SplitRatio = 0.8)
train_set = subset(reg_log, split == TRUE)
test_set = subset(reg_log, split == FALSE)

summary(train_set)

#ESEGUO REGRESSIONE LOGISTICA E NOTO CHE ALCUNE VARIABILI NON SONO SIGNIFICATIVE
re=glm(Credit_Score~ Age + Annual_Income + Monthly_Inhand_Salary + Num_Bank_Accounts+ Num_Credit_Card+Interest_Rate+Num_of_Loan+Delay_from_due_date+Num_of_Delayed_Payment+Changed_Credit_Limit+Num_Credit_Inquiries+Outstanding_Debt+Credit_Utilization_Ratio+Credit_History_Age+Total_EMI_per_month+Amount_invested_monthly+ Monthly_Balance,
        data=train_set,family=binomial)
summary(re)

#TOLGO LE SEGUENTI VARIBAILI PERCHè NON SONO SIGN Age,Annual_Income,Num_of_Loan,Changed_Credit_Limit,Credit_Utilization_Ratio,Total_EMI_per_month,Amount_invested_monthly,Monthly_Balance  
#RIFACCIO IL MODELLO ELIMINANDO LE VARIABILI NON SIGNIFICATIVE
r=glm(Credit_Score~Monthly_Inhand_Salary + Num_Bank_Accounts +
        Num_Credit_Card+Interest_Rate+Delay_from_due_date+Num_of_Delayed_Payment+
        Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
        data=train_set,family=binomial)
summary(r)

#TOLGO LE SEGUENTI VARIABILI NON SIGN Monthly_Inhand_Salary
#RIFACCIO IL MODELLO ELIMINANDO LE VARIABILI NON SIGNIFICATIVE
reg=glm(Credit_Score~Num_Bank_Accounts +
        Num_Credit_Card+Interest_Rate+Delay_from_due_date+Num_of_Delayed_Payment+
        Num_Credit_Inquiries+Outstanding_Debt+Credit_History_Age,
      data=train_set,family=binomial)
summary(reg)

#AIC e BIC
AIC(reg)
BIC(reg)

#FACCIO GRAFICI PER CAPIRE DISTRIBUZIONE REG --------- Studiare
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
reg.pred <- rep("0", 451)
reg.pred
reg.pred[reg.probs > .5] <- "1"
reg.pred

reg.pred[1:10]
round(reg.probs[1:10],3)

#USIAMO TABLE() PER CREARE UNA CONFUSION MATRIX PER DETERMINARE QUANTE OSSERVAZIONI
#SONO STATE CLASSIFICATE GIUSTE
Credit_Score_test=test_set$Credit_Score
table(reg.pred, Credit_Score_test)

#CREO VARIBILI FN,FP,TP,TN,n
fn=22
fp=42
tp=130
tn=257
n=451

#TEST ERROR RATE (FN+FP/n)
e=((fn+fp)/n)
e
#0.1419069

#ACCURACY (TN+TP/n)
a=((tn+tp)/n)
a
#0.8580931

#SPECIFICITY (TN/TN+FP)
sp=(tn/(tn+fp))
sp
#0.8595318

#SENSITIVITY (TP/FN+TP)
se=(tp/(fn+tp))
se
#0.8552632

#ERRORE CLASSI NEGATIVE FP/N
en=(fp/(tp+fp))
en
#0.244186

#ERRORE CLASSI POSITIVO FN/P
ep=(fn/(fn+tp))
ep
#0.1447368

# AREA UNDER ROC CURVE
library(pROC)
reg.pred = as.numeric(reg.pred)
roc_curve <- roc(response = test_set$Credit_Score, predictor = reg.pred)
auc(roc_curve)

#PER LA BANCA è PIU IMPORTANTE AVERE SPECIFICITY PIU ALTA QUINDI AUMENTIAMO THRESHOLD
reg.pred <- rep("0", 451)
reg.pred
reg.pred[reg.probs > .80] <- "1"
reg.pred

reg.pred[1:10]
round(reg.probs[1:10],3)

#USIAMO TABLE() PER CREARE UNA CONFUSION MATRIX PER DETERMINARE QUANTE OSSERVAZIONI
#SONO STATE CLASSIFICATE GIUSTE
Credit_Score_test=test_set$Credit_Score
table(reg.pred, Credit_Score_test)

#CREO VARIBILI FN,FP,TP,TN,n
fn=68
fp=14
tp=84
tn=285
n=451

#TEST ERROR RATE (FN+FP/n)
e=((fn+fp)/n)
e
#0.1818182

#ACCURACY (TN+TP/n)
a=((tn+tp)/n)
a
#0.8181818

#SPECIFICITY (TN/TN+FP)
sp=(tn/(tn+fp))
sp
#0.9531773

#SENSITIVITY (TP/FN+TP)
se=(tp/(fn+tp))
se
#0.5526316

#ERRORE CLASSI NEGATIVE FP/N
en=(fp/(tp+fp))
en
#0.1428571

#ERRORE CLASSI POSITIVO FN/P
ep=(fn/(fn+tp))
ep
#0.4473684

#AREA UNDER ROC CURVE
library(pROC)
reg.pred = as.numeric(reg.pred)
roc_curve <- roc(response = test_set$Credit_Score, predictor = reg.pred)
auc(roc_curve)


"""
L Area sotto la curva ROC (AUC-ROC) è una misura di quanto bene 
un modello di classificazione possa distinguere tra le classi positive 
e negative. L'AUC-ROC varia da 0 a 1, dove un valore più alto indica 
una migliore capacità discriminante del modello. 
Ecco come interpretare il tuo risultato di 0.7529:

AUC-ROC = 0.5: Indica una capacità discriminante casuale, come quella di un classificatore che fa previsioni casuali.
0.5 < AUC-ROC < 0.7: Indica una scarsa capacità discriminante. Il modello ha difficoltà a distinguere tra le classi positive e negative.
0.7 < AUC-ROC < 0.8: Indica una capacità discriminante ragionevole. Il modello ha una discreta capacità di distinguere tra le classi, ma ci sono margini di miglioramento.
0.8 < AUC-ROC < 0.9: Indica una buona capacità discriminante. Il modello è efficace nel distinguere tra le classi positive e negative.
AUC-ROC > 0.9: Indica un'eccellente capacità discriminante. Il modello è molto efficace nel distinguere tra le classi positive e negative.
Nel tuo caso, con un AUC-ROC di 0.7529, il modello mostra una capacità discriminante ragionevole. Tuttavia, potrebbe esserci spazio per miglioramenti. Considera anche di esaminare la curva ROC per ottenere ulteriori informazioni sulla trade-off tra sensibilità e specificità a diversi punti di soglia.
"""


#####CLUSTER####
###### K-Means Clustering ----
##DEVO FARE SUBSET CON STANDARD
kl=subset(train)

#RIORDINO GLI INDICI DA 1 A N
rownames(kl) = NULL

#ELIMINO MONTH,CREDIT_MIX E TYPE_OF_LOAN COME VARIABILI
kl= kl[, -which(names(kl) == "Month")]
kl=kl[, -which(names(kl) == "Credit_Mix")]
kl= kl[, -which(names(kl) == "Type_of_Loan")]

#TRASFORMO LA COLONNA CREDIT_SCORE IN UNA VARIABILE BINARIA
library(dplyr)
#reg_log$Credit_Score=ifelse(reg_log$Credit_Score=="Good",1,0)
kl <- kl %>%
  mutate(Credit_Score = recode(Credit_Score, "Good" = 2,
                               "Standard"=1,
                               "Poor" = 0))


library(readr)
set.seed(123)


# we choose a number of clusters equal to 3 for K-Means 
kl.out <- kmeans(kl[,-c(18)], 3, nstart = 40, iter.max = 40)

#Memberships
kl.out$cluster[1:40]
clusters = as.data.frame(kl.out$cluster)

#centroids
kl.out$centers[,1:17]

library(ggplot2)
library(ggfortify)

#NORMALE
autoplot(stats::kmeans(kl[, -c(18)], 3, nstart = 40, iter.max = 40), data = kl)

#STANDARDIZZATO
kl_standardized <- scale(kl)
kl_standardized = as.data.frame(kl_standardized)
autoplot(stats::kmeans(kl_standardized[,-c(18)], 3, nstart = 40, iter.max = 40), data = kl)


#### Hierarchical Clustering ----

hc.complete<-hclust(dist(kl[,-c(18)]), method="complete") 
# largest distance between the data points in the clusters

# or "single" or "average"

cut_avg <- cutree(hc.complete, k = 3)
table(cut_avg)

#install.packages('dendextend')
library(dendextend)
avg_dend_obj <- as.dendrogram(hc.complete)
avg_col_dend <- color_branches(avg_dend_obj, k = 3)
plot(avg_col_dend)

d <- table(kl$Credit_Score,cut_avg)
kable(d)


#Remark: scaling variables yields different results!!!
xsc <- scale(kl[,-c(18)])
hc.complete1<-hclust(dist(xsc), method="complete") 

cut_avg1 <- cutree(hc.complete1, k = 3)

avg_dend_obj1 <- as.dendrogram(hc.complete1)
avg_col_dend1 <- color_branches(avg_dend_obj1, k = 3)

plot(avg_col_dend1, main = "Hierarchical Clustering with Scaled Features")

# Correlation-based distance can be computed using the as.dist()
# which converts an arbitrary square symmetric matrix into a form that
# the hclust() function recognizes as a distance matrix
dd <- as.dist(1 - cor(t(xsc)))

hc.complete2<-hclust(dd, method="complete")
cut_avg2 <- cutree(hc.complete2, k = 3)

avg_dend_obj2 <- as.dendrogram(hc.complete2)
avg_col_dend2 <- color_branches(avg_dend_obj2, k = 3)

plot(avg_col_dend2, main = "Complete Linkage with Correlation-Based Distance")
