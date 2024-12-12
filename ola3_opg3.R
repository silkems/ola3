
# Load nødvendige biblioteker
library(dplyr)
library(caret)

#OPG 3 - ola 3
#### opg 3.1 ####
#Lav en Machine Learning model, der med afsæt i DST’s forbrugertillidsindikator, kan forudsige om
#julehandlen i 2024 er større end i 2023.

####Q4 værdi####
#kender ik indikator for dec - udregner selv 
# Fjern eventuelle mellemrum i tidskolonnen, hvis der skulle være nogle
FORV1$TID <- trimws(FORV1$TID)

# Filtrer rækker, hvor TID-kolonnen ender på "Q4", og behold både TID og Forbrugertillidsindikatoren kolonnerne
q4_data <- as.data.frame(FORV1.samlet[grep("Q4$", FORV1.samlet$TID), c("TID", "Forbrugertillidsindikatoren")],na.rm=T)
nrow(q4_data)  # Tjek antallet af rækker - 152

# Filtrer rækker i q4_data, hvor TID er fra og med "2000Q4"
q4_data.p <- q4_data[q4_data$TID >= "2000Q4", ]

# Tjek resultaterne
print(head(q4_data.p)) #2000Q4 er start
print(tail(q4_data.p)) #2024Q4 er slut

q4_data.p <- na.omit(q4_data.p)

#procentmæssig ændring
Q4.procent.p <- q4_data.p %>%
  group_by(TID) %>%
  summarise(
    nov_dec_pct_change = (Forbrugertillidsindikatoren[3] - Forbrugertillidsindikatoren[2]) / Forbrugertillidsindikatoren[2] * 100
  )

Q4.procent.p$nov_dec_pct_change[is.infinite(Q4.procent.p$nov_dec_pct_change)] <- NA #erstatter 0 værdi med NA

# Beregn gennemsnitslig ændring fra nov-dec 
gennemsnit_dec <- mean(Q4.procent.p$nov_dec_pct_change, na.rm = TRUE) #-8.038866

#Indikator værdi for december 2024
november_værdi <- -9.3 #FTI for november 2024

ændret_dec <- november_værdi * (1 + gennemsnit_dec / 100)
#December værdi -8.552385

Q4.indikator <- data.frame(Indikator = (-8.9 + -9.3 + -8.5)/3)
#-8.9 er samlet indikatortal for Q4

#Q4: -8.4 indikatortal for Q4
Q4.indikator <- data.frame(Indikator = -8.9)

#udregner sandsynligheden for stigning
NKHC021$dummy <- ifelse(NKHC021$Realvækst >= 0, print(1), print(0))
table(NKHC021$dummy)

#realvækst og fti i dataframe
julehandel <- as.data.frame(cbind(Dummy = NKHC021$dummy, FTI = FORV1$Forbrugertillidsindikatoren[1:99]))
#julehandel <- rbind(julehandel, c(1, -8.9)) #tilføjer Q4 værdier GØR IKKE

colnames(julehandel) <- c("Dummy","Indikator")
julehandel$Dummy <- as.factor(julehandel$Dummy)

# Logistisk regression 
Q4.log <- glm(Dummy ~ Indikator, data = julehandel, family = "binomial")

#Forudsigelse af sandsynlighed for en stigning
Julehandel.predict <- predict(Q4.log, newdata = Q4.indikator, type = "response")

print(Julehandel.predict) #0.555184

summary(Q4.log)$coef

#(Intercept) 1.4044486
#coef 0.1260095

#Høj sandsynlighed (tæt på 1): Der er en høj sandsynlighed for, at forbruget stiger.
#Lav sandsynlighed (tæt på 0): Der er en lav sandsynlighed for, at forbruget stiger (dvs. forbruget falder).
#vi vurderer at en sandsynlighed på over 0.7 betyder stigning og under betyder fald i forbruget


#### OPG 3.2 validering ####

#Predict funktion
#KONFUSIONSMATRICE

#forudsigelse på hele perioden
Q4.log.periode <- glm(julehandel$Dummy ~ julehandel$Indikator, family = "binomial")

Julehandel.predict.periode <- predict(Q4.log.periode, newdata = Q4.indikator, type = "response")
print(Julehandel.predict.periode)

julehandel$predict.dummy <- ifelse(Julehandel.predict.periode >= 0.7, print(1), print(0))
table(julehandel$predict.dummy)

library(ConfusionTableR)
library(lattice)
library(tidyverse)
library(caret)

#reele og forudsigelse
actual <- as.factor(julehandel$Dummy)
predicted <- as.factor(julehandel$predict.dummy)

#Lav konfusionsmatrice
conf_matrix <- confusionMatrix(actual, predicted)
print(conf_matrix)
####Visualiser KONFUSIONSMATRICE
library(ggplot2)
library(caret)

# Lav konfusionsmatrix (du har allerede denne del)
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Lav heatmap for konfusionsmatricen
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix", x = "Reference (Actual)", y = "Prediction (Predicted)") +
  theme_minimal()


#True Positives = 68 (modellen forudsagde 1, og den faktiske var også 1).
#True Negatives: 12 (modellen forudsagde 0, og den faktiske var også 0).
#False Positives: 9  gange forudsagde modellen 1, men den faktiske klasse var 0.
#False Negatives: 10 gange forudsage modellen 0, men den faktiske klasse var 1


##########ROC KURVE 
#logistisk regressionsmodel - hele periode
Julehandel2 <- glm(Dummy ~ Indikator, data = julehandel, family = "binomial")

Julehandel.predict <- predict(Julehandel2, type = "response") # Forudsig sandsynligheder
actual <- julehandel$Dummy  # De faktiske værdier

roc_curve <- roc(actual, Julehandel.predict)

# Plot ROC-kurven
plot(roc_curve, main = "ROC Kurve", col = "darkblue", lwd = 2)

# Beregn AUC
auc_value <- auc(roc_curve) #0.7884
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "darkblue", cex = 1.5)

#79% sandsynlighed for, at modellen korrekt rangerer en tilfældig positiv observation højere end en tilfældig negativ observation


#kan bruges hvis man vil til eksamen - metode til at finde optimal grænseværdi

roc_curve <- roc(NKHC021$dummy, Julehandel.predict.periode)

#Extract sensitivity, specificity, and thresholds
sensitivities <- roc_curve$sensitivities
specificities <- roc_curve$specificities
thresholds <- roc_curve$thresholds

#Calculate Youden's J statistic
youden_j <- sensitivities + specificities - 1

#Find the index of the maximum J statistic
optimal_index <- which.max(youden_j)

#Get the optimal threshold
optimal_threshold <- thresholds[optimal_index]


