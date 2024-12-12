####Opgave 2.1 – PCA regression ####
#Lav en PCA regression på jeres data fra opgave 1
library(pls)
#tager tidsperiode i dataframe - uden kvartal 4
FORV1.pcr <- FORV1[-nrow(FORV1),c(3:14)] 

pcr.fit <- pcr(NKHC021$Realvækst~FORV1.pcr$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                 FORV1.pcr$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                 FORV1.pcr$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                 FORV1.pcr$`Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                 FORV1.pcr$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                 FORV1.pcr$`Priser i dag, sammenlignet med for et år siden`+
                 FORV1.pcr$`Priser om et år, sammenlignet med i dag`+
                 FORV1.pcr$`Arbejdsløsheden om et år, sammenlignet med i dag`+
                 FORV1.pcr$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`+
                 FORV1.pcr$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`+
                 FORV1.pcr$`Regner med at kunne spare op i de kommende 12 måneder`+
                 FORV1.pcr$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`,
               data=FORV1.pcr, validation="CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings #loadings bruges i senere udregning
loadings.pcr.fit[1:12]

w.indicators.1 <- loadings.pcr.fit[1:12]^2 #loadings ^2 er vægtene
sum(w.indicators.1)

#### Opgave 2.2 – De vigtigste indikatorer ####
#højeste vægte
w.indicators.1

###Opgave 2.3 – Forudsig forbruget ####

#VORES METODE
#1.loadings er ganget på alle værdi i perioden 2000Q1-2024Q4
#2. ny indikator er oprettet som rowsums af hvert kvartal
#3. Q3 og Q4 værdier af indikator (m gangede loadings) er hentet
#4. En linær reg er udført med realvækst og den nye indikator
#5. Intercept og coef er hentet
#6. predict funktion er udført for Q3 og Q4

FORV1.pcr.2 <- as.data.frame(FORV1[,c(3:14)]) #inkl Q4
# Opret en tom dataframe til resultaterne
resultater <- as.data.frame(matrix(nrow = nrow(FORV1.pcr.2), ncol = ncol(FORV1.pcr.2)))

loadings <- pcr.fit$loadings[,1] #fanger loadings fra 1. comp

# Loop over hver kolonne i værdier - ganger laodings på spg i hele periode
for (j in 1:ncol(FORV1.pcr.2)) {
  # Multiplicer den j'te kolonne med den tilsvarende loading
  resultater[, j] <- FORV1.pcr.2[, j] * loadings[j]
}

# Opdater kolonnenavne
colnames(resultater) <- colnames(FORV1.pcr.2) #resultater er alle spg ganget om på vægtene

#lav det til en samlet indikator
resultater$PCA.FTI<- rowSums(resultater)

#gemmer Q3 og Q4 værdier
Q3 <- data.frame(PCA.FTI = resultater[99,13]) #vægtet samlet indikator Q3 2024
Q4 <- data.frame(PCA.FTI = resultater[100,13]) #vægtet samlet indikator Q4 2024


#forudisgelser m. predict
resultater.lm <- resultater[-100,] #fjerner Q4 så lm kan laves mod realvækst

resultater.lm$Realvækst <- NKHC021$Realvækst#tilføjer realvækst

PCA.lm <- lm(Realvækst~PCA.FTI, data = resultater.lm)
PCA.lm$coefficients
#Intercept = #1.36096291   
#PCA.FTI =  0.04246449

#Q3 2024 forudsigelse
predict(PCA.lm, newdata = Q3, type = "response") #0.28

#Q4 2024 forudsigelse
predict(PCA.lm, newdata = Q4, type = "response") #0.045


#manuel udregning Q4 
loadings <- pcr.fit$loadings[,1]

PC1 <- colSums(loadings* t(as.matrix(FORV1[-nrow(FORV1),3:14], ncol = 12)))

lm.fit <- lm(NKHC021$Realvækst ~ PC1) #linær reg m vægtet
summary(lm.fit)

coef.pcr <- c(coefficients(lm.fit)[2]*pcr.fit$loadings[,1]) #koef ganges m loadings

Q3.m <- FORV1[99,3:14] #sidste kartal
Q4.m <- FORV1[100,3:14] #sidste kartal

#Udregning Q3
#samler intercpt + coef + sidste kvartals tal
sum(data.frame(coefficients(lm.fit)[1],coef.pcr*Q3.m))
##[1] 0.2832294

#Udregning Q4
sum(data.frame(coefficients(lm.fit)[1],coef.pcr*Q4.m)) 
# [1] 0.04520508


#pcr.fit = ikke vægtet
#pca.lm = vægtet
