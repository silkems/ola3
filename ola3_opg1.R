library(dkstat)
library(tidyverse)

#Opgave 1 OLA 3 
################## Opgave 1.1 – Kombinationsalgoritme i R
#Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
#kvartal 2000 til og med 4. kvartal 2024.
#ny dataframe uden fti og realvækst
FORV1.2 <- FORV1[,-c(1:2)]

#test af korrelation - testet på alle - negativ korrelation på to nedenstående
cor(NKHC021$Realvækst, FORV1.2$`Priser i dag, sammenlignet med for et år siden`[-100])
cor(NKHC021$Realvækst, FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag`[-100])

#Ganger med -1 på spørgsmål 6 og 8, for at ændre værdierne i de to spørgsmål (de er negativ korrelerede til realvæksten)
FORV1.2$`Priser i dag, sammenlignet med for et år siden`= FORV1.2$`Priser i dag, sammenlignet med for et år siden`*-1
FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag` = FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1


# Liste til at gemme alle kombinationer
kombinationer_liste <- list()

for (k in 1:12) {
  # Brug combn til at finde alle kombinationer af 'k' spørgsmål
  comb_result <- combn(FORV1.2, k, simplify = FALSE)
  
  # Gem hver kombination som en data frame
  kombinationer_liste[[paste0("komb_", k)]] <- comb_result
}

#Der er i alt 4095 kombinationer af de 12 spørgsmål i alt

##### Opgave 1.2 – R2 og forbrugertillidsindikatorer ####

# tag gns af alle rækker så alle kombinationer kan dannes til en samlet indikator

mean_results <- list() #liste til at gemme gennemsnittene

# Gå igennem hver kombination fra komb_1 til komb_12
for (komb_index in 1:12) {
  komb_name <- paste0("komb_", komb_index)  # Generer kolonnenavnet dynamisk
  mean_values <- numeric()  # Opret en vektor til at gemme gennemsnittene for den aktuelle kombination
  
  # Gå igennem hver data frame i den nuværende kombination
  for (i in 1:length(kombinationer_liste[[komb_name]])) {
    df <- kombinationer_liste[[komb_name]][[i]]  # Få den aktuelle data frame
    mean_values[i] <- mean(rowMeans(df, na.rm = TRUE))  # Beregn gennemsnittet af rækkerne
  }
  
  # Gem resultaterne i listen
  mean_results[[komb_name]] <- mean_values
}

# Opret en tom liste til at samle gennemsnittene
gennemsnit_liste <- list()

# Iterér gennem hver kombinationsliste (komb_1 til komb_12)
for (k in 1:length(kombinationer_liste)) {
  # Hent hver liste af kombinationer af data frames
  comb_list <- kombinationer_liste[[k]]
  
  # Loop gennem hver kombination i den aktuelle liste
  for (i in 1:length(comb_list)) {
    # Hent den aktuelle kombination (som er en matrix af data)
    current_comb <- comb_list[[i]]
    
    # Konverter data til en matrix, hvis den ikke allerede er det
    current_comb_matrix <- as.matrix(current_comb)
    
    # Beregn række-gennemsnittet
    row_avg <- rowMeans(current_comb_matrix, na.rm = TRUE)
    
    # Tilføj gennemsnittet til vores gennemsnit_liste
    gennemsnit_liste[[paste0("Komb_", k, "_", i)]] <- row_avg
  }
}

# Konverter listen med gennemsnittene til en data frame
gennemsnit_df <- as.data.frame(do.call(cbind, gennemsnit_liste))

# Tilføj kolonnenavne for at identificere hver kombination
colnames(gennemsnit_df) <- names(gennemsnit_liste)

# Vis dataframen med gennemsnittene
print(gennemsnit_df)


##### Lav lineær regression på alle rækkerne #####

##Først skal vi kombinere gennemsnit_df med realvækstkolonnen fra din data
#realvækst fo Q4 er ikke givet, men vi har forudset denne i OLA 2. opg 2.2 - denne værdi kan evt tilføjes istedet for at fjerne sidste række
#[1] "Forudsigelse af realvækst baseret på FTI: -0.243620892083438"

# Kombiner realvækst fra perioden med gennemsnittene
data_for_regression <- cbind(Realvækst = NKHC021$Realvækst, gennemsnit_df[-nrow(gennemsnit_df),])

# Fjern eventuelle rækker med NA-værdier, da de kan påvirke regressionen
data_for_regression <- na.omit(data_for_regression)

# Vis den kombinerede data frame
head(data_for_regression)


####### Udfør lineær regression på alle kombinationer #######

# Opret en liste til at gemme regression resultater
regression_resultater <- list()

# Loop gennem hver kombination (hver kolonne i gennemsnit_df)
for (kombination in colnames(gennemsnit_df)) {
  # Udfør lineær regression mellem Realvækst og den aktuelle kombination
  model <- lm(Realvækst ~ data_for_regression[[kombination]], data = data_for_regression)
  
  # Gem resultaterne (du kan gemme hele modelobjektet eller bare resuméet)
  regression_resultater[[kombination]] <- summary(model)
}

# Se resultaterne for en specifik kombination
print(regression_resultater[["Komb_1_1"]])  # Skift "Komb_1_1" til en anden kombination efter behov


##### Beregning af R2 for alle kombinationer #####

# Opret en tom liste til at gemme R2-værdierne
r_squared_values <- list()

# Loop gennem regression resultaterne og træk R2 værdien ud for hver model
for (kombination in names(regression_resultater)) {
  # Træk R2 værdien fra modelresuméet
  r_squared_values[[kombination]] <- regression_resultater[[kombination]]$r.squared
}

# Konverter listen med R2 værdier til en data frame for lettere visualisering/analyse
r_squared_df <- as.data.frame(do.call(rbind, r_squared_values))

# Tilføj kolonnenavne (hvis nødvendigt)
colnames(r_squared_df) <- c("R2")

# Print dataframen med R2 værdierne
print(r_squared_df)


####top 10 kombinationer####

# Sorter R2 værdierne for at finde de højeste forklaringsgrader
r_squared_sorted <- r_squared_df[order(-r_squared_df$R2), , drop = FALSE]

# Konverter rownames til en ny kolonne
r_squared_sorted$Kombination <- rownames(r_squared_sorted)

# 10 øverste rækker baseret på R2
top_10_r2 <- r_squared_sorted[1:10, c("Kombination", "R2")]


#Resultat af 10 kombinationer har den højeste R2:
#Komb_5_660 0.4417049
#Komb_4_355 0.4403798
#Komb_6_837 0.4390678
#Komb_7_585 0.4367412
#Komb_6_582 0.4362860
#Komb_5_400 0.4357517
#Komb_5_653 0.4355240
#Komb_6_575 0.4340686
#Komb_7_756 0.4326847
#Komb_4_368 0.4311775

#Resultat: Ud fra dette resultat kan vi se, at kombinationen "Komb_5_660" har den højeste R2-værdi på 0.4417049

#Beregn korrelationen for alle top 10:

top_10_r2$Correlation <- NA  # tom kolonne

# Loop gennem hver kombination og beregn korrelationen
for (i in seq_len(nrow(top_10_r2))) {
  kombination <- top_10_r2$Kombination[i]  # 
  
  # Beregn korrelationen mellem Realvækst og den aktuelle kombination
  cor_value <- cor(data_for_regression$Realvækst, data_for_regression[[kombination]])
  
  # Gem korrelationen i top_10_r2
  top_10_r2$Correlation[i] <- cor_value
}

#p-værdi for top 10

# Initialiser en liste til at gemme p-værdier
p_values <- list()

# Loop gennem hver kombination
for (kombination in top_10_r2$Kombination) {
  # Hent lm
  model_summary <- regression_resultater[[kombination]]
  
  #  p-værdier fra coefficients
  p_values[[kombination]] <- model_summary$coefficients[2,4]
}

# Konverter listen til en data frame
p_values_df <- do.call(rbind, lapply(names(p_values), function(komb) {
  data.frame(Kombination = komb, t(p_values[[komb]]))
}))

# Vis resultatet
print(p_values_df)

top_10_r2$P.værdi <- p_values_df[,2]




####### Opgave 1.3 ####
#Spørgsmål i den bedste indikatoren

# Hent kombination nummer 551 i listen over 7-spørgsmålskombinationer (Komb_5_660)
Komb_5_660 <- kombinationer_liste[["komb_5"]][[660]]

# Udskriv navnene på de spørgsmål i kombinationen
sporgsmaal_Komb_5_660 <- colnames(Komb_5_660)

# Print spørgsmålene
sporgsmaal_Komb_5_660

#Spørgsmålene som indgår i indikatoren "Komb_5_660" som bedst forklarer variatione i forbruget er:
#[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"                       
#[2] "Priser om et år, sammenlignet med i dag"                                                     
#[3] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."                             
#[4] "Regner med at kunne spare op i de kommende 12 måneder"                                       
#[5] "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"




### Opgave 1.4 #### 
#Forudsigelser med afsæt i jeres indikatorer

## Dataframe med vores indikator

Vores_FTI <- data.frame(TID = FTI$TID)
Vores_FTI$VFTI <- data_for_regression$Komb_5_660
Vores_FTI$Realvækst <- FTI$Realvækst

vores_reg <- lm(formula = Realvækst ~
                  VFTI, data = Vores_FTI)

summary(vores_reg)
vores_reg$coefficients #   0.2749 
# Selv små ændringer i  VFTI forårsager proportionelle ændringer i forudsigelsen.
# Interceptet er negativt, men vægten fra VFTI-bidraget er høj nok til at overgå det.


## Beregn VFTI for Q3 24, og Q4 24 (1 måned)

## Q3 og Q4 24
VFTIQ3 <- round(mean(as.numeric(FORV1.2[99, c(3, 7, 9, 11, 12)])),2) #11.72667
VFTIQ4 <- round(mean(as.numeric(FORV1.2[100, c(3, 7, 9, 11, 12)])),2) #11.86
print(VFTIQ4)

print(FORV1.2[99, c(3, 7, 9, 11, 12)]) #se de enkeltes værdier

## Opret nyt data til predict med Q3 og Q4(én måned)

nyt_data <- data.frame(VFTI = c(VFTIQ3, VFTIQ4))

predict(vores_reg, newdata = nyt_data, type = "response")
## 1          2 
## 1.674573   1.711224 


#### Opgave 1.5 ######

#ny dataframe med alle mikrospørgsmål
FORV1.mikro <- FORV1.2[,c(1,2,9,11,12)]

kombinationer_mikro <- list()

for (k in 1:5) {
  # Brug combn til at finde alle kombinationer af 'k' spørgsmål
  comb_result <- combn(FORV1.mikro, k, simplify = FALSE)
  
  # Gem hver kombination som en data frame
  kombinationer_mikro[[paste0("komb_", k)]] <- comb_result
}


# Beregn gennemsnittet på alle rækker på de 31 kombinationer af de 5 spørgsmål og udskriv i dataframe

# Opret en tom liste til at samle gennemsnittene
gennemsnit_mikro <- list()

# Iterér gennem hver kombinationsliste (komb_1 til komb_12)
for (k in 1:length(kombinationer_mikro)) {
  # Hent hver liste af kombinationer af data frames
  comb_mikro <- kombinationer_mikro[[k]]
  
  # Loop gennem hver kombination i den aktuelle liste
  for (i in 1:length(comb_mikro)) {
    # Hent den aktuelle kombination (som er en matrix af data)
    current_mikro <- comb_mikro[[i]]
    
    # Konverter data til en matrix, hvis den ikke allerede er det
    current_mikro_matrix <- as.matrix(current_mikro)
    
    # Beregn række-gennemsnittet
    row_avg <- rowMeans(current_mikro_matrix, na.rm = TRUE)
    
    # Tilføj gennemsnittet til vores gennemsnit_liste
    gennemsnit_mikro[[paste0("Komb_", k, "_", i)]] <- row_avg
  }
}

# Konverter listen med gennemsnittene til en data frame
gennemsnit_mikro <- as.data.frame(do.call(cbind, gennemsnit_mikro))

# Vis dataframen med gennemsnittene
View(gennemsnit_mikro)

####### Lav lineær regression på alle rækkerne ########


##Først skal vi kombinere gennemsnit_df med realvækstkolonnen fra din data

# Kombiner realvækst fra perioden med gennemsnittene
mikro_for_regression <- cbind(Realvækst = NKHC021$Realvækst, gennemsnit_mikro[-nrow(gennemsnit_mikro),])

# Vis den kombinerede data frame
View(mikro_for_regression)


####### Udfør lineær regression på alle kombinationer #######

# Opret en liste til at gemme regression resultater
regression_mikro <- list()

# Loop gennem hver kombination (hver kolonne i gennemsnit_df)
for (kombination in colnames(gennemsnit_mikro)) {
  # Udfør lineær regression mellem Realvækst og den aktuelle kombination
  model <- lm(Realvækst ~ mikro_for_regression[[kombination]], data = mikro_for_regression)
  
  # Gem resultaterne (du kan gemme hele modelobjektet eller bare resuméet)
  regression_mikro[[kombination]] <- summary(model)
}


##### Mikro R2 ####

# Opret en tom liste til at gemme R2-værdierne
r_squared_mikro <- list()

# Loop gennem regression resultaterne og træk R2 værdien ud for hver model
for (kombination in names(regression_mikro)) {
  # Træk R2 værdien fra modelresuméet
  r_squared_mikro[[kombination]] <- regression_mikro[[kombination]]$r.squared
}

# Konverter listen med R2 værdier til en data frame for lettere visualisering/analyse
r_squared_mikro <- as.data.frame(do.call(rbind, r_squared_mikro))

# Tilføj kolonnenavne (hvis nødvendigt)
colnames(r_squared_mikro) <- c("R2")

# Sorter R2 værdierne for at finde de højeste forklaringsgrader
r_squared_mikro_sorted <- r_squared_mikro[order(-r_squared_mikro$R2), , drop = FALSE]

# Print dataframen med R2 værdierne
head(r_squared_mikro_sorted)

# Hent kombination 3 i listen over 1-spørgsmålskombinationer (Komb_1_3)
komb_1_3 <- kombinationer_mikro[["komb_1"]][[3]]

# Udskriv navnene på de spørgsmål i kombinationen
sporgsmaal_1_3 <- colnames(komb_1_3)

# [1] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."


####graf af mikro og makro spg ####

FORV1.mikro <- FORV1.2[,c(1,2,9,11,12)]
FORV1.makro <- FORV1.2[,c(3,4,5,6,7,8,10)]

# Beregn gennemsnittet af hver række (minus tidskolonnen) i df1 og df2
Mikro.gns <- rowMeans(FORV1.mikro, na.rm = TRUE)  # Fjern den første kolonne (tidskolonnen)
Makro.gns <- rowMeans(FORV1.makro, na.rm = TRUE)  # Fjern den første kolonne (tidskolonnen)

gns <- data.frame(Mikro.gns,Makro.gns)
gns.plot <- cbind(gns[-nrow(gns),], NKHC021)

library(ggplot2)

# Skaler Realvækst op med en skaleringsfaktor, f.eks. 10
gns.plot$Realvækst_scaled <- gns.plot$Realvækst * 2

gns.plot$Year <- substr(gns.plot$TID, 1, 4)


# Opret ggplot med to y-akser
ggplot(data = gns.plot, aes(x = TID)) +
  
  # Søjlediagram for den skalerede Realvækst
  geom_bar(aes(y = Realvækst_scaled), stat = "identity", fill = "grey", color = "lightgrey") +
  
  # Linjediagram for Mikro.gns
  geom_line(aes(y = Mikro.gns, group = 1, color = "Mikro.indikator"), linewidth = 0.5) +
  
  # Linjediagram for Makro.gns
  geom_line(aes(y = Makro.gns, group = 1, color = "Makro.indikator"), linewidth = 0.5) +
  
  # Venstre y-akse (Realvækst)
  scale_y_continuous(name = "Årlig Realvækst (Pct.)",
                     sec.axis = sec_axis(~ ., name = "Mikro og Makro indikatorer (Nettotal)") ) +
  
  # Tilføj x-akse og titel
  labs(x = "Tid", 
       title = "Større variation i indikatortal i makroøkonomiske forbrugerspørgsmål"
  ) +
  
  # Juster x-aksen til kun at vise årstallet (én gang per år)
  scale_x_discrete(labels = function(x) {
    # Skift hver fjerde kvartal ud med årstallet, kun vis én gang per år
    ifelse(grepl("Q1", x), substr(x, 1, 4), "")
  }) +  # Custom labels, viser årstallet kun én gang per år
  
  # Æstetiske tilpasninger
  theme_minimal() +
  theme(axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  
  # Tilføj en legend
  scale_fill_manual(name = "Variabler", values = c("Realvækst" = "lightgrey")) +
  scale_color_manual(name = "Variabler", values = c("Mikro.indikator" = "red", "Makro.indikator" = "blue"))

