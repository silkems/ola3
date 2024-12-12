#### Opgave 4.1 #### 
#stabilitetstest 
library(tidyverse)

####1. nested-loop####
#Her fjernes kvartaler fra 2000Q1 og 40 kvartaler frem

#data frame for alle kombinationer 
stab_gns <- data_for_regression

stab_df <- data.frame() # tom data frame til at samle R2 resultaterne

for (j in 2:ncol(stab_gns)) {  #1. loop - iterer gennem alle kombinationer 
  for (i in 99:60) { #2. loop - fjerner kvartalsvis og udregner R2 for periode
    
    temp_df <- stab_gns[1:i, c(1, j)]  # #midlertidig dataframe med Realvækst og den j'te indikator-kolonne
    
    model <- lm(Realvækst ~ ., data = temp_df) #lineær regression med Realvækst som y og den valgte komb som x
    
    stab_df[100 - i, j - 1] <- summary(model)$r.squared #gemmer R2 værdi i stab_df
  }
}

# Opdater kolonnenavne med indikator-navnene
colnames(stab_df) <- colnames(stab_gns)[2:ncol(stab_gns)]

####Stabilitetstest####

#Transponer data frame så den kan analyseres med rowMeans og opretter kolonne med udregnet gns
stab_df <- data.frame(t(stab_df))
stab_gns <- rowMeans(stab_df)
stab_df$Gns <- stab_gns

#Find 25%- og 75%-kvartiler for R2 for hver kombination
Kvartilstest <- t(apply(stab_df[, 1:40], 1, quantile, probs = c(0.25, 0.75)))

# Tilføj kvartilerne som nye kolonner i data framen
stab_df$Q25 <- Kvartilstest[, 1]
stab_df$Q75 <- Kvartilstest[, 2] 

#Opret en stabilitetsscore
stab_df <- stab_df %>%
  mutate(Score = Gns/(Q75 - Q25)) %>% 
  arrange(desc(Score))  # Sortér efter højeste score først

stab_results <- stab_df[, 41:44]

#Høj score = høje R2 værdier og lav variation = meget stabil
#Lav score = lave R2 værdier eller høj variationen (eller begge dele) = mindre stabil

## Højeste score er kombination 3_116 
Komb_3_116 <- kombinationer_liste[["komb_3"]][[116]]
colnames(Komb_3_116)

#[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"
#[2] "Priser i dag, sammenlignet med for et år siden"                       
#[3] "Priser om et år, sammenlignet med i dag"


####2. nested-loop#### 
#fra 2024Q3 og 40 kvartaler tilbage
#data frame for alle kombinationer 
stab_gns2 <- data_for_regression 

# Opret en tom data frame til at samle R2 resultaterne
stab_df2 <- data.frame()

# For-loop for at beregne R2 for hver indikator-kolonne 
for (j in 2:ncol(stab_gns2)) {  #1. loop - iterer gennem alle kombinationer 
  for (i in 1:40) { #2. loop - fjerner kvartalsvis og udregner R2 for periode
    
    temp_df <- stab_gns2[i:99, c(1, j)]  
    
    model <- lm(Realvækst ~ ., data = temp_df)
    
    stab_df2[i, j - 1] <- summary(model)$r.squared
  }
}

#### stabilitetstest 2 ####
# Opdater kolonnenavne med indikator-navnene
colnames(stab_df2) <- colnames(stab_gns2)[2:ncol(stab_gns2)]

# Transponer data frame så den kan analyseres med rowMeans
stab_df2 <- data.frame(t(stab_df2))
stab_gns2 <- rowMeans(stab_df2)
stab_df2$Gns <- stab_gns2

#Find 25%- og 75%-kvartiler for R2 for hver kombination
Kvartilstest2 <- t(apply(stab_df2[, 1:40], 1, quantile, probs = c(0.25, 0.75)))

# Tilføj kvartilerne som nye kolonner i data framen
stab_df2$Q25 <- Kvartilstest2[, 1]
stab_df2$Q75 <- Kvartilstest2[, 2] 

# Opret en stabilitetsscore
stab_df2 <- stab_df2 %>% 
  mutate(Score = Gns/(Q75 - Q25)) %>% 
  arrange(desc(Score))  # Sortér efter højeste score først

# Se data frame med resultaterne
View(stab_df2)

stab_results2 <- stab_df2[, 41:44]

rownames(stab_results2)[1:5] #udtræk 5 med højeste score
#Komb_6_750" "Komb_8_474" "Komb_6_877" "Komb_7_709" "Komb_6_624"

## Højeste score er kombination 6_750 
Komb_6_750 <- kombinationer_liste[["komb_6"]][[750]]
colnames(Komb_6_750)

#[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"
#[2] "Danmarks økonomiske situation om et år, sammenlignet med i dag"       
#[3] "Priser i dag, sammenlignet med for et år siden"                       
#[4] "Priser om et år, sammenlignet med i dag"                              
#[5] "Arbejdsløsheden om et år, sammenlignet med i dag"                     
#[6] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.


