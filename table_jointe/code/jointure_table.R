#library##

library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)


### import data###

customer_loyalty_history <- read_delim("input/customer_loyalty_history.txt", 
                                       delim = "!", escape_double = FALSE, trim_ws = TRUE)

Customer_Flight_Activity <- readRDS("C:/Users/arman/OneDrive/Bureau/ExamensR/table_jointe/input/Customer_Flight_Activity.rds")


# 1) joindre deux tables ###

# Créer les données de exemple pour les deux tables (remplacez les données factices par vos données réelles)
missing_in_history <- c("100018", "100102", "100140", "100214", "100272", "100301", "100364", "100380", "100428", "100504", "100550", "863070", "100590", "100642", "100644", "100646")
missing_in_activity <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22")

# Créer les deux data frames à partir des vecteurs
df_missing_in_history <- data.frame(ID = missing_in_history)
df_missing_in_activity <- data.frame(ID = missing_in_activity)

# Faire la jointure des deux tables
df_jointure <- merge(df_missing_in_history, df_missing_in_activity, by = "ID", all = TRUE)

# Dimensions de la nouvelle table
dimensions <- dim(df_jointure)


# 3)- A partir d'ici, travailler uniquement sur les clients qui n'ont échangé aucun point au cours du dernier trimestre de 2017 et 2018 et qui sont célibataires

library(dplyr)

# 1. Jointure des deux dataframes
merged_data <- Customer_Flight_Activity %>%
  inner_join(cleaner_customer_loyalty_history, by = "customer_id")

# 2. Filtrer les clients qui n'ont pas échangé de points au cours du dernier trimestre de 2017 et 2018 et qui sont célibataires
filtered_data <- merged_data %>%
  filter(
    (year(activity_date) == 2017 & quarter(activity_date) == 4 | 
       year(activity_date) == 2018 & quarter(activity_date) == 4) &
      points_exchanged == 0 &
      marital_status == "Single"
  )


### 3)- Compter le nombre de clients présents dans cette table###
# Compter le nombre de clients présents dans la table
nombre_clients <- cleaned_customer_loyalty_history %>%
  summarise(nombre_clients = n())

# Afficher le nombre de clients
nombre_clients

### 4)- Recodons  la variable Education en créant la variable edu_fr qui traduit les valeurs de Education en français###

# Recodons  la variable Education en français
cleaned_customer_loyalty_history$edu_fr <- education_mapping[cleaned_customer_loyalty_history$Education]

# Affichons  les premières lignes pour vérification
head(cleaned_customer_loyalty_history)


### 5) - Recodons  le CLV en cat_clv qui reprend les valeurs suivantes

# Calculons les quantiles pour définir les seuils
quantiles_CLV <- quantile(cleaned_customer_loyalty_history$CLV, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Fonction pour recoder CLV en cat_clv
recoder_CLV <- function(value) {
  if (is.na(value)) {
    return(NA)
  } else if (value <= quantiles_CLV[2]) {
    return("min à 1er quartile")
  } else if (value <= quantiles_CLV[3]) {
    return("premier quartile - mediane")
  } else if (value <= quantiles_CLV[4]) {
    return("mediane - 3eme quartile")
  } else {
    return("3eme quartile - Maximum")
  }
}

# Appliqueons la fonction pour recoder CLV en cat_clv
cleaned_customer_loyalty_history$cat_clv <- sapply(cleaned_customer_loyalty_history$CLV, recoder_CLV)

# Affichons les premières lignes pour vérification
head(cleaned_customer_loyalty_history)


### 6)- Faire le tableau de contingence entre edu_fr et cat_clv

# Chargeons  la librairie "gmodels" si elle n'est pas déjà chargée
if (!require(gmodels)) {
  install.packages("gmodels")
  library(gmodels)
}

# Créeons le tableau de contingence
contingency_table <- table(cleaned_customer_loyalty_history$edu_fr, cleaned_customer_loyalty_history$cat_clv)

# Afficheons le tableau de contingence
contingency_table


### 7)- Faire le graphique en bar de la moyenne de la distance parcouru par eud_fr colorié par cat_clv###

# Créer le graphique en barre de la moyenne avec Enrollment.Type et CLV
bar_plot_enrollment_clv <- cleaned_customer_loyalty_history %>%
  group_by(edu_fr, cat_clv, Enrollment.Type..) %>%
  summarise(mean_clv = mean(CLV.., na.rm = TRUE)) %>%
  ggplot(aes(x = edu_fr, y = mean_clv, fill = cat_clv)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Enrollment.Type..) +
  labs(title = "Moyenne de la valeur à vie du client par edu_fr, coloriée par cat_clv et facet_wrap par Enrollment.Type",
       x = "Education (en français)",
       y = "Valeur à vie du client (moyenne)",
       fill = "cat_clv") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(bar_plot_enrollment_clv)


### 8)- Faire la table de correlation entre Distance, Total Flights, Salary et CLV : interpretez les résultats (particulièeremnt Distance vs Salary)###

# Sélectionner les colonnes pertinentes
selected_columns <- cleaned_customer_loyalty_history[, c( "Salary..", "CLV..")]

# Calculer la matrice de corrélation
correlation_matrix <- cor(selected_columns, use="complete.obs")

# Afficher la matrice de corrélation
print(correlation_matrix)

###10- exporter en format excel les résultats des questions 8 et 9###


}



openxlsx::write.xlsx(list(selected_columns = selected_columns
                          
                          
), file = "output/stat_etude.xlsx")

## sauvegarde ####

save.image(file = "sauvegarde/etudecomplete.RData")
