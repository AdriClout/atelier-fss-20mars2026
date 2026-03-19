################################################################################
#                                                                              #
#   ATELIER PRATIQUE — Approfondissement en analyse de données avec R          #
#   FSS, Université Laval — 20 mars 2026                                       #
#   Adrien Cloutier                                                            #
#                                                                              #
#   Instructions :                                                             #
#   1. Exécutez les lignes une par une (Ctrl+Enter / Cmd+Enter)                #
#   2. Lisez les commentaires — ils expliquent chaque étape                    #
#   3. Les exercices sont progressifs — commencez par le début!                #
#                                                                              #
#   Solutions disponibles en ligne :                                           #
#   https://adriencloutier.com/atelier-fss-20mars2026/exercices.html           #
#                                                                              #
################################################################################


# ==============================================================================
# 0. INSTALLATION ET CHARGEMENT DES PACKAGES
# ==============================================================================

# À faire UNE SEULE FOIS (dans la console, pas dans le fichier)
# install.packages(c("tidyverse", "gapminder", "tidytext", "wordcloud", "modelsummary"))

# À faire à chaque session
library(tidyverse)
library(gapminder)
library(tidytext)
library(wordcloud)
library(modelsummary)


# ==============================================================================
# SECTION 1 : RAPPEL DPLYR
# ==============================================================================

# Les données Gapminder : 142 pays, 1952 à 2007, toutes les 5 ans
glimpse(gapminder)

# ------ Exercice 1.1 ------
# Filtrez les données pour obtenir uniquement les pays d'Asie en 2007.
# Sélectionnez seulement les colonnes : country, lifeExp, gdpPercap
# Triez par espérance de vie décroissante.
# Combien de pays asiatiques ont une espérance de vie > 75 ans?

gapminder |>
  filter(???) |>
  select(???) |>
  arrange(???)


# ------ Exercice 1.2 ------
# Calculez, pour chaque continent et chaque année, la MÉDIANE du PIB per capita.
# Nommez la colonne résultante "pib_median".
# Affichez seulement les données depuis 1990.

gapminder |>
  filter(???) |>
  group_by(???) |>
  summarise(???)


# ------ Exercice 1.3 ------
# Créez une nouvelle colonne "pib_total" qui correspond à pop * gdpPercap.
# Trouvez les 5 pays avec le PIB total le plus élevé en 2007.

gapminder |>
  filter(???) |>
  mutate(???) |>
  arrange(???) |>
  slice_head(n = 5)

# --> Solutions : https://adriencloutier.com/atelier-fss-20mars2026/exercices.html


# ==============================================================================
# SECTION 2 : VISUALISATION AVEC GGPLOT2
# ==============================================================================

# ------ Exercice 2.1 : Nuage de points ------
# Reproduisez le graphique vu en présentation :
# - Axe X : PIB per capita (échelle logarithmique)
# - Axe Y : Espérance de vie
# - Couleur : continent
# - Données : 2007 seulement
# - Titre, étiquettes d'axes, thème minimal

gapminder |>
  filter(???) |>
  ggplot(aes(???)) +
  geom_point(???) +
  scale_x_log10() +
  labs(???) +
  theme_minimal()


# ------ Exercice 2.2 : Évolution dans le temps ------
# Tracez l'évolution de l'espérance de vie MOYENNE par continent de 1952 à 2007.
# Utilisez geom_line() avec une ligne par continent.
# Astuce : calculez d'abord la moyenne avec group_by() + summarise()

gapminder |>
  group_by(???) |>
  summarise(???) |>
  ggplot(aes(x = year, y = ???, color = continent)) +
  geom_line(linewidth = 1) +
  labs(???) +
  theme_minimal()


# ------ Exercice 2.3 : Distribution ------
# Faites un histogramme de la distribution du PIB per capita en 2007.
# Colorez selon le continent (fill = continent).
# Ajoutez alpha = 0.6 pour la transparence.

# Votre code ici :


# --> Solutions : https://adriencloutier.com/atelier-fss-20mars2026/exercices.html


# ==============================================================================
# SECTION 3 : ANALYSE TEXTUELLE
# ==============================================================================

# Nos données : réponses à une question ouverte de sondage
reponses <- tibble(
  id     = 1:8,
  groupe = c("Centre", "Droite", "Centre", "Gauche",
             "Centre", "Droite", "Gauche", "Droite"),
  texte  = c(
    "The government must invest more in clean energy and protect the environment",
    "The cost of living crisis is terrible, prices are too high for working families",
    "Our healthcare system needs urgent reform and more funding immediately",
    "I appreciate the government's efforts on climate change and housing affordability",
    "Education should be free and accessible to all young people in this country",
    "The economy is struggling and the government has failed to address inflation",
    "We need stronger social programs and better support for vulnerable communities",
    "This government's terrible policies have destroyed our economic opportunities"
  )
)


# ------ Exercice 3.1 : stringr ------
# Avec le data frame reponses :
# a) Créez une colonne "nb_mots" avec le nombre de mots dans chaque réponse
#    (indice : str_count(texte, "\\w+"))
# b) Créez une colonne "mentionne_gouvernement" (TRUE/FALSE) si "government" apparaît
# c) Filtrez pour garder seulement les réponses qui mentionnent "government"

reponses |>
  mutate(
    nb_mots                = str_count(???, "\\w+"),
    mentionne_gouvernement = str_detect(???, ???)
  ) |>
  filter(???)


# ------ Exercice 3.2 : Tokenisation et fréquences ------
# Tokenisez les réponses avec unnest_tokens()
# Enlevez les stop words (anti_join avec stop_words)
# Affichez les 10 mots les plus fréquents sous forme de graphique en barres

mots <- reponses |>
  unnest_tokens(mot, texte)

data(stop_words)

mots_propres <- mots |>
  anti_join(???, by = c("mot" = "word"))

mots_propres |>
  count(???, sort = TRUE) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(mot, n), y = n)) +
  geom_col(fill = "#003875") +
  coord_flip() +
  labs(title = "Mots les plus fréquents", x = "Mot", y = "Fréquence") +
  theme_minimal()


# ------ Exercice 3.3 : Sentiment par groupe ------
# Calculez le score de sentiment net (positifs - négatifs) pour chaque groupe
# (Centre, Droite, Gauche) en utilisant le lexique "bing"
# Visualisez le résultat

bing <- get_sentiments("bing")

mots_propres |>
  inner_join(???, by = c("mot" = "word")) |>
  count(groupe, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(score_net = positive - negative) |>
  ggplot(aes(x = groupe, y = score_net, fill = score_net > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick", "#2ca25f")) +
  labs(title = "Ton des réponses par groupe politique",
       x = "Groupe", y = "Score net") +
  theme_minimal()

# --> Solutions : https://adriencloutier.com/atelier-fss-20mars2026/exercices.html


# ==============================================================================
# SECTION 4 : RÉGRESSION LINÉAIRE
# ==============================================================================

data_2007 <- gapminder |>
  filter(year == 2007) |>
  mutate(log_pib = log(gdpPercap))

# ------ Exercice 4.1 : Régression simple ------
# Faites une régression : lifeExp ~ log_pib
# Affichez le résumé avec summary()
# Interprétez : l'effet est-il significatif? Positif ou négatif?

modele <- lm(???, data = data_2007)
summary(modele)


# ------ Exercice 4.2 : Visualiser la droite ------
# Représentez la relation log_pib ~ lifeExp avec geom_point() + geom_smooth(method = "lm")

data_2007 |>
  ggplot(aes(x = ???, y = ???)) +
  geom_point(aes(color = continent), alpha = 0.7) +
  geom_smooth(???) +
  labs(???) +
  theme_minimal()


# ------ Exercice 4.3 : Régression multiple ------
# Ajoutez continent comme deuxième prédicteur.
# Comparez les deux modèles avec modelsummary().

modele_multiple <- lm(lifeExp ~ log_pib + ???, data = data_2007)

modelsummary(
  list("Modèle simple" = modele, "Modèle multiple" = modele_multiple),
  stars = TRUE
)

# --> Solutions : https://adriencloutier.com/atelier-fss-20mars2026/exercices.html


# ==============================================================================
# FIN — Bonne continuation!
# ==============================================================================
# Questions : adrien.cloutier.1@ulaval.ca
# Site de l'atelier : https://adriencloutier.com/atelier-fss-20mars2026/
# R for Data Science : https://r4ds.hadley.nz/
