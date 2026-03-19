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
################################################################################


# ==============================================================================
# 0. INSTALLATION ET CHARGEMENT DES PACKAGES
# ==============================================================================

# À faire UNE SEULE FOIS (dans la console, pas dans le fichier)
# install.packages("tidyverse")
# install.packages("gapminder")
# install.packages("tidytext")

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

# Votre code ici :
gapminder |>
  filter(???) |>
  select(???) |>
  arrange(???)


# ------ Exercice 1.2 ------
# Calculez, pour chaque continent et chaque année, la MÉDIANE du PIB per capita.
# Nommez la colonne résultante "pib_median".
# Affichez seulement les données depuis 1990.

# Votre code ici :
gapminder |>
  filter(???) |>
  group_by(???) |>
  summarise(???)


# ------ Exercice 1.3 ------
# Créez une nouvelle colonne "pib_total" qui correspond à pop * gdpPercap.
# Trouvez les 5 pays avec le PIB total le plus élevé en 2007.

# Votre code ici :
gapminder |>
  filter(???) |>
  mutate(???) |>
  arrange(???) |>
  slice_head(n = 5)


# CORRECTION 1.1
gapminder |>
  filter(year == 2007, continent == "Asia") |>
  select(country, lifeExp, gdpPercap) |>
  arrange(desc(lifeExp))

# CORRECTION 1.2
gapminder |>
  filter(year >= 1990) |>
  group_by(continent, year) |>
  summarise(pib_median = median(gdpPercap), .groups = "drop")

# CORRECTION 1.3
gapminder |>
  filter(year == 2007) |>
  mutate(pib_total = pop * gdpPercap) |>
  arrange(desc(pib_total)) |>
  slice_head(n = 5)


# ==============================================================================
# SECTION 2 : VISUALISATION AVEC GGPLOT2
# ==============================================================================

# ------ Exercice 2.1 : Nuage de points ------
# Reproduisez le graphique suivant :
# - Axe X : PIB per capita (échelle logarithmique)
# - Axe Y : Espérance de vie
# - Couleur : continent
# - Données : 2007 seulement
# - Titre, sous-titre, étiquettes d'axes appropriés
# - Thème minimal

# Votre code ici :
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
# Astuce : calculez d'abord la moyenne par continent et année avec group_by() + summarise()

# Votre code ici :
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


# CORRECTION 2.1
gapminder |>
  filter(year == 2007) |>
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(
    title    = "PIB per capita et espérance de vie (2007)",
    subtitle = "Chaque point représente un pays",
    x        = "PIB per capita (échelle log)",
    y        = "Espérance de vie (années)",
    color    = "Continent",
    size     = "Population"
  ) +
  theme_minimal()

# CORRECTION 2.2
gapminder |>
  group_by(continent, year) |>
  summarise(esperance_moy = mean(lifeExp), .groups = "drop") |>
  ggplot(aes(x = year, y = esperance_moy, color = continent)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Évolution de l'espérance de vie par continent (1952–2007)",
    x     = "Année",
    y     = "Espérance de vie moyenne (années)",
    color = "Continent"
  ) +
  theme_minimal()

# CORRECTION 2.3
gapminder |>
  filter(year == 2007) |>
  ggplot(aes(x = gdpPercap, fill = continent)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  scale_x_log10() +
  labs(
    title = "Distribution du PIB per capita par continent (2007)",
    x     = "PIB per capita (échelle log)",
    y     = "Nombre de pays",
    fill  = "Continent"
  ) +
  theme_minimal()


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

# Votre code ici :
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

# Votre code ici :
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


# CORRECTION 3.1
reponses |>
  mutate(
    nb_mots                = str_count(texte, "\\w+"),
    mentionne_gouvernement = str_detect(texte, "government")
  ) |>
  filter(mentionne_gouvernement)

# CORRECTION 3.2
mots <- reponses |>
  unnest_tokens(mot, texte)

data(stop_words)

mots_propres <- mots |>
  anti_join(stop_words, by = c("mot" = "word"))

mots_propres |>
  count(mot, sort = TRUE) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(mot, n), y = n)) +
  geom_col(fill = "#003875") +
  coord_flip() +
  labs(title = "10 mots les plus fréquents", x = "Mot", y = "Fréquence") +
  theme_minimal()

# CORRECTION 3.3
bing <- get_sentiments("bing")

mots_propres |>
  inner_join(bing, by = c("mot" = "word")) |>
  count(groupe, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(score_net = positive - negative) |>
  ggplot(aes(x = reorder(groupe, score_net), y = score_net, fill = score_net > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick", "#2ca25f")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Ton des réponses par groupe politique",
       x = "Groupe", y = "Score de sentiment net (positifs − négatifs)") +
  theme_minimal()


# ==============================================================================
# SECTION 3 : RÉGRESSION LINÉAIRE
# ==============================================================================

# ------ Exercice 3.1 ------
# Avec les données Gapminder pour 2007 :
# 1. Faites une régression linéaire : lifeExp ~ log(gdpPercap)
# 2. Affichez le résumé (summary)
# 3. Interprétez : l'effet est-il significatif? Positif ou négatif?

data_2007 <- gapminder |>
  filter(year == 2007) |>
  mutate(log_pib = log(gdpPercap))

# Modèle simple
modele_simple <- lm(lifeExp ~ log_pib, data = data_2007)
summary(modele_simple)

# Modèle multiple (avec contrôle pour la population)
modele_multiple <- lm(lifeExp ~ log_pib + log(pop), data = data_2007)
summary(modele_multiple)

# Visualiser la relation
data_2007 |>
  ggplot(aes(x = log_pib, y = lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    title    = "Régression : PIB per capita (log) → Espérance de vie",
    x        = "Log du PIB per capita",
    y        = "Espérance de vie (années)",
    color    = "Continent"
  ) +
  theme_minimal()

# Pour des tableaux de régression formatés :
# install.packages("modelsummary")
# library(modelsummary)
# modelsummary(list("Modèle simple" = modele_simple,
#                   "Modèle multiple" = modele_multiple))


# ==============================================================================
# FIN — Bonne continuation!
# ==============================================================================
# Pour des questions : adrien.cloutier.1@ulaval.ca
# Ressources : https://r4ds.hadley.nz/ | https://www.tidytextmining.com/
