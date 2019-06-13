# introduction aux graphiques avec ggplot2
# charger et attacher le package ggplot2

library(ggplot2)

ggplot()
# il faut des donnees
ggplot(data = ToothGrowth)

# il faut aussi expliciter comment on veut les représenter
# le systeme d'axe : aestetic
ggplot(data = ToothGrowth,
       mapping = aes(x = len))

# il faut specifier comment representer l'information
# dans ce systeme d'axe : les geom_
ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_histogram()

help("geom_histogram")

ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_histogram(binwidth = 5)

ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_histogram(binwidth = 5) +
  facet_grid(dose ~ supp)

# boites à moustaches
# version minimale
ggplot(data = ToothGrowth,
       mapping = aes(y = len, x = supp)) +
  geom_boxplot()

# personnalisation
ggplot(data = ToothGrowth,
       mapping = aes(y = len, x = supp)) +
  geom_boxplot(color = "purple",
               fill = "orange",
               alpha = .4) +
  labs(title =" Longueur des dents",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)")

# comment faire pour avoir la dose en abscisse ?
# utiliser factor !
ggplot(data = ToothGrowth,
       mapping = aes(y = len, x = factor(dose))) +
  geom_boxplot(color = "purple",
               fill = "orange",
               alpha = .4) +
  labs(title =" Longueur des dents",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)")

# on aimerait bien avoir à la fois la dose et
# le mode de supplémentation
# codage en couleur de remplissage
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp)) +
  geom_boxplot(alpha = .4) +
  labs(title =" Longueur des dents",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)")

# la notion d'héritabilité des propriétés
# et de hiérarchie
# codage en couleur de remplissage
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp)) +
  geom_boxplot(fill = "orange",
               alpha = .4) +
  labs(title =" Longueur des dents",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)")
# ici le fill au niveau du geom va à l'encontre
# du fill défini dans l'esthétique au niveau
# de ggplot

# illustration
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp)) +
  geom_boxplot()
# est identique à
ggplot() +
  geom_boxplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp))

#
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     color = supp)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_point(position = position_dodge(1))

# graphe perfectionné
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose))) +
  geom_violin(mapping = aes(fill = supp),
              draw_quantiles = c(0.25, 0.5, 0.75),
              position = position_dodge(1),
              alpha = .4) +
  geom_point(mapping = aes(group = supp),
    position = position_dodge(1),
    shape = 1) +
  scale_fill_manual(name = "supplementation :",
    values = c("orange","blue"),
                    labels = c("jus d'orange",
                               "acide ascorbique")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 35,
                                  by = 5)) +
  labs(title =" Longueur des dents et vitamine C",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5,
                                  size = 16))

# le graphe classique d'un design à deux facteurs

# Effectivement, on peut reprendre le tableau précédemment construit avec aggregate
# fonction pour les moyennes et ecarts-types
moyenne_ecarttype <- function(x) {
  c(moyenne = mean(x),
    ecart_type = sd(x))
}

Toothgrowth_description <- aggregate(formula = len ~ supp + dose,
                                     data = ToothGrowth,
                                     FUN = moyenne_ecarttype)

print(Toothgrowth_description)

# attention à la structure de ce dataframe !
str(Toothgrowth_description)
# en fait len.moyenne et len.ecarttype sont deux
# colonnes de len qui est devenue une matrice.
# on y a accès par exemple avec leurs indices

ggplot(data = Toothgrowth_description,
       mapping = aes(x = dose,
                     color = supp)) +
  geom_point(mapping = aes(y = len[,1],
                           shape = supp),
             position = position_dodge(.5)) +
  geom_errorbar(mapping = aes(ymin = len[,1] - len[,2],
                              ymax = len[,1] + len[,2]),
                position = position_dodge(.5),
                width = .3)

# comme toujours, en R, c'est faisable, mais c'est moche
# il y a donc heureusement moyen de faire plus simple

# version avec la moyenne et les ecarts types
TG_moyenne = aggregate(len ~ supp + dose, data = ToothGrowth, FUN = mean)
TG_ecart_type = aggregate(len ~ supp + dose, data = ToothGrowth, FUN = sd)

TG_description = data.frame(supp = c("OJ","VC","OJ","VC","OJ","VC"),
           dose = c(.5, .5, 1, 1, 2, 2),
           moyenne = TG_moyenne$len,
           ecart_type = TG_ecart_type$len)
str(TG_description)

# version presque aussi moche
TG_conditions = data.frame(supp = c("OJ","VC","OJ","VC","OJ","VC"),
           dose = c(.5, .5, 1, 1, 2, 2))
TG_description = cbind(TG_conditions,
                       moyenne = TG_moyenne$len,
                       ecart_type = Tg_ecart_type$len)
str(TG_description)

# pour ceux que ça intéresse de voir une version plus élégante, voir help(gl)

# version plus subtile avec dplyr
library(dplyr)
TG = left_join(TG_moyenne, TG_ecart_type, by = c("dose", "supp"))
str(TG)

ToothGrowth %>%
  group_by(dose, supp) %>%
  summarise(moyenne = mean(len),
            ecart_type = sd(len))

ToothGrowth %>%
  group_by(dose, supp) %>%
  summarise(moyenne = mean(len),
            ecart_type = sd(len)) %>%
ggplot(mapping = aes(x = dose,
                     color = supp)) +
  geom_point(mapping = aes(y = moyenne),
             position = position_dodge(.5)) +
  geom_errorbar(mapping = aes(ymin = moyenne - ecart_type,
                              ymax = moyenne + ecart_type),
                position = position_dodge(.5))

####################################################

# deuxieme jour
library("ggplot2")
data(ToothGrowth)

# facet_grid
ggplot(data = ToothGrowth,
       mapping = aes(y = len, x = supp)) +
  geom_boxplot() +
  facet_grid(dose ~ .)
# c'est pas très lisible

# autre méthode
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp)) +
  geom_boxplot()

# personnalisation des couleurs
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = supp)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "yellow"))

# pour re-ordonner les niveaux de notre facteur suppl
help("relevel")
help("factor")

ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose),
                     fill = relevel(supp, ref = "VC"))) +
  geom_boxplot() +
  scale_fill_manual(values = c("yellow", "orange"))

# doses décroissantes
# 1. regarder le nombre de niveaux du facteur
nlevels(factor(ToothGrowth$dose))
# 2. identifier les niveaux du facteur
levels(factor(ToothGrowth$dose))
# 3. décider des niveaux à considérer et des étiquettes associees
factor(ToothGrowth$dose,
       levels = c("2", "1", "0.5"),
       labels = c("forte", "moyenne", "faible"))
# on peut considérer plus de niveaux que ceux apparaissant dans les données
factor(ToothGrowth$dose,
       levels = c("2", "1.5", "1", "0.5"),
       labels = c("forte", "elevée", "moyenne", "faible"))

ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose,
                                levels = c("2", "1", "0.5"),
                                labels = c("forte", "moyenne", "faible")),
                     fill = relevel(supp, ref = "VC"))) +
  geom_boxplot() +
  scale_fill_manual(values = c("yellow", "orange"))

# graphe perfectionné
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose))) +
  geom_violin(mapping = aes(fill = supp),
              draw_quantiles = c(0.25, 0.5, 0.75),
              position = position_dodge(1),
              alpha = .4) +
  geom_point(mapping = aes(group = supp),
             position = position_dodge(1),
             shape = 1) +
  scale_fill_manual(name = "supplementation :",
                    values = c("orange","blue"),
                    labels = c("jus d'orange",
                               "acide ascorbique")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 35,
                                  by = 5)) +
  labs(title =" Longueur des dents et vitamine C",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5,
                                  size = 16))

ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = factor(dose))) +
  geom_violin(mapping = aes(fill = supp),
              position = position_dodge(1),
              alpha = .4,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_point(mapping = aes(group = supp),
             position = position_dodge(1),
             shape = 1) +
  scale_fill_manual(name = "supplementation :",
                  values = c("orange","blue"),
                  labels = c("jus d'orange",
                             "acide ascorbique")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 35,
                                  by = 5)) +
  labs(title =" Longueur des dents et vitamine C",
       x = "Dose (mg/jour)",
       y = "Longueur (mm)") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5,
                                  size = 16))

####################################################

# visualisation
library("ggplot2")

# ggplot : la base
ggplot()
# ça crée une fenêtre graphique
help(ggplot)

# il faut des données
ggplot(data = ToothGrowth)

# et une esthétique
# en première approximation c'est le système d'axes
ggplot(data = ToothGrowth,
       mapping = aes(x = len))

# on choisit le mode de représentation de l'information : geom
ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_histogram()
# et voilà !

# pour la courbe de densité
ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_density()

# les deux !
# ah, il y a un problème d'échelle

# la solution : normaliser la densité sur le nombre d'observations
ggplot(data = ToothGrowth,
       mapping = aes(y = ..count..,
                     x = len)) +
  geom_histogram() +
  geom_density()

# pour faire plus joli
ggplot(data = ToothGrowth,
       mapping = aes(y = ..count..,
                     x = len)) +
  geom_histogram(color = "deepskyblue4",
                 fill = "lightblue",
                 alpha = .5) +
  geom_density(color = "purple")

summary(ToothGrowth)
# pour avoir une information selon les conditions expérimentales

# une représentation graphique possible, le scatterplot : geom_point
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = dose)) +
  geom_point()

# pour éviter les points supperposés : geom_jitter
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = dose)) +
  geom_jitter(width = .1)

# héritabilité et hiérarchie
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = supp)) +
  geom_point()

ggplot() +
  geom_point(data = ToothGrowth,
              mapping = aes(y = len,
                            x = supp))

# Comment représenter l'information sur les doses et la supplémentation ?
# 1ère suggestion : points de taille proportionnelle à la dose
# c'est possible et c'est facile : on joue sur la propriété size
# il faut la définir en mapping = aes() car il y a un lien entre une
# propriété d'un geom et une variable du dataframe
ggplot(data = ToothGrowth,
       mapping = aes(y = len,
                     x = supp)) +
  geom_jitter(mapping = aes(size = dose),
              width = .2,
              alpha = .3,
              shape = 21,
              color = "black",
              fill = "lightblue")

# revenons aux densités
ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_density()

# densités selon la supplémentation
ggplot(data = ToothGrowth,
       mapping = aes(x = len,
                     fill = supp)) +
  geom_density(alpha = .4)

# densités selon la dose
ggplot(data = ToothGrowth,
       mapping = aes(x = len,
                     fill = factor(dose))) +
  geom_density(alpha = .4)

# les deux facteurs !
# densités avec supplémentation en couleur
# et sous-graphes selon la dose
ggplot(data = ToothGrowth,
       mapping = aes(x = len,
                     fill = supp)) +
  geom_density(alpha = .4) +
  facet_grid(dose ~ .)

ggplot(data = ToothGrowth,
       mapping = aes(x = len)) +
  geom_density(alpha = .4) +
  facet_grid(dose ~ supp)


####################################################
