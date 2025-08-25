# Analyse des Transferts Football ⚽
Application R Shiny interactive pour l'analyse des données de transferts des principales ligues européennes de football avec visualisations complètes et exports détaillés.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)

Retrouvez l'application en cliquant ici : [![Shiny App](https://img.shields.io/badge/Shiny-App%20Live-blue?style=flat-square&logo=RStudio)]( https://rakostats.shinyapps.io/Transferts_market_analysis/)

## ✨ Fonctionnalités

- **Analyse par ligue** : 7 championnats européens majeurs disponibles
- **Bilans financiers** : Visualisation des revenus vs dépenses par équipe
- **Activité de transfert** : Comparaison des volumes de transactions
- **Top transferts individuels** : Classements des arrivées et départs les plus chers
- **Résumé exécutif** : Statistiques globales en temps réel
- **Export complet** : Téléchargement d'archives ZIP avec graphiques haute qualité

## 🏆 Ligues supportées

- **Premier League** (Angleterre 🏴󐁧󐁢󐁥󐁮󐁧󐁿)
- **Ligue 1** (France 🇫🇷) 
- **Bundesliga** (Allemagne 🇩🇪)
- **Serie A** (Italie 🇮🇹)
- **La Liga** (Espagne 🇪🇸)
- **Primeira Liga** (Portugal 🇵🇹)
- **Eredivisie** (Pays-Bas 🇳🇱)

## 🚀 Version locale

### Prérequis
- R version 4.0.0 ou supérieure
- RStudio (recommandé)

### Packages requis
```r
install.packages(c(
  "shiny", "shinydashboard", "worldfootballR", 
  "dplyr", "ggplot2", "scales", "stringr", 
  "plotly", "DT", "cowplot", "shinycssloaders", 
  "zip", "htmlwidgets"
))
```

### Installation et lancement
```r
# Télécharger le fichier app.R depuis GitHub
# Puis dans RStudio :
shiny::runApp("app.R")
```

## 📊 Aperçu des analyses

### 💰 Bilan Net des Transferts
Comparaison revenus/dépenses avec identification des équipes en excédent ou déficit

### 📈 Activité de Transfert  
Volume des transactions par équipe pour mesurer l'activité sur le marché

### ⚽ Top Transferts Individuels
- **Arrivées** : Les 10 transferts entrants les plus chers
- **Départs** : Les 10 transferts sortants les plus chers

## 📥 Export des données

L'application génère une archive ZIP complète contenant :
- Graphiques haute résolution (PNG 300 DPI)
- Résumé textuel détaillé avec statistiques
- Classements des équipes par bilan financier



## 📅 Données disponibles

Saisons couvertes : **2018/19** à **2024/25**

Source des données : Package `worldfootballR` (Transfermarkt)

---
