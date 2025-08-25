# =============================================================================
# APPLICATION RSHINY - ANALYSE DES TRANSFERTS
# =============================================================================

# Chargement des packages
library(shiny)
library(shinydashboard)
library(worldfootballR)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(plotly)
library(DT)
library(cowplot)
library(shinycssloaders)
library(zip)  # Pour créer l'archive ZIP
library(htmlwidgets)  # Pour sauvegarder les graphiques plotly

# =============================================================================
# INTERFACE UTILISATEUR (UI)
# =============================================================================

ui <- dashboardPage(
  skin = "black",
  
  # En-tête
  dashboardHeader(
    title = "⚽ Analyse des Transferts Football",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📊 Analyse des Transferts", tabName = "transferts", icon = icon("chart-bar")),
      
      # Contrôles
      br(),
      h4("🎯 Paramètres", style = "color: white; text-align: center;"),
      
      # Sélection du pays/ligue
      selectInput(
        "country",
        "🌍 Pays:",
        choices = list(
          "Allemagne (Bundesliga)" = "Germany",
          "Angleterre (Premier League)" = "England", 
          "Espagne (La Liga)" = "Spain",
          "France (Ligue 1)" = "France",
          "Italie (Serie A)" = "Italy",
          "Portugal (Primeira Liga)" = "Portugal",
          "Pays-Bas (Eredivisie)" = "Netherlands"
        ),
        selected = "Germany"
      ),
      
      # Sélection de la saison
      selectInput(
        "season",
        "📅 Saison:",
        choices = list(
          "2024/25" = 2024,
          "2023/24" = 2023,
          "2022/23" = 2022,
          "2021/22" = 2021,
          "2020/21" = 2020,
          "2019/20" = 2019,
          "2018/19" = 2018
        ),
        selected = 2020
      ),
      
      # Bouton d'analyse
      br(),
      actionButton(
        "analyze",
        "🚀 Lancer l'Analyse",
        class = "btn-primary btn-lg",
        style = "width: 100%; margin-bottom: 20px;"
      ),
      
      # Bouton de téléchargement des analyses
      br(),
      downloadButton(
        "download_plots",
        "📊 Télécharger les Analyses",
        class = "btn-warning btn-lg",
        style = "width: 100%;"
      )
    )
  ),
  
  # Corps principal
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #1a1a1a;
        }
        .box {
          background-color: #2c2c2c;
          border: 1px solid #444;
        }
        .box-header {
          color: white;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #2c2c2c;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # Onglet principal - Analyse
      tabItem(
        tabName = "transferts",
        
        # Boîte d'information
        fluidRow(
          box(
            title = "📈 Résumé Exécutif",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 120,
            withSpinner(
              verbatimTextOutput("summary_stats"),
              color = "#3c8dbc"
            )
          )
        ),
        
        # Graphiques principaux
        fluidRow(
          # Bilan net par équipe
          box(
            title = "💰 Bilan Net des Transferts",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = 500,
            withSpinner(
              plotlyOutput("plot_balance", height = "400px"),
              color = "#3c8dbc"
            )
          ),
          
          # Activité par équipe
          box(
            title = "📊 Activité de Transfert",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = 500,
            withSpinner(
              plotlyOutput("plot_activity", height = "400px"),
              color = "#00a65a"
            )
          )
        ),
        
        # Transferts individuels
        fluidRow(
          box(
            title = "⚽ Top Transferts Individuels",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            height = 600,
            withSpinner(
              plotOutput("plot_transfers", height = "500px"),
              color = "#f39c12"
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVEUR (SERVER)
# =============================================================================

server <- function(input, output, session) {
  
  # Variables réactives pour stocker les données
  team_balances <- reactiveVal()
  player_transfers <- reactiveVal()
  transfers_clean <- reactiveVal()
  
  # Variables réactives pour stocker les graphiques
  plot_balance_obj <- reactiveVal()
  plot_activity_obj <- reactiveVal()
  plot_transfers_obj <- reactiveVal()
  
  # Fonction pour récupérer les données
  get_transfer_data <- function(country, season) {
    
    # Affichage du pays sélectionné
    country_name <- names(which(c(
      "Germany" = "Allemagne (Bundesliga)",
      "England" = "Angleterre (Premier League)", 
      "Spain" = "Espagne (La Liga)",
      "France" = "France (Ligue 1)",
      "Italy" = "Italie (Serie A)",
      "Portugal" = "Portugal (Primeira Liga)",
      "Netherlands" = "Pays-Bas (Eredivisie)"
    ) == input$country))
    
    showNotification(
      paste("🔄 Récupération des données pour", country_name, "saison", season, "/", season + 1),
      type = "default",
      duration = 5
    )
    
    # 1. Récupération des bilans d'équipes
    tryCatch({
      balances <- tm_team_transfer_balances(
        country_name = country,
        start_year = season
      )
      
      if(nrow(balances) == 0) {
        showNotification("❌ Aucune donnée de bilan trouvée", type = "error")
        return(NULL)
      }
      
      team_balances(balances)
      
      showNotification("✅ Bilans d'équipes récupérés", type = "default")
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur bilans:", e$message), type = "error")
      return(NULL)
    })
    
    # 2. Récupération des URLs d'équipes
    tryCatch({
      team_urls <- tm_league_team_urls(
        country_name = country,
        start_year = season
      )
      
      if(length(team_urls) == 0) {
        showNotification("⚠️ URLs d'équipes non disponibles", type = "warning")
        return(balances)
      }
      
      # Récupération de toutes les équipes
      selected_urls <- team_urls  # Toutes les équipes au lieu de head(team_urls, 10)
      
      showNotification(
        paste("🔄 Récupération des transferts pour toutes les équipes (", length(selected_urls), ")..."),
        type = "default",
        duration = 15
      )
      
      # 3. Récupération des transferts individuels
      all_transfers <- list()
      
      for(i in seq_along(selected_urls)) {
        tryCatch({
          team_url <- selected_urls[i]
          
          # Extraction du nom d'équipe
          team_name <- str_extract(team_url, "(?<=/)[^/]+(?=/startseite)")
          team_name <- str_replace_all(team_name, "-", " ")
          team_name <- str_to_title(team_name)
          
          # Récupération des transferts
          transfers <- tm_team_transfers(
            team_url = team_url,
            transfer_window = "all"
          )
          
          if(!is.null(transfers) && nrow(transfers) > 0) {
            transfers$team_name <- team_name
            transfers$team_url <- team_url
            all_transfers[[i]] <- transfers
          }
          
          # Pause plus courte mais avec gestion progressive
          if(i %% 5 == 0) {
            showNotification(
              paste("📊 Progression:", i, "/", length(selected_urls), "équipes traitées"),
              type = "default",
              duration = 2
            )
          }
          
          Sys.sleep(1)  # Pause réduite pour toutes les équipes
          
        }, error = function(e) {
          # Continue silencieusement en cas d'erreur
        })
      }
      
      # Combinaison des transferts
      if(length(all_transfers) > 0) {
        combined_transfers <- bind_rows(all_transfers)
        player_transfers(combined_transfers)
        
        showNotification(
          paste("✅ Analyse terminée:", nrow(combined_transfers), "transferts pour", length(all_transfers), "équipes"),
          type = "default"
        )
      } else {
        showNotification("⚠️ Aucun transfert récupéré - Vérifiez la disponibilité des données", type = "warning")
        player_transfers(data.frame())
      }
      
    }, error = function(e) {
      showNotification("⚠️ Transferts individuels non disponibles", type = "warning")
      player_transfers(data.frame())
    })
    
    return(TRUE)
  }
  
  # Observer pour le bouton d'analyse
  observeEvent(input$analyze, {
    
    result <- get_transfer_data(input$country, as.numeric(input$season))
    
    if(!is.null(result)) {
      showNotification("🎉 Analyse terminée avec succès!", type = "default", duration = 3)
    }
  })
  
  # Préparation des données pour les graphiques
  prepared_data <- reactive({
    req(team_balances())
    
    # Analyse des bilans
    team_analysis <- team_balances() %>%
      mutate(
        net_transfer_income = income_euros - expenditure_euros,
        made_money = net_transfer_income > 0,
        activity_level = case_when(
          abs(net_transfer_income) > 50e6 ~ "Très actif",
          abs(net_transfer_income) > 20e6 ~ "Actif",
          abs(net_transfer_income) > 5e6 ~ "Modéré",
          TRUE ~ "Peu actif"
        )
      ) %>%
      arrange(desc(net_transfer_income))
    
    # Préparation des transferts individuels
    if(!is.null(player_transfers()) && nrow(player_transfers()) > 0) {
      transfers_data <- player_transfers() %>%
        filter(!is.na(transfer_fee) & is.numeric(transfer_fee) & transfer_fee > 0) %>%
        mutate(
          transfer_fee_millions = transfer_fee / 1e6,
          player_name = ifelse(is.na(player_name), "Joueur Inconnu", player_name)
        )
      
      # Ajout conditionnel des colonnes
      if(!"player_age" %in% names(transfers_data)) {
        transfers_data$player_age <- 25
      }
      if(!"player_position" %in% names(transfers_data)) {
        transfers_data$player_position <- "Non spécifié"
      }
      
      transfers_clean(transfers_data)
    } else {
      transfers_clean(data.frame())
    }
    
    return(team_analysis)
  })
  
  # Résumé statistique
  output$summary_stats <- renderText({
    req(team_balances())
    
    balances <- team_balances()
    total_spent <- sum(balances$expenditure_euros, na.rm = TRUE)
    total_income <- sum(balances$income_euros, na.rm = TRUE)
    net_balance <- total_income - total_spent
    
    transfers_count <- ifelse(!is.null(transfers_clean()) && nrow(transfers_clean()) > 0, nrow(transfers_clean()), 0)
    
    paste0(
      "🏟️ Équipes analysées: ", nrow(balances), " | ",
      "💰 Dépenses totales: €", round(total_spent/1e6, 1), "M | ",
      "💰 Revenus totaux: €", round(total_income/1e6, 1), "M | ",
      "📊 Bilan net: €", round(net_balance/1e6, 1), "M | ",
      "⚽ Transferts individuels: ", transfers_count
    )
  })
  
  # Fonction pour créer le graphique du bilan net (version statique pour téléchargement)
  create_balance_plot <- function() {
    req(prepared_data())
    
    team_analysis <- prepared_data()
    
    team_analysis %>%
      mutate(
        squad = reorder(squad, net_transfer_income),
        bilan_label = ifelse(made_money, "Excédent", "Déficit")
      ) %>%
      ggplot(aes(x = net_transfer_income/1e6, y = squad, fill = bilan_label)) +
      geom_col(width = 0.8, alpha = 0.8) +
      scale_fill_manual(
        values = c("Déficit" = "#DC143C", "Excédent" = "#228B22"),
        name = "Bilan"
      ) +
      scale_x_continuous(
        labels = function(x) paste0("€", x, "M"),
        name = "Revenus nets (millions €)"
      ) +
      labs(
        title = paste("Bilan Net par Équipe -", names(which(c(
          "Germany" = "Bundesliga",
          "England" = "Premier League", 
          "Spain" = "La Liga",
          "France" = "Ligue 1",
          "Italy" = "Serie A",
          "Portugal" = "Primeira Liga",
          "Netherlands" = "Eredivisie"
        ) == input$country)), input$season, "/", as.numeric(input$season) + 1),
        y = "Équipes"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
      )
  }
  
  # Fonction pour créer le graphique d'activité (version statique pour téléchargement)
  create_activity_plot <- function() {
    req(prepared_data())
    
    team_analysis <- prepared_data()
    
    activity_data <- team_analysis %>%
      select(squad, expenditure_euros, income_euros) %>%
      tidyr::pivot_longer(cols = c(expenditure_euros, income_euros),
                          names_to = "type", values_to = "amount") %>%
      mutate(
        type = case_when(
          type == "expenditure_euros" ~ "Dépenses",
          type == "income_euros" ~ "Revenus"
        ),
        amount_millions = amount / 1e6
      ) %>%
      filter(amount > 1e6) %>%
      arrange(desc(amount)) %>%
      head(20)
    
    activity_data %>%
      ggplot(aes(x = amount_millions, y = reorder(squad, amount), fill = type)) +
      geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      scale_fill_manual(
        values = c("Dépenses" = "#FF6B35", "Revenus" = "#4ECDC4"),
        name = "Type"
      ) +
      scale_x_continuous(
        labels = function(x) paste0("€", x, "M")
      ) +
      labs(
        title = paste("Activité de Transfert -", names(which(c(
          "Germany" = "Bundesliga",
          "England" = "Premier League", 
          "Spain" = "La Liga",
          "France" = "Ligue 1",
          "Italy" = "Serie A",
          "Portugal" = "Primeira Liga",
          "Netherlands" = "Eredivisie"
        ) == input$country)), input$season, "/", as.numeric(input$season) + 1),
        x = "Montant (millions €)",
        y = "Équipes"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
      )
  }
  
  # Graphique du bilan net (version interactive)
  output$plot_balance <- renderPlotly({
    req(prepared_data())
    
    team_analysis <- prepared_data()
    
    p <- team_analysis %>%
      mutate(
        squad = reorder(squad, net_transfer_income),
        bilan_label = ifelse(made_money, "Excédent", "Déficit")
      ) %>%
      ggplot(aes(x = net_transfer_income/1e6, y = squad, fill = bilan_label,
                 text = paste0(squad, "\nBilan: €", round(net_transfer_income/1e6, 1), "M\nType: ", bilan_label))) +
      geom_col(width = 0.8, alpha = 0.8) +
      scale_fill_manual(
        values = c("Déficit" = "#DC143C", "Excédent" = "#228B22"),
        name = "Bilan"
      ) +
      scale_x_continuous(
        labels = function(x) paste0("€", x, "M"),
        name = "Revenus nets (millions €)"
      ) +
      labs(
        title = "Bilan Net par Équipe",
        y = "Équipes"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(color = "white")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = "white")
      )
  })
  
  # Graphique d'activité (version interactive)
  output$plot_activity <- renderPlotly({
    req(prepared_data())
    
    team_analysis <- prepared_data()
    
    activity_data <- team_analysis %>%
      select(squad, expenditure_euros, income_euros) %>%
      tidyr::pivot_longer(cols = c(expenditure_euros, income_euros),
                          names_to = "type", values_to = "amount") %>%
      mutate(
        type = case_when(
          type == "expenditure_euros" ~ "Dépenses",
          type == "income_euros" ~ "Revenus"
        ),
        amount_millions = amount / 1e6
      ) %>%
      filter(amount > 1e6) %>%
      arrange(desc(amount)) %>%
      head(20)
    
    p <- activity_data %>%
      ggplot(aes(x = amount_millions, y = reorder(squad, amount), fill = type,
                 text = paste0(squad, "\n", type, ": €", round(amount_millions, 1), "M"))) +
      geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      scale_fill_manual(
        values = c("Dépenses" = "#FF6B35", "Revenus" = "#4ECDC4"),
        name = "Type"
      ) +
      scale_x_continuous(
        labels = function(x) paste0("€", x, "M")
      ) +
      labs(
        title = "Activité de Transfert",
        x = "Montant (millions €)",
        y = "Équipes"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(color = "white")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = "white")
      )
  })
  
  # Graphique des transferts individuels
  output$plot_transfers <- renderPlot({
    req(transfers_clean())
    
    plot_obj <- create_transfers_plot()
    plot_transfers_obj(plot_obj)  # Sauvegarder pour téléchargement
    
    return(plot_obj)
  }, bg = "black")
  
  # Fonction pour créer le graphique des transferts individuels
  create_transfers_plot <- function() {
    req(transfers_clean())
    
    if(nrow(transfers_clean()) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée de transfert individuel disponible", 
                   size = 16, color = "white") +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black")
          )
      )
    }
    
    transfers_data <- transfers_clean()
    
    # Séparation des arrivées et départs
    top_arrivals <- transfers_data %>%
      filter(str_detect(tolower(transfer_type), "arrival|in") | 
               (!str_detect(tolower(transfer_type), "departure|out") & !is.na(transfer_fee))) %>%
      arrange(desc(transfer_fee)) %>%
      head(10) %>%
      mutate(
        transfer_display = case_when(
          "club_2" %in% names(.) & !is.na(club_2) & club_2 != "" ~ 
            paste0(player_name, " : ", club_2, " → ", team_name),
          TRUE ~ paste0(player_name, " : ", team_name)
        )
      )
    
    top_departures <- transfers_data %>%
      filter(str_detect(tolower(transfer_type), "departure|out")) %>%
      arrange(desc(transfer_fee)) %>%
      head(10) %>%
      mutate(
        transfer_display = case_when(
          "club_2" %in% names(.) & !is.na(club_2) & club_2 != "" ~ 
            paste0(player_name, " : ", team_name, " → ", club_2),
          TRUE ~ paste0(player_name, " : ", team_name)
        )
      )
    
    # Graphique des arrivées
    arrivals_plot <- if(nrow(top_arrivals) > 0) {
      top_arrivals %>%
        ggplot(aes(x = transfer_fee_millions, y = reorder(transfer_display, transfer_fee))) +
        geom_col(fill = "#4ECDC4", alpha = 0.8) +
        geom_text(
          aes(label = paste0("€", round(transfer_fee_millions, 1), "M")),
          hjust = -0.1, color = "white", size = 3.2, fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0("€", x, "M"),
          expand = expansion(mult = c(0, 0.35))
        ) +
        labs(title = "TOP 10 ARRIVÉES", x = "Montant (millions €)", y = "") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          panel.grid.major.x = element_line(colour = "grey30", linetype = "dotted"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 14, colour = "#4ECDC4", face = "bold", hjust = 0.5),
          axis.title = element_text(colour = "white", size = 10),
          axis.text = element_text(colour = "white", size = 8.5),
          plot.margin = margin(10, 15, 10, 10)
        )
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucune arrivée", size = 12, color = "#4ECDC4") +
        labs(title = "TOP 10 ARRIVÉES") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "black"),
          plot.title = element_text(size = 14, colour = "#4ECDC4", face = "bold", hjust = 0.5)
        )
    }
    
    # Graphique des départs
    departures_plot <- if(nrow(top_departures) > 0) {
      top_departures %>%
        ggplot(aes(x = transfer_fee_millions, y = reorder(transfer_display, transfer_fee))) +
        geom_col(fill = "#FF6B35", alpha = 0.8) +
        geom_text(
          aes(label = paste0("€", round(transfer_fee_millions, 1), "M")),
          hjust = -0.1, color = "white", size = 3.2, fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0("€", x, "M"),
          expand = expansion(mult = c(0, 0.35))
        ) +
        labs(title = "TOP 10 DÉPARTS", x = "Montant (millions €)", y = "") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          panel.grid.major.x = element_line(colour = "grey30", linetype = "dotted"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 14, colour = "#FF6B35", face = "bold", hjust = 0.5),
          axis.title = element_text(colour = "white", size = 10),
          axis.text = element_text(colour = "white", size = 8.5),
          plot.margin = margin(10, 15, 10, 10)
        )
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucun départ", size = 12, color = "#FF6B35") +
        labs(title = "TOP 10 DÉPARTS") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "black"),
          plot.title = element_text(size = 14, colour = "#FF6B35", face = "bold", hjust = 0.5)
        )
    }
    
    # Titre principal
    title_plot <- ggplot() + 
      labs(title = paste("TRANSFERTS INDIVIDUELS -", toupper(names(which(c(
        "Germany" = "BUNDESLIGA",
        "England" = "PREMIER LEAGUE", 
        "Spain" = "LA LIGA",
        "France" = "LIGUE 1",
        "Italy" = "SERIE A",
        "Portugal" = "PRIMEIRA LIGA",
        "Netherlands" = "EREDIVISIE"
      ) == input$country))), input$season, "/", as.numeric(input$season) + 1)) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16, colour = "white", face = "bold", hjust = 0.5)
      )
    
    # Sous-titre
    subtitle_plot <- ggplot() + 
      labs(subtitle = "Format : Joueur : Club origine → Club destination") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "black"),
        plot.subtitle = element_text(size = 11, colour = "white", hjust = 0.5)
      )
    
    # Assemblage final
    plot_grid(
      title_plot,
      subtitle_plot,
      plot_grid(arrivals_plot, departures_plot, ncol = 2, rel_widths = c(1, 1)),
      ncol = 1,
      rel_heights = c(0.15, 0.05, 0.8)
    )
  }
  
  # Fonction pour créer le graphique des transferts individuels (version pour téléchargement)
  create_transfers_plot_download <- function() {
    req(transfers_clean())
    
    if(nrow(transfers_clean()) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée de transfert individuel disponible", 
                   size = 16, color = "black") +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")
          )
      )
    }
    
    transfers_data <- transfers_clean()
    
    # Séparation des arrivées et départs
    top_arrivals <- transfers_data %>%
      filter(str_detect(tolower(transfer_type), "arrival|in") | 
               (!str_detect(tolower(transfer_type), "departure|out") & !is.na(transfer_fee))) %>%
      arrange(desc(transfer_fee)) %>%
      head(10) %>%
      mutate(
        transfer_display = case_when(
          "club_2" %in% names(.) & !is.na(club_2) & club_2 != "" ~ 
            paste0(player_name, " : ", club_2, " → ", team_name),
          TRUE ~ paste0(player_name, " : ", team_name)
        )
      )
    
    top_departures <- transfers_data %>%
      filter(str_detect(tolower(transfer_type), "departure|out")) %>%
      arrange(desc(transfer_fee)) %>%
      head(10) %>%
      mutate(
        transfer_display = case_when(
          "club_2" %in% names(.) & !is.na(club_2) & club_2 != "" ~ 
            paste0(player_name, " : ", team_name, " → ", club_2),
          TRUE ~ paste0(player_name, " : ", team_name)
        )
      )
    
    # Graphique des arrivées (version download)
    arrivals_plot <- if(nrow(top_arrivals) > 0) {
      top_arrivals %>%
        ggplot(aes(x = transfer_fee_millions, y = reorder(transfer_display, transfer_fee))) +
        geom_col(fill = "#4ECDC4", alpha = 0.8) +
        geom_text(
          aes(label = paste0("€", round(transfer_fee_millions, 1), "M")),
          hjust = -0.1, color = "black", size = 3.2, fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0("€", x, "M"),
          expand = expansion(mult = c(0, 0.35))
        ) +
        labs(title = "TOP 10 ARRIVÉES", x = "Montant (millions €)", y = "") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(colour = "grey70", linetype = "dotted"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 14, colour = "#4ECDC4", face = "bold", hjust = 0.5),
          axis.title = element_text(colour = "black", size = 10),
          axis.text = element_text(colour = "black", size = 8.5),
          plot.margin = margin(10, 15, 10, 10)
        )
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucune arrivée", size = 12, color = "#4ECDC4") +
        labs(title = "TOP 10 ARRIVÉES") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = 14, colour = "#4ECDC4", face = "bold", hjust = 0.5)
        )
    }
    
    # Graphique des départs (version download)
    departures_plot <- if(nrow(top_departures) > 0) {
      top_departures %>%
        ggplot(aes(x = transfer_fee_millions, y = reorder(transfer_display, transfer_fee))) +
        geom_col(fill = "#FF6B35", alpha = 0.8) +
        geom_text(
          aes(label = paste0("€", round(transfer_fee_millions, 1), "M")),
          hjust = -0.1, color = "black", size = 3.2, fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0("€", x, "M"),
          expand = expansion(mult = c(0, 0.35))
        ) +
        labs(title = "TOP 10 DÉPARTS", x = "Montant (millions €)", y = "") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(colour = "grey70", linetype = "dotted"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 14, colour = "#FF6B35", face = "bold", hjust = 0.5),
          axis.title = element_text(colour = "black", size = 10),
          axis.text = element_text(colour = "black", size = 8.5),
          plot.margin = margin(10, 15, 10, 10)
        )
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Aucun départ", size = 12, color = "#FF6B35") +
        labs(title = "TOP 10 DÉPARTS") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = 14, colour = "#FF6B35", face = "bold", hjust = 0.5)
        )
    }
    
    # Titre principal (version download)
    title_plot <- ggplot() + 
      labs(title = paste("TRANSFERTS INDIVIDUELS -", toupper(names(which(c(
        "Germany" = "BUNDESLIGA",
        "England" = "PREMIER LEAGUE", 
        "Spain" = "LA LIGA",
        "France" = "LIGUE 1",
        "Italy" = "SERIE A",
        "Portugal" = "PRIMEIRA LIGA",
        "Netherlands" = "EREDIVISIE"
      ) == input$country))), input$season, "/", as.numeric(input$season) + 1)) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust = 0.5)
      )
    
    # Sous-titre (version download)
    subtitle_plot <- ggplot() + 
      labs(subtitle = "Format : Joueur : Club origine → Club destination") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 11, colour = "black", hjust = 0.5)
      )
    
    # Assemblage final
    plot_grid(
      title_plot,
      subtitle_plot,
      plot_grid(arrivals_plot, departures_plot, ncol = 2, rel_widths = c(1, 1)),
      ncol = 1,
      rel_heights = c(0.15, 0.05, 0.8)
    )
  }
  
  # Téléchargement de tous les graphiques
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("analyses_transferts_", input$country, "_", input$season, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Vérifier que les données sont disponibles
      req(team_balances())
      
      # Créer un dossier temporaire
      temp_dir <- tempdir()
      
      tryCatch({
        showNotification("📊 Génération des graphiques en cours...", type = "default", duration = 5)
        
        # 1. Graphique du bilan net
        balance_plot <- create_balance_plot()
        balance_filename <- file.path(temp_dir, paste0("bilan_net_", input$country, "_", input$season, ".png"))
        ggsave(balance_filename, plot = balance_plot, width = 12, height = 8, dpi = 300, bg = "white")
        
        # 2. Graphique d'activité
        activity_plot <- create_activity_plot()
        activity_filename <- file.path(temp_dir, paste0("activite_transferts_", input$country, "_", input$season, ".png"))
        ggsave(activity_filename, plot = activity_plot, width = 12, height = 8, dpi = 300, bg = "white")
        
        # 3. Graphique des transferts individuels
        if(!is.null(transfers_clean()) && nrow(transfers_clean()) > 0) {
          transfers_plot <- create_transfers_plot_download()
          transfers_filename <- file.path(temp_dir, paste0("transferts_individuels_", input$country, "_", input$season, ".png"))
          ggsave(transfers_filename, plot = transfers_plot, width = 16, height = 10, dpi = 300, bg = "white")
        }
        
        # 4. Créer un résumé textuel
        balances <- team_balances()
        total_spent <- sum(balances$expenditure_euros, na.rm = TRUE)
        total_income <- sum(balances$income_euros, na.rm = TRUE)
        net_balance <- total_income - total_spent
        transfers_count <- ifelse(!is.null(transfers_clean()) && nrow(transfers_clean()) > 0, nrow(transfers_clean()), 0)
        
        country_name <- names(which(c(
          "Germany" = "Bundesliga",
          "England" = "Premier League", 
          "Spain" = "La Liga",
          "France" = "Ligue 1",
          "Italy" = "Serie A",
          "Portugal" = "Primeira Liga",
          "Netherlands" = "Eredivisie"
        ) == input$country))
        
        summary_text <- paste0(
          "RÉSUMÉ DE L'ANALYSE DES TRANSFERTS\n",
          "=====================================\n\n",
          "Ligue: ", country_name, "\n",
          "Saison: ", input$season, "/", as.numeric(input$season) + 1, "\n",
          "Date d'analyse: ", Sys.Date(), "\n\n",
          "STATISTIQUES GÉNÉRALES:\n",
          "- Équipes analysées: ", nrow(balances), "\n",
          "- Dépenses totales: €", round(total_spent/1e6, 1), "M\n",
          "- Revenus totaux: €", round(total_income/1e6, 1), "M\n",
          "- Bilan net du marché: €", round(net_balance/1e6, 1), "M\n",
          "- Nombre de transferts individuels: ", transfers_count, "\n\n",
          "ÉQUIPES AVEC LES PLUS GROS DÉFICITS:\n"
        )
        
        # Ajouter le top 5 des équipes avec les plus gros déficits
        top_deficits <- balances %>%
          mutate(net_transfer_income = income_euros - expenditure_euros) %>%
          arrange(net_transfer_income) %>%
          head(5)
        
        for(i in 1:min(nrow(top_deficits), 5)) {
          summary_text <- paste0(summary_text, 
                                 i, ". ", top_deficits$squad[i], ": €", 
                                 round(top_deficits$net_transfer_income[i]/1e6, 1), "M\n")
        }
        
        summary_text <- paste0(summary_text, "\nÉQUIPES AVEC LES PLUS GROS EXCÉDENTS:\n")
        
        # Ajouter le top 5 des équipes avec les plus gros excédents
        top_profits <- balances %>%
          mutate(net_transfer_income = income_euros - expenditure_euros) %>%
          arrange(desc(net_transfer_income)) %>%
          head(5)
        
        for(i in 1:min(nrow(top_profits), 5)) {
          summary_text <- paste0(summary_text, 
                                 i, ". ", top_profits$squad[i], ": €", 
                                 round(top_profits$net_transfer_income[i]/1e6, 1), "M\n")
        }
        
        # Écrire le résumé
        summary_filename <- file.path(temp_dir, paste0("resume_analyse_", input$country, "_", input$season, ".txt"))
        writeLines(summary_text, summary_filename)
        
        # Lister tous les fichiers à inclure dans l'archive
        files_to_zip <- c(balance_filename, activity_filename, summary_filename)
        if(!is.null(transfers_clean()) && nrow(transfers_clean()) > 0) {
          files_to_zip <- c(files_to_zip, transfers_filename)
        }
        
        # Créer l'archive ZIP
        zip::zip(file, files = basename(files_to_zip), root = temp_dir)
        
        showNotification("✅ Archive créée avec succès!", type = "default", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur lors de la création de l'archive:", e$message), type = "error", duration = 10)
      })
    },
    contentType = "application/zip"
  )
}

# =============================================================================
# LANCEMENT DE L'APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
