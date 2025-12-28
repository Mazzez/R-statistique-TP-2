# ===================================================================
# app.R - Application Shiny pour l'Analyse de la Pauvreté en Tunisie
# ===================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(tmap)
library(plotly)
library(DT)
library(leaflet)

# ===================================================================
# CHARGEMENT ET PRÉPARATION DES DONNÉES
# ===================================================================

# IMPORTANT : Désactiver S2 pour éviter les erreurs de géométrie
sf_use_s2(FALSE)

# Import des données
raw_pauv <- read.csv2("~/Desktop/R-stat/TP-2/pauvrete.csv",
                      sep = ";", dec = ".",
                      header = FALSE,
                      stringsAsFactors = FALSE)

# Nettoyage
pauv <- raw_pauv[-c(1:3), ]
colnames(pauv) <- c("Gouvernorat", "Taux_enquete", "Ecart_type_enquete",
                    "IC95_bas", "IC95_haut", "Taux_recensement",
                    "Ecart_type_recensement", "Diff_absolue")

pauv <- pauv %>%
  mutate(
    Gouvernorat = factor(Gouvernorat),
    Taux_enquete = as.numeric(Taux_enquete),
    Ecart_type_enquete = as.numeric(Ecart_type_enquete),
    IC95_bas = as.numeric(IC95_bas),
    IC95_haut = as.numeric(IC95_haut),
    Taux_recensement = as.numeric(Taux_recensement),
    Ecart_type_recensement = as.numeric(Ecart_type_recensement),
    Diff_absolue = as.numeric(Diff_absolue),
    Largeur_IC = IC95_haut - IC95_bas,
    Diff = Taux_recensement - Taux_enquete,
    Diff_relatif = Diff / Taux_recensement,
    Recensement_dans_IC = Taux_recensement >= IC95_bas & 
      Taux_recensement <= IC95_haut,
    Gouv_clean = toupper(as.character(Gouvernorat))
  )

# Chargement des données géographiques avec correction des géométries
gouv_sf <- st_read("~/Desktop/R-stat/TP-2/decoupage.geojson.json", quiet = TRUE) %>%
  rename(Gouvernorat_geo = gov_name_f) %>%
  mutate(Gouv_clean = toupper(Gouvernorat_geo)) %>%
  mutate(Gouv_clean = case_when(
    Gouv_clean == "SILIANA" ~ "SELIANA",
    grepl("KEF", Gouv_clean) ~ "LE KEF",
    Gouv_clean == "MANUBAH" ~ "MANOUBA",
    Gouv_clean == "BÉJA" ~ "BEJA",
    Gouv_clean == "KASSÉRINE" ~ "KASSERINE",
    Gouv_clean == "SIDI BOU ZID" ~ "SIDI BOUZID",
    Gouv_clean == "GABÈS" ~ "GABES",
    Gouv_clean == "MÉDENINE" ~ "MEDNINE",
    TRUE ~ Gouv_clean
  )) %>%
  # CORRECTION DES GÉOMÉTRIES INVALIDES
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

# Jointure spatiale avec correction supplémentaire
carte_pauv <- gouv_sf %>% 
  left_join(pauv, by = "Gouv_clean") %>%
  st_make_valid()  # Correction après la jointure

# Districts
districts_5 <- data.frame(
  Gouvernorat = c("BIZERTE", "BEJA", "JENDOUBA", "LE KEF",
                  "TUNIS", "ARIANA", "BEN AROUS", "MANOUBA", "ZAGHOUAN", "NABEUL",
                  "SELIANA", "SOUSSE", "KASSERINE", "KAIROUAN", "MONASTIR", "MAHDIA",
                  "TOZEUR", "SIDI BOUZID", "SFAX", "GAFSA",
                  "TATAOUINE", "GABES", "MEDNINE", "KEBILI"),
  District = c(rep("District 1 (Nord-Ouest)", 4),
               rep("District 2 (Nord-Est)", 6),
               rep("District 3 (Centre)", 6),
               rep("District 4 (Sud-Ouest/Centre-Est)", 4),
               rep("District 5 (Sud-Est)", 4))
) %>%
  mutate(Gouv_clean = toupper(Gouvernorat))

pauv_dist <- pauv %>%
  left_join(districts_5, by = "Gouv_clean") %>%
  group_by(District) %>%
  summarise(
    Moyenne_Enquete = mean(Taux_enquete, na.rm = TRUE),
    Moyenne_Recensement = mean(Taux_recensement, na.rm = TRUE),
    Diff_Moyenne = mean(Diff, na.rm = TRUE)
  )

# Préparation des districts géographiques
sf_use_s2(FALSE)

sf_districts <- gouv_sf %>%
  left_join(districts_5, by = "Gouv_clean") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  group_by(District) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  left_join(pauv_dist, by = "District") %>%
  st_transform(4326)  # Projection pour Leaflet

# ===================================================================
# INTERFACE UTILISATEUR (UI)
# ===================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # En-tête
  dashboardHeader(
    title = "Analyse Pauvreté Tunisie 2023",
    titleWidth = 350
  ),
  
  # Barre latérale
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("📊 Tableau de Bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("📈 Visualisations", tabName = "viz", icon = icon("chart-line")),
      menuItem("🗺️ Cartographie", tabName = "maps", icon = icon("map")),
      menuItem("🏛️ Cartes Districts", tabName = "districts", icon = icon("layer-group")),
      menuItem("📉 Régression", tabName = "regression", icon = icon("chart-area")),
      menuItem("📋 Données Brutes", tabName = "data", icon = icon("table")),
      menuItem("ℹ️ À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Corps principal
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { border-top: 3px solid #3c8dbc; }
      "))
    ),
    
    tabItems(
      # ===== ONGLET 1 : TABLEAU DE BORD =====
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBox(
            value = nrow(pauv),
            subtitle = "Gouvernorats analysés",
            icon = icon("map-marker-alt"),
            color = "blue"
          ),
          valueBox(
            value = paste0(round(mean(pauv$Taux_enquete), 1), "%"),
            subtitle = "Taux moyen (Enquête)",
            icon = icon("poll"),
            color = "red"
          ),
          valueBox(
            value = paste0(round(mean(pauv$Taux_recensement), 1), "%"),
            subtitle = "Taux moyen (Recensement)",
            icon = icon("chart-pie"),
            color = "green"
          )
        ),
        fluidRow(
          box(
            title = "Distribution des Taux de Pauvreté",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("boxplot_dashboard", height = 300)
          ),
          box(
            title = "Top 5 Gouvernorats (Enquête)",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top5_dashboard", height = 300)
          )
        ),
        fluidRow(
          box(
            title = "Cohérence Statistique (IC 95%)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("coherence_dashboard", height = 400)
          )
        )
      ),
      
      # ===== ONGLET 2 : VISUALISATIONS =====
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Options de Visualisation",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("viz_type", "Type de graphique :",
                        choices = c("Barplot comparatif" = "barplot",
                                    "Nuage de points" = "scatter",
                                    "Histogramme différences" = "hist",
                                    "Boxplot par source" = "boxplot")),
            checkboxInput("show_labels", "Afficher les étiquettes", value = TRUE),
            sliderInput("point_size", "Taille des points :",
                        min = 1, max = 10, value = 5, step = 0.5)
          ),
          box(
            title = "Graphique",
            status = "success",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("main_plot", height = 500)
          )
        )
      ),
      
      # ===== ONGLET 3 : CARTOGRAPHIE =====
      tabItem(
        tabName = "maps",
        fluidRow(
          box(
            title = "Paramètres Cartographiques",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("map_var", "Variable à cartographier :",
                        choices = c("Taux Enquête" = "Taux_enquete",
                                    "Taux Recensement" = "Taux_recensement",
                                    "Différence absolue" = "Diff",
                                    "Différence relative (%)" = "Diff_relatif")),
            selectInput("map_palette", "Palette de couleurs :",
                        choices = c("Rouge (Reds)" = "Reds",
                                    "Bleu (Blues)" = "Blues",
                                    "Vert (Greens)" = "Greens",
                                    "Violet-Orange (PuOr)" = "PuOr",
                                    "Rouge-Jaune-Vert (RdYlGn)" = "RdYlGn",
                                    "Viridis" = "viridis",
                                    "Plasma" = "plasma")),
            sliderInput("map_opacity", "Opacité des polygones :",
                        min = 0.3, max = 1, value = 0.7, step = 0.1),
            checkboxInput("show_legend", "Afficher la légende", value = TRUE),
            hr(),
            helpText("💡 Utilisez la souris pour zoomer et déplacer la carte.",
                     "Survolez les gouvernorats pour voir les détails.")
          ),
          box(
            title = "Carte Choroplèthe Interactive",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("choropleth_map", height = 600)  # MODIFIÉ : plotOutput -> leafletOutput
          )
        )
      ),
      
      # ===== ONGLET 4 : CARTES DES DISTRICTS =====
      tabItem(
        tabName = "districts",
        fluidRow(
          box(
            title = "Informations sur les Districts",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            HTML("
        <p><strong>Les 5 Districts Administratifs de la Tunisie (2023)</strong></p>
        <ul>
          <li><b>District 1 (Nord-Ouest)</b> : Bizerte, Béja, Jendouba, Le Kef</li>
          <li><b>District 2 (Nord-Est)</b> : Tunis, Ariana, Ben Arous, Manouba, Zaghouan, Nabeul</li>
          <li><b>District 3 (Centre)</b> : Siliana, Sousse, Kasserine, Kairouan, Monastir, Mahdia</li>
          <li><b>District 4 (Sud-Ouest/Centre-Est)</b> : Tozeur, Sidi Bouzid, Sfax, Gafsa</li>
          <li><b>District 5 (Sud-Est)</b> : Tataouine, Gabès, Médenine, Kébili</li>
        </ul>
      ")
          )
        ),
        
        fluidRow(
          box(
            title = "Paramètres des Cartes",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("district_var", "Variable à afficher :",
                        choices = c("Moyenne Enquête" = "Moyenne_Enquete",
                                    "Moyenne Recensement" = "Moyenne_Recensement",
                                    "Différence Moyenne" = "Diff_Moyenne")),
            selectInput("district_palette", "Palette de couleurs :",
                        choices = c("Rouge (Reds)" = "Reds",
                                    "Bleu (Blues)" = "Blues",
                                    "Vert (Greens)" = "Greens",
                                    "Orange (Oranges)" = "Oranges",
                                    "Violet-Orange (PuOr)" = "PuOr",
                                    "Viridis" = "viridis")),
            sliderInput("district_opacity", "Opacité :",
                        min = 0.3, max = 1, value = 0.8, step = 0.1),
            checkboxInput("show_district_labels", "Afficher noms districts", value = TRUE),
            hr(),
            helpText("🗺️ Carte interactive des 5 districts administratifs")
          ),
          
          box(
            title = "Carte des Districts - Vue Interactive",
            status = "success",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("district_map", height = 600)
          )
        ),
        
        fluidRow(
          box(
            title = "Comparaison Enquête vs Recensement par District",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("district_comparison", height = 400)
          ),
          
          box(
            title = "Tableau Récapitulatif des Districts",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("district_summary_table")
          )
        )
      ),
      
      # ===== ONGLET 5 : RÉGRESSION =====
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            title = "Régression Linéaire : Recensement ~ Enquête",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("regression_plot", height = 500)
          ),
          box(
            title = "Statistiques du Modèle",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("regression_summary")
          )
        ),
        fluidRow(
          box(
            title = "Résidus du Modèle",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("residuals_plot", height = 350)
          )
        )
      ),
      
      # ===== ONGLET 6 : DONNÉES BRUTES =====
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Données Complètes",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        ),
        fluidRow(
          box(
            title = "Statistiques Descriptives",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("summary_stats")
          ),
          box(
            title = "Analyse par Districts",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("district_table")
          )
        )
      ),
      
      # ===== ONGLET 7 : À PROPOS =====
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "À propos de cette application",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <h3>Analyse des Disparités de Pauvreté en Tunisie (2023)</h3>
              <p><strong>Auteur :</strong> Mazzez Mohamed Amine</p>
              <p><strong>Contexte :</strong> Comparaison des taux de pauvreté entre l'Enquête Nationale 
              et le Recensement Général au niveau des 24 gouvernorats tunisiens.</p>
              
              <h4>Fonctionnalités de l'application :</h4>
              <ul>
                <li>📊 Tableau de bord avec indicateurs clés</li>
                <li>📈 Visualisations interactives (graphiques Plotly)</li>
                <li>🗺️ Cartographie choroplèthe des taux de pauvreté</li>
                <li>📉 Analyse de régression linéaire</li>
                <li>📋 Exploration des données brutes</li>
              </ul>
              
              <h4>Technologies utilisées :</h4>
              <ul>
                <li>R Shiny & Shinydashboard</li>
                <li>Tidyverse (dplyr, ggplot2)</li>
                <li>SF & tmap (cartographie)</li>
                <li>Plotly (graphiques interactifs)</li>
                <li>DT (tableaux interactifs)</li>
              </ul>
              
              <hr>
              <p><em>Projet réalisé dans le cadre d'un cours de statistiques</em></p>
            ")
          )
        )
      )
    )
  )
)

# ===================================================================
# SERVEUR
# ===================================================================

server <- function(input, output, session) {
  
  # ----- TABLEAU DE BORD -----
  output$boxplot_dashboard <- renderPlotly({
    pauv_long <- pauv %>%
      select(Gouvernorat, Taux_enquete, Taux_recensement) %>%
      pivot_longer(cols = c(Taux_enquete, Taux_recensement),
                   names_to = "Source", values_to = "Taux") %>%
      mutate(Source = ifelse(Source == "Taux_enquete", "Enquête", "Recensement"))
    
    plot_ly(pauv_long, y = ~Taux, x = ~Source, color = ~Source,
            type = "box", colors = c("#E74C3C", "#3498DB")) %>%
      layout(title = "", yaxis = list(title = "Taux de pauvreté (%)"),
             xaxis = list(title = ""), showlegend = FALSE)
  })
  
  output$top5_dashboard <- renderPlotly({
    top5 <- pauv %>%
      arrange(desc(Taux_enquete)) %>%
      head(5)
    
    plot_ly(top5, x = ~Taux_enquete, y = ~reorder(Gouvernorat, Taux_enquete),
            type = "bar", marker = list(color = "#E74C3C")) %>%
      layout(xaxis = list(title = "Taux de pauvreté (%)"),
             yaxis = list(title = ""), showlegend = FALSE)
  })
  
  output$coherence_dashboard <- renderPlotly({
    plot_ly(pauv, x = ~reorder(Gouvernorat, Taux_recensement),
            y = ~Taux_recensement, type = "scatter", mode = "markers",
            marker = list(size = 10, color = ~Recensement_dans_IC,
                          colors = c("#E74C3C", "#27AE60")),
            error_y = list(type = "data", symmetric = FALSE,
                           array = ~IC95_haut - Taux_recensement,
                           arrayminus = ~Taux_recensement - IC95_bas,
                           color = "#95A5A6")) %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = "Taux de pauvreté (%)"),
             showlegend = FALSE) %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  # ----- VISUALISATIONS -----
  output$main_plot <- renderPlotly({
    if (input$viz_type == "barplot") {
      
      pauv_sorted <- pauv %>% arrange(Taux_enquete)
      
      plot_ly(pauv_sorted) %>%
        add_trace(x = ~Gouvernorat, y = ~Taux_enquete, type = "bar",
                  name = "Enquête", marker = list(color = "#E74C3C")) %>%
        add_trace(x = ~Gouvernorat, y = ~Taux_recensement, type = "bar",
                  name = "Recensement", marker = list(color = "#3498DB")) %>%
        layout(barmode = "group",
               xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = "Taux de pauvreté (%)"))
      
    } else if (input$viz_type == "scatter") {
      
      # Version SIMPLE - création directe du graphique
      fig <- plot_ly(data = pauv, 
                     x = ~Taux_enquete, 
                     y = ~Taux_recensement,
                     type = "scatter",
                     mode = "markers",
                     marker = list(size = ~input$point_size * 2, color = "#2980B9"),
                     text = ~Gouvernorat,
                     hovertemplate = paste('<b>%{text}</b><br>',
                                           'Enquête: %{x:.1f}%<br>',
                                           'Recensement: %{y:.1f}%',
                                           '<extra></extra>'),
                     name = "Gouvernorats")
      
      # Ligne y = x
      fig <- fig %>%
        add_segments(x = min(pauv$Taux_enquete), 
                     xend = max(pauv$Taux_enquete),
                     y = min(pauv$Taux_enquete), 
                     yend = max(pauv$Taux_enquete),
                     line = list(dash = "dash", color = "gray", width = 2),
                     name = "y = x",
                     hoverinfo = "skip")
      
      # Ajouter les labels seulement si demandé
      if (input$show_labels) {
        for(i in 1:nrow(pauv)) {
          fig <- fig %>%
            add_annotations(
              x = pauv$Taux_enquete[i],
              y = pauv$Taux_recensement[i],
              text = as.character(pauv$Gouvernorat[i]),
              xanchor = "left",
              xshift = 5,
              showarrow = FALSE,
              font = list(size = 8)
            )
        }
      }
      
      # Layout final
      fig %>%
        layout(xaxis = list(title = "Taux Enquête (%)"),
               yaxis = list(title = "Taux Recensement (%)"),
               showlegend = TRUE)
      
    } else if (input$viz_type == "hist") {
      
      plot_ly(pauv, x = ~Diff, type = "histogram",
              marker = list(color = "#16A085")) %>%
        layout(xaxis = list(title = "Différence (Rec - Enq)"),
               yaxis = list(title = "Fréquence"))
      
    } else if (input$viz_type == "boxplot") {
      
      pauv_long <- pauv %>%
        pivot_longer(cols = c(Taux_enquete, Taux_recensement),
                     names_to = "Source", values_to = "Taux")
      
      plot_ly(pauv_long, x = ~Source, y = ~Taux, color = ~Source,
              type = "box", colors = c("#E74C3C", "#3498DB")) %>%
        layout(xaxis = list(title = ""), 
               yaxis = list(title = "Taux de pauvreté (%)"))
    }
  })
  
  # ----- CARTOGRAPHIE -----
  output$choropleth_map <- renderLeaflet({
    
    # S'assurer que les géométries sont valides
    carte_clean <- carte_pauv %>% 
      st_make_valid() %>%
      st_transform(4326)  # Projection WGS84 pour Leaflet
    
    # Récupérer la variable sélectionnée
    var_data <- carte_clean[[input$map_var]]
    
    # Créer une palette de couleurs
    if (input$map_palette %in% c("viridis", "plasma")) {
      pal <- colorNumeric(
        palette = input$map_palette,
        domain = var_data,
        na.color = "#808080"
      )
    } else {
      # Pour les palettes RColorBrewer
      pal <- colorNumeric(
        palette = input$map_palette,
        domain = var_data,
        na.color = "#808080",
        reverse = FALSE
      )
    }
    
    # Créer les labels HTML pour les tooltips
    labels <- sprintf(
      "<strong>%s</strong><br/>
    Taux Enquête: <b>%.1f%%</b><br/>
    Taux Recensement: <b>%.1f%%</b><br/>
    Différence: <b>%+.1f points</b><br/>
    IC 95%%: [%.1f - %.1f]<br/>
    %s",
      carte_clean$Gouv_clean,
      carte_clean$Taux_enquete,
      carte_clean$Taux_recensement,
      carte_clean$Diff,
      carte_clean$IC95_bas,
      carte_clean$IC95_haut,
      ifelse(carte_clean$Recensement_dans_IC, 
             "✅ Cohérent avec IC", 
             "❌ Hors IC")
    ) %>% lapply(htmltools::HTML)
    
    # Créer la carte Leaflet
    map <- leaflet(carte_clean) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(opacity = 0.8)) %>%
      addPolygons(
        fillColor = ~pal(var_data),
        fillOpacity = input$map_opacity,
        color = "white",
        weight = 2,
        opacity = 1,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )
    
    # Ajouter la légende si demandée
    if (input$show_legend) {
      map <- map %>%
        addLegend(
          pal = pal,
          values = var_data,
          opacity = 0.7,
          title = input$map_var,
          position = "bottomright"
        )
    }
    
    # Centrer la carte sur la Tunisie
    map <- map %>%
      setView(lng = 9.5, lat = 34, zoom = 6.5)
    
    map
  })
  
  # ----- CARTES DES DISTRICTS -----
  output$district_map <- renderLeaflet({
    
    # Créer une palette de couleurs
    var_data <- sf_districts[[input$district_var]]
    
    if (input$district_palette %in% c("viridis", "plasma")) {
      pal <- colorNumeric(
        palette = input$district_palette,
        domain = var_data,
        na.color = "#808080"
      )
    } else {
      pal <- colorNumeric(
        palette = input$district_palette,
        domain = var_data,
        na.color = "#808080"
      )
    }
    
    # Créer les labels pour les tooltips
    labels <- sprintf(
      "<strong>%s</strong><br/>
    <hr style='margin:5px 0;'>
    📊 <b>Moyenne Enquête:</b> %.2f%%<br/>
    📊 <b>Moyenne Recensement:</b> %.2f%%<br/>
    📈 <b>Différence:</b> %+.2f points<br/>
    <hr style='margin:5px 0;'>
    <em>Cliquez pour plus de détails</em>",
      sf_districts$District,
      sf_districts$Moyenne_Enquete,
      sf_districts$Moyenne_Recensement,
      sf_districts$Diff_Moyenne
    ) %>% lapply(htmltools::HTML)
    
    # Créer la carte
    map <- leaflet(sf_districts) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(var_data),
        fillOpacity = input$district_opacity,
        color = "#444444",
        weight = 3,
        opacity = 1,
        highlight = highlightOptions(
          weight = 5,
          color = "#FF0000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "5px 10px"),
          textsize = "14px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = var_data,
        opacity = 0.7,
        title = input$district_var,
        position = "bottomright"
      ) %>%
      setView(lng = 9.5, lat = 34, zoom = 6)
    
    # Ajouter les étiquettes des districts si demandé
    if (input$show_district_labels) {
      # Calculer les centroïdes pour placer les labels
      centroids <- st_centroid(sf_districts)
      coords <- st_coordinates(centroids)
      
      map <- map %>%
        addLabelOnlyMarkers(
          lng = coords[, "X"],
          lat = coords[, "Y"],
          label = sf_districts$District,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "#000000",
              "font-weight" = "bold",
              "font-size" = "12px",
              "text-shadow" = "2px 2px 4px #FFFFFF, -2px -2px 4px #FFFFFF"
            )
          )
        )
    }
    
    map
  })
  
  # Graphique de comparaison par district
  output$district_comparison <- renderPlotly({
    plot_ly(pauv_dist, x = ~District) %>%
      add_trace(
        y = ~Moyenne_Enquete,
        type = "bar",
        name = "Enquête",
        marker = list(color = "#E74C3C")
      ) %>%
      add_trace(
        y = ~Moyenne_Recensement,
        type = "bar",
        name = "Recensement",
        marker = list(color = "#3498DB")
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(
          title = "",
          tickangle = -25,
          tickfont = list(size = 10)
        ),
        yaxis = list(title = "Taux de pauvreté moyen (%)"),
        legend = list(orientation = "h", y = -0.2),
        margin = list(b = 100)
      )
  })
  
  # Tableau récapitulatif des districts
  output$district_summary_table <- renderDT({
    pauv_dist %>%
      mutate(
        District = gsub("District \\d+ \\((.+)\\)", "\\1", District)
      ) %>%
      datatable(
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "District",
          "Moy. Enquête (%)",
          "Moy. Recensement (%)",
          "Différence (pts)"
        )
      ) %>%
      formatRound(columns = c("Moyenne_Enquete", "Moyenne_Recensement", "Diff_Moyenne"),
                  digits = 2) %>%
      formatStyle(
        "Diff_Moyenne",
        backgroundColor = styleInterval(
          cuts = c(-1, 1),
          values = c("#27AE60", "#F39C12", "#E74C3C")
        )
      )
  })
  
  # ----- RÉGRESSION -----
  modele <- reactive({
    lm(Taux_recensement ~ Taux_enquete, data = pauv)
  })
  
  output$regression_plot <- renderPlotly({
    mod <- modele()
    pauv$pred <- predict(mod)
    
    plot_ly(pauv, x = ~Taux_enquete, y = ~Taux_recensement,
            type = "scatter", mode = "markers",
            marker = list(size = 10, color = "#2C3E50"),
            text = ~Gouvernorat, name = "Observations") %>%
      add_trace(x = ~Taux_enquete, y = ~pred,
                type = "scatter", mode = "lines",
                line = list(color = "#E74C3C", width = 2),
                name = "Régression") %>%
      layout(xaxis = list(title = "Taux Enquête (%)"),
             yaxis = list(title = "Taux Recensement (%)"))
  })
  
  output$regression_summary <- renderPrint({
    summary(modele())
  })
  
  output$residuals_plot <- renderPlotly({
    mod <- modele()
    pauv$resid <- residuals(mod)
    pauv$fitted <- fitted(mod)
    
    plot_ly(pauv, x = ~fitted, y = ~resid,
            type = "scatter", mode = "markers",
            marker = list(size = 8, color = "#9B59B6")) %>%
      add_trace(x = c(min(pauv$fitted), max(pauv$fitted)),
                y = c(0, 0), type = "scatter", mode = "lines",
                line = list(dash = "dash", color = "red"),
                name = "y = 0") %>%
      layout(xaxis = list(title = "Valeurs ajustées"),
             yaxis = list(title = "Résidus"),
             showlegend = FALSE)
  })
  
  # ----- DONNÉES BRUTES -----
  output$data_table <- renderDT({
    datatable(pauv %>% 
                select(Gouvernorat, Taux_enquete, Taux_recensement, 
                       IC95_bas, IC95_haut, Diff, Recensement_dans_IC),
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE)
  })
  
  output$summary_stats <- renderPrint({
    summary(pauv %>% 
              select(Taux_enquete, Taux_recensement, Diff, Largeur_IC))
  })
  
  output$district_table <- renderDT({
    datatable(pauv_dist,
              options = list(pageLength = 5, dom = 't'),
              rownames = FALSE) %>%
      formatRound(columns = c("Moyenne_Enquete", "Moyenne_Recensement", "Diff_Moyenne"),
                  digits = 2)
  })
}

# ===================================================================
# LANCEMENT DE L'APPLICATION
# ===================================================================

shinyApp(ui = ui, server = server)
