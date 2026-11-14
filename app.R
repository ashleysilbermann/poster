
library(shiny)
library(ggplot2)

# Interface
ui <- fluidPage(
  titlePanel("MSA - Analyse du système de mesure"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Paramètres de simulation"),
      
      sliderInput("rep", "Répétabilité",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      
      sliderInput("repro", "Reproductibilité",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      
      sliderInput("piece", "Variabilité des pièces",
                  min = 0.1, max = 10, value = 5, step = 0.1),
      
      hr(),
      h4("Définitions"),
      p("Répétabilité : variation des mesures réalisées dans les mêmes conditions."),
      p("Reproductibilité : variation des mesures entre opérateurs."),
      p("Gage R&R : combinaison de la répétabilité et de la reproductibilité.")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Présentation
        tabPanel("Présentation",
                 h3("Qu'est-ce que le MSA ?"),
                 p("Le MSA (Measurement System Analysis) permet d'évaluer la fiabilité d'un système de mesure."),
                 h4("Objectifs"),
                 tags$ul(
                   tags$li("Identifier les sources de variabilité"),
                   tags$li("Évaluer la répétabilité"),
                   tags$li("Évaluer la reproductibilité"),
                   tags$li("Déterminer si le système de mesure est adapté")
                 )
        ),
        
        # Mesures
        tabPanel("Mesures",
                 h3("Simulation des mesures"),
                 plotOutput("mesures", height = "400px"),
                 p("Chaque couleur représente un opérateur. Les points représentent les mesures simulées.")
        ),
        
        # Gage R&R
        tabPanel("Gage R&R",
                 h3("Décomposition de la variabilité"),
                 plotOutput("variabilite", height = "350px"),
                 h4("Résultats théoriques"),
                 tableOutput("resultats"),
                 p("Les contributions correspondent à la proportion de variance attribuée à chaque source.")
        ),
        
        # Interprétation
        tabPanel("Interprétation",
                 h3("Performance du système de mesure"),
                 h4("Gage R&R (%Study Variation)"),
                 h2(textOutput("pourcentage")),
                 h4(textOutput("conclusion")),
                 hr(),
                 h4("Critères indicatifs"),
                 p("Moins de 10 % : système généralement acceptable."),
                 p("De 10 à 30 % : système acceptable sous conditions."),
                 p("Plus de 30 % : système généralement non acceptable.")
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Génération automatique des mesures
  donnees <- reactive({
    set.seed(123)
    
    pieces <- rep(1:10, each = 9)
    operateurs <- rep(rep(c("A", "B", "C"), each = 3), 10)
    
    valeurs_pieces <- rnorm(10, mean = 50, sd = input$piece)
    effets_operateurs <- rnorm(3, mean = 0, sd = input$repro)
    
    mesures <- valeurs_pieces[pieces] +
      effets_operateurs[match(operateurs, c("A", "B", "C"))] +
      rnorm(90, mean = 0, sd = input$rep)
    
    data.frame(
      Piece = factor(pieces),
      Operateur = factor(operateurs),
      Mesure = mesures
    )
  })
  
  # Graphique des mesures
  output$mesures <- renderPlot({
    ggplot(donnees(), aes(x = Piece, y = Mesure, color = Operateur)) +
      geom_point(
        position = position_jitterdodge(
          jitter.width = 0.15,
          dodge.width = 0.6,
          seed = 123
        ),
        size = 2.5,
        alpha = 0.8
      ) +
      scale_color_manual(values = c(
        "A" = "#0072B2",
        "B" = "#E69F00",
        "C" = "#009E73"
      )) +
      labs(
        title = "Mesures par pièce et opérateur",
        x = "Pièces",
        y = "Valeur mesurée",
        color = "Opérateur"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # Variances théoriques
  variances <- reactive({
    c(
      "Répétabilité" = input$rep^2,
      "Reproductibilité" = input$repro^2,
      "Pièces" = input$piece^2
    )
  })
  
  # Graphique de variabilité
  output$variabilite <- renderPlot({
    v <- variances()
    
    df <- data.frame(
      Source = factor(names(v), levels = names(v)),
      Contribution = as.numeric(100 * v / sum(v))
    )
    
    ggplot(df, aes(x = Source, y = Contribution, fill = Source)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(Contribution, 1), " %")),
        vjust = -0.5,
        size = 5
      ) +
      scale_fill_manual(values = c(
        "#0072B2", "#E69F00", "#009E73"
      )) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, 20)
      ) +
      labs(
        title = "Contribution des sources de variabilité",
        x = "",
        y = "Contribution à la variance (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  # Tableau des résultats
  output$resultats <- renderTable({
    v <- variances()
    
    data.frame(
      Source = names(v),
      Variance = round(as.numeric(v), 2),
      Contribution = paste0(
        round(100 * v / sum(v), 1), " %"
      )
    )
  })
  
  # Calcul du Gage R&R théorique
  rr <- reactive({
    v <- variances()
    sqrt((v["Répétabilité"] + v["Reproductibilité"]) / sum(v)) * 100
  })
  
  output$pourcentage <- renderText({
    paste0(round(rr(), 1), " %")
  })
  
  output$conclusion <- renderText({
    if (rr() < 10) {
      "Système de mesure généralement acceptable"
    } else if (rr() <= 30) {
      "Système acceptable sous conditions"
    } else {
      "Système de mesure à améliorer"
    }
  })
}

# Lancement
shinyApp(ui = ui, server = server)
