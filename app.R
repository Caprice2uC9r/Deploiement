#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(flexdashboard)
library(shinythemes)
library(ggplot2)
library(naniar)
library(visdat)
library(gridExtra)


url= "C:/Users/HP/OneDrive/Documents/DATA SCIENCE/Cours R/Application_Determinants_Cancert/Cancert_2.csv"
data= read.csv(url, header=TRUE, sep = ",")

colnames(data) <- c(
  "Age",             # Âge
  "Sexe",            # Sexe
  "TypeDouleurThoracique",  # Type de Douleur Thoracique
  "TensionRepos",    # Tension au Repos
  "Cholesterol",     # Cholestérol
  "GlycemieAJeun",   # Glycémie à Jeun
  "ECGRepos",        # Electrocardiogramme au repos
  "FreqCardiaqueMax",# Fréquence Cardiaque Maximale
  "AngineExercice",  # Angine d'Exercice
  "DepressionST",    # Dépression ST
  "PenteST",         # Pente ST (aspect de l'électrocardiogramme)
  "MaladieCardiaque" # Maladie Cardiaque
)

data$MaladieCardiaque <- factor(data$MaladieCardiaque)

data


# Assurez-vous que les variables qualitatives sont du bon type
data$Sexe <- factor(data$Sexe, ordered = TRUE)
data$TypeDouleurThoracique <- factor(data$TypeDouleurThoracique, ordered = TRUE)
data$ECGRepos <- factor(data$ECGRepos, ordered = TRUE)
data$AngineExercice <- factor(data$AngineExercice, ordered = TRUE)
data$PenteST <- factor(data$PenteST, ordered = TRUE)


# Créer des barplots pour chaque variable qualitative par rapport à MaladieCardiaque


ui <- fluidPage(
  h1("APPLICATION: Determinant de la maladie du Cancert"),
  h2("Auteur: Wilga Prefina MBANI"),
  tags$a('Mon site facebook :', href= 'http://www.facebook.com/ECOSTATmachinelearnia'),
  tags$a('Mon site youtube :', href= 'http://www.youtube.com/@ECO-STATmachine_learnia'),
  theme = shinytheme("flatly"),  # Appliquer un thème
  

  sidebarLayout(
    sidebarPanel(
      selectInput('analysis_type', 'Type d\'analyse :', 
                  choices = c('Univariée', 'Bivariée')),
      conditionalPanel(
        condition = "input.analysis_type == 'Univariée'",
        selectInput('univariate_plot_type', 'Type de graphique univarié :', 
                    choices = c('Histogramme', 'Box plot', 'Diagramme à barres', 'Camembert')),
        selectInput('var', 'Choisis une variable :', choices = names(data))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Bivariée'",
        selectInput('bivariate_plot_type', 'Type de graphique bivarié :', 
                    choices = c('Scatter plot', 'Box plot')),
        selectInput('var', 'Choisis une variable :', choices = names(data)),
        selectInput('var2', 'Choisis une 2ème variable :', choices = names(data))
      ),
      actionButton("run_descriptive", "Analyse descriptive"),
      actionButton("run_structure", "Afficher la structure des données"),
      actionButton("run_normalization", "Normaliser les données"),
      actionButton("run_preprocessing", "Prétraiter les données"),
      actionButton("show_missing_data", "Visualiser les données manquantes"),
      actionButton("show_data_info", "Visualiser les infos"),
      actionButton("show_box_plot", "Visualiser les boxplots")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Data',
                 DTOutput('data_data'), 
                 downloadButton('save_data', 'Save to CSV')),
        tabPanel('Statistiques',
                 verbatimTextOutput('summary')),
        tabPanel('Graphiques', 
                 fluidRow(
                   column(6, plotOutput('univariate_plot'), height = "600px"),
                   column(6, plotOutput('bivariate_plot'), height = "600px")
                   
                 )
        ),
        tabPanel('Informations', 
                 verbatimTextOutput('info'),
                 
                 fluidRow (
                   column(6, plotOutput('missing_plot'),height = "600px"),
                   column(6, plotOutput('show_data'), height= "600px"),
                   column(6, plotOutput('box_plot'), height= '600px')
                   )
                 )
        )
        )
      )
    )
  

  


server <- function(input, output) {
  df <- reactive({
    data
  })
  
  
  observeEvent(input$run_structure, {
    output$summary <- renderPrint({
      str(data)
    })
  })
  
  observeEvent(input$show_data, {
    output$data_data <- renderDT({
      datatable(df(), options = list(searching = TRUE, paging = TRUE, info = TRUE))
    })
  })
  
  output$summary <- renderPrint({
    "Action personnalisée exécutée"
  })
  
  observeEvent(input$run_normalization, {
    # Ajoutez ici votre code de normalisation
    output$summary <- renderPrint({
      "Normalisation effectuée"
    })
  })
  
  observeEvent(input$run_descriptive, {
    
    output$summary <- renderPrint({
      summary(data)
    })
  })
  
  observeEvent(input$show_data_info, {
    
    output$show_data <- renderPlot({
      barplot_plots <- lapply(names(data)[sapply(data, is.factor)],
                              function(variable) {
                                ggplot(data, aes(x = factor(MaladieCardiaque), fill = .data[[variable]])) +
                                  geom_bar(position = "fill") +
                                  labs(title = paste("Répartition de", variable, "par Maladie Cardiaque"),
                                       x = "Maladie Cardiaque",
                                       y = "Proportion") +
                                  theme_minimal() +
                                  scale_fill_brewer(palette = "Set1")
                              })
      do.call(grid.arrange, c(barplot_plots, ncol = 2))
    })
  })
  
  observeEvent(input$show_box_plot,{
    
    output$box_plot= renderPlot({
      boxplot_plots <- lapply(names(data)[sapply(data, is.numeric)],
                              function(variable) {
                                ggplot(data, aes(x = factor(MaladieCardiaque), y = .data[[variable]], fill = factor(MaladieCardiaque))) +
                                  geom_boxplot() +
                                  labs(title = paste("Boxplot de", variable, "par Maladie Cardiaque"),
                                       x = "Maladie Cardiaque", y = variable) +
                                  theme_minimal()
                              })
    })
  })
  
  observeEvent(input$show_missing_data, {
    output$missing_plot <- renderPlot({
      vis_miss(data)
    })
  })
  output$data_data <- renderDT({
    datatable(df(), options = list(searching = TRUE, paging = TRUE, info = TRUE))
  })
  
  output$univariate_plot <- renderPlot({
    req(input$analysis_type == 'Univariée')
    var <- input$var
    plot_type <- input$univariate_plot_type
    if (plot_type == 'Histogramme' && is.numeric(data[[var]])) {
      hist(data[[var]], main = paste('Histogram of', var), xlab = var, col= "brown")
    } else if (plot_type == 'Box plot' && is.numeric(data[[var]])) {
      boxplot(data[[var]], main = paste('Boxplot of', var), xlab = var, col= "steelblue")
    } else if (plot_type == 'Diagramme à barres' && !is.numeric(data[[var]])) {
      barplot(table(data[[var]]), main = paste('Barplot of', var), xlab = var, ylab = 'Frequency', col= data$MaladieCardiaque)
    } else if (plot_type == 'Camembert' && var == 'MaladieCardiaque') {
      pie(table(data[[var]]), main = paste('Pie chart of', var), col= c("green", "red"))
    }
  })
  
  output$bivariate_plot <- renderPlot({
    req(input$analysis_type == 'Bivariée')
    var <- input$var
    var2 <- input$var2
    plot_type <- input$bivariate_plot_type
    if (plot_type == 'Scatter plot' && is.numeric(data[[var]]) && is.numeric(data[[var2]])) {
      plot(data[[var]], data[[var2]], main = paste('Scatter plot of', var, 'and', var2), xlab = var, ylab = var2, col=data$MaladieCardiaque)
    } else if (plot_type == 'Box plot' && is.numeric(data[[var]]) && !is.numeric(data[[var2]])) {
      boxplot(data[[var]] ~ data[[var2]], main = paste('Box plot of', var, 'by', var2), xlab = var2, ylab = var, col= data$MaladieCardiaque)
    }
  })
 
  

 
}

shinyApp(ui = ui, server = server)

