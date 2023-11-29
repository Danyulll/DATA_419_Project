library(shiny)
library(plotly)
library(klaR)
library(mvtnorm)
library(shinyMatrix)
library(semantic.dashboard)
library(shinythemes)
library(thematic)
library(DT)
library(GGally)
library(ggplot2)
set.seed(231354)


ui <- dashboardPage(
  dashboardHeader(title = "DATA 419 Project Title"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "simulation", "Simulation"),
    menuItem(tabName = "eda", "EDA"),
    menuItem(tabName = "pca & nmf", "PCA & NMF")
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "simulation",
      box(plotlyOutput("plot"), width = 8, title = "Density"),
      box(
        numericInput("sld1x", "Change mu1x:", value = 1),
        numericInput("sld1y", "Change mu1y:", value = 1),
        numericInput("sld2x", "Change mu2x:", value = 0),
        numericInput("sld2y", "Change mu2y:", value = 0),
        numericInput("sld3x", "Change mu3x:", value = -1),
        numericInput("sld3y", "Change mu3y:", value = -1),
        width = 3
      ),
      box(
        
        h3(withMathJax("$$ \\begin{align} 
                       (\\gamma = 0, \\lambda = 0)&\\text{: QDA} \\\\
                       (\\gamma = 0, \\lambda = 1)&\\text{: LDA} \\\\
                       (\\gamma = 1, \\lambda = 0)&\\text{: Independent QDA} \\\\
                       (\\gamma = 1, \\lambda = 1)&\\text{: Independent QDA} 
                       \\end{align} $$")),numericInput("lambda", "Change lambda:", value = 0),
        numericInput("gamma", "Change gamma:", value = 0),
        actionButton("regenerate", "(Re)generate Plot"),
        width = 4
      ),
      # box(
      #   h3(withMathJax("$$ \\begin{align} 
      #                   \\hat{\\Sigma}_k(\\lambda) &:= (1-\\lambda)\\hat{\\Sigma}_k+\\lambda\\hat{\\Sigma} \\\\  
      # \\hat{\\Sigma}_k(\\lambda,\\gamma) &= (1-\\gamma)\\hat{\\Sigma}_k(\\lambda) + \\gamma \\frac{1}{d} \\text{tr}[\\hat{\\Sigma}_k(\\lambda)]I \\\\
      # 
      #                  \\end{align} $$")),width = 8
      # ),
      
      box(
        matrixInput(
          "matrix1",
          label = "G1",
          value = matrix(
            c(1, 0, 0, 1),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          ),
          rows = list(names = FALSE),
          cols = list(names = FALSE),
          class = "numeric"
        ),
       
        matrixInput(
          "matrix2",
          label = "G2",
          value = matrix(
            c(1, 0, 0, 1),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          ),
          rows = list(names = FALSE),
          cols = list(names = FALSE),
          class = "numeric"
        ),
        matrixInput(
          "matrix3",
          label = "G3",
          value = matrix(
            c(1, 0, 0, 1),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          ),
          rows = list(names = FALSE),
          cols = list(names = FALSE),
          class = "numeric"
        )
      ),
  
      box(
        sliderInput(
          "slider1",
          "Slider 1:",
          min = 0,
          max = 1,
          value = 0.5
        ),
        sliderInput(
          "slider2",
          "Slider 2:",
          min = 0,
          max = 1,
          value = 0.5
        ),
        sliderInput(
          "slider3",
          "Slider 3:",
          min = 0,
          max = 1,
          value = 0.5
        ),width = 4
      )
    ),
    tabItem(tabName = "eda",     fluidRow(
      box(
        title = "Dataframe Box",
        width = 7,
        dataTableOutput("bc")
      ),
      box(
        verbatimTextOutput("descrip"),width = 5
      ),box(selectInput("selectedColumn", "Select Column", choices = colnames(bc)),
            tableOutput("summaryOutput"),width=3),
      box(plotOutput("pairs1")),
      box(plotOutput("pairs2"))
    )),tabItem(tabName = "pca & nmf")
  ))
)
   
server <- function(input, output, session) {
  
  # Tab 1
  plot_data <- eventReactive(input$regenerate, {
    # Population Params
    # sigma1 <- matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE)
    sigma1 <- input$matrix1
    mu1 <- c(input$sld1x, input$sld1y)

    sigma2 <- input$matrix2
    mu2 <- c(input$sld2x, input$sld2y)

    sigma3 <- input$matrix3
    mu3 <- c(input$sld3x, input$sld3y)

    # DGP
    group1 <- data.frame(rmvnorm(100, mean = mu1, sigma = sigma1))
    group2 <- data.frame(rmvnorm(100, mean = mu2, sigma = sigma2))
    group3 <- data.frame(rmvnorm(100, mean = mu3, sigma = sigma3))
    groups <- c(rep("G1", 100), rep("G2", 100), rep("G3", 100))

    dgp <-
      data.frame(
        "G" = groups,
        "X1" = c(group1$X1, group2$X1, group3$X1),
        "X2" = c(group1$X2, group2$X2, group3$X2)
      )

    # Set up grid
    x <- seq(-5, 5, length.out = 100)
    y <- seq(-5, 5, length.out = 100)
    grid <- expand.grid(x = x, y = y)
    colnames(grid) <- c("X1", "X2")

    # Calculate densities values for dists
    pdf_values1 <- dmvnorm(grid, mean = mu1, sigma = sigma1)
    pdf_values2 <- dmvnorm(grid, mean = mu2, sigma = sigma2)
    pdf_values3 <- dmvnorm(grid, mean = mu3, sigma = sigma3)

    # Reshape the density values to match the grid
    density1 <- matrix(pdf_values1, nrow = length(x))
    density2 <- matrix(pdf_values2, nrow = length(x))
    density3 <- matrix(pdf_values3, nrow = length(x))

    # Fit RDA model
    rda.out <-
      rda(G ~ .,
          dgp,
          gamma = input$gamma,
          lambda = input$lambda)

    # Get grid predictions
    prd = as.numeric(predict(rda.out, newdata = grid)$class)

    plot1 <-
      plot_ly() |>
      add_surface(
        x = x,
        y = y,
        z = density1,
        colors = "viridis",
        opacity = input$slider1,
        showlegend = FALSE
      ) |> add_surface(
        x = x,
        y = y,
        z = density2,
        colors = "viridis",
        opacity = input$slider2,
        showlegend = FALSE
      ) |> add_surface(
        x = x,
        y = y,
        z = density3,
        colors = "viridis",
        opacity = input$slider3,
        showlegend = FALSE
      ) |> add_markers(
        data = dgp,
        x = dgp$X1,
        y = dgp$X2,
        z = 0,
        opacity = 1,
        color = ~ dgp$G,
        marker = list(size = 4),
        showlegend = FALSE
      ) |> add_markers(
        x = grid$X1,
        y = grid$X2,
        z = 0,
        color =  ~ prd,
        opacity = 0.01,
        showlegend = FALSE

      )


  })

  output$plot <- renderPlotly({
    plot_data()
  })
  
  # Tab 2
  bc <- read.csv("C:\\Users\\danie\\Desktop\\2023 W1\\DATA 419C\\Project\\DATA_419_Project\\breast-cancer.csv") 
  bc <- na.omit(bc)
  bc <- bc <- bc[,2:12]
  bc$diagnosis <- factor(bc$diagnosis)
  output$bc <- renderDataTable(bc,
                               options = list(
                                 scrollX = TRUE,  # Enable horizontal scrolling
                                 scrollY = "300px",paging = FALSE  # Enable vertical scrolling with a fixed height
                               ),
                               callback = JS("table.column(0).visible(false);"),
                               )
  
  output$descrip <- renderText({   # Create a character vector with bullet points
      "diagnosis: Target: M - Malignant B - Benign
      
      radius_mean: Radius of Lobes

      texture_mean: Mean of Surface Texture
      
      perimeter_mean: Outer Perimeter of Lobes
      
      area_mean: Mean Area of Lobes

      smoothness_mean: Mean of Smoothness Levels

      compactness_mean: Mean of Compactness

      concavity_mean: Mean of Concavity

      concave points_mean: Mean of Cocave Points"
  })
  
  output$pairs1 <- renderCachedPlot({
    ggpairs(bc, columns = 1:5, aes(color = diagnosis, alpha = 0.5),
                     upper = list(continuous = wrap("cor", size = 3)))
  },
  cacheKeyExpr = { list() }
  )
  
  output$pairs2 <- renderCachedPlot({
    ggpairs(bc, columns = 5:10, aes(color = diagnosis, alpha = 0.5),
                      upper = list(continuous = wrap("cor", size = 3)))
  },
  cacheKeyExpr = { list() }
  )
  
  output$summaryOutput <- renderTable({
    col_selected <- input$selectedColumn
    if (!is.null(col_selected)) {
      as.data.frame(summary(bc[, col_selected, drop = FALSE]))[,3]
    }
  })
  
  
  
}
# thematic_shiny()
shinyApp(ui, server)