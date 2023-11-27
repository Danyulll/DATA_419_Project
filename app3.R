library(shiny)
library(plotly)
library(klaR)
library(mvtnorm)
library(shinyMatrix)
library(semantic.dashboard)
library(shinythemes)
library(thematic)
set.seed(231354)


ui <- dashboardPage(
  dashboardHeader(title = "DATA 419 Project Title"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "simulation", "Simulation"),
    menuItem(tabName = "application", "Application")
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
    tabItem(tabName = "application", fluidPage(h1("application")))
  )),#theme = "darkly"
)
   

# ui <- dashboardPage(
#   dashboardHeader(title = "My Dashboard"),
#   dashboardSidebar(
#     width = 2,  # Adjust the width of the sidebar
#     numericInput("sld1x", "Change mu1x:", value = 1),
#     numericInput("sld1y", "Change mu1y:", value = 1),
    # numericInput("sld2x", "Change mu2x:", value = 0),
    # numericInput("sld2y", "Change mu2y:", value = 0),
    # numericInput("sld3x", "Change mu3x:", value = -1),
    # numericInput("sld3y", "Change mu3y:", value = -1),
    # matrixInput(
    #   "matrix1",
    #   label = "G1",
    #   value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE),
    #   rows = list(names = FALSE),
    #   cols = list(names = FALSE),
    #   class = "numeric"
    # ),
    # matrixInput(
    #   "matrix2",
    #   label = "G2",
    #   value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE),
    #   rows = list(names = FALSE),
    #   cols = list(names = FALSE),
    #   class = "numeric"
    # ),
    # matrixInput(
    #   "matrix3",
    #   label = "G3",
    #   value = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE),
    #   rows = list(names = FALSE),
    #   cols = list(names = FALSE),
    #   class = "numeric"
    # ),
    # numericInput("lambda", "Change lambda:", value = 0),
    # numericInput("gamma", "Change gamma:", value = 0),
    # sliderInput("slider1", "Slider 1:", min = 0, max = 1, value = 0.5),
    # sliderInput("slider2", "Slider 2:", min = 0, max = 1, value = 0.5),
    # sliderInput("slider3", "Slider 3:", min = 0, max = 1, value = 0.5),
    # actionButton("regenerate", "(Re)generate Plot")
#   ),
#   dashboardBody(
#     width = 10,  # Adjust the width of the body
#     fluidRow(
#       box(
#         plotlyOutput("plot", height = "600px", width = "100%")  # Use 100% width
#       )
#     )
#   )
# )



# 
# ui <- fluidPage(fluidRow(
#   column(
#     6,
#     numericInput("sld1x", "Change mu1x:", value = 1),
#     numericInput("sld1y", "Change mu1y:", value = 1),
#     numericInput("sld2x", "Change mu2x:", value = 0),
#     numericInput("sld2y", "Change mu2y:", value = 0),
#     numericInput("sld3x", "Change mu3x:", value = -1),
#     numericInput("sld3y",
#                  "Change mu3y:",
#                  value = -1),
#     matrixInput(
#       "matrix1",
#       label = "G1",
#       value = matrix(
#         c(1, 0, 0, 1),
#         nrow = 2,
#         ncol = 2,
#         byrow = TRUE
#       ),
#       rows = list(names = FALSE),
#       cols = list(names = FALSE),
#       class = "numeric"
#     ),
#     matrixInput(
#       "matrix2",
#       label = "G2",
#       value = matrix(
#         c(1, 0, 0, 1),
#         nrow = 2,
#         ncol = 2,
#         byrow = TRUE
#       ),
#       rows = list(names = FALSE),
#       cols = list(names = FALSE),
#       class = "numeric"
#     ),
#     matrixInput(
#       "matrix3",
#       label = "G3",
#       value = matrix(
#         c(1, 0, 0, 1),
#         nrow = 2,
#         ncol = 2,
#         byrow = TRUE
#       ),
#       rows = list(names = FALSE),
#       cols = list(names = FALSE),
#       class = "numeric"
#     ),
#     numericInput("lambda", "Change lambda:", value = 0),
#     numericInput("gamma", "Change gamma:", value = 0),
#     sliderInput(
#       "slider1",
#       "Slider 1:",
#       min = 0,
#       max = 1,
#       value = 0.5
#     ),
#     sliderInput(
#       "slider2",
#       "Slider 2:",
#       min = 0,
#       max = 1,
#       value = 0.5
#     ),
#     sliderInput(
#       "slider3",
#       "Slider 3:",
#       min = 0,
#       max = 1,
#       value = 0.5
#     ),
#     actionButton("regenerate", "(Re)generate Plot")
#   ),
#   column(3,
#          plotlyOutput("gaussian_plot", height = "600px", width = "800px"))
# ))


server <- function(input, output, session) {
  
  
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
  
  
}
# thematic_shiny()
shinyApp(ui, server)