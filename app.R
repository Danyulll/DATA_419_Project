library(shiny)
library(plotly)
library(klaR)
library(mvtnorm)
set.seed(231354)

ui <- fluidPage(
  fluidRow(
    column(6,
           numericInput("sld1x", "Change mu1x:", value = 0),
           numericInput("sld1y", "Change mu1y:", value = 0),
           numericInput("sld2x", "Change mu2x:", value = 0),
           numericInput("sld2y", "Change mu2y:", value = 0),
           sliderInput("slider1", "Slider 1:", min = 0, max = 1, value = 0.5),
           sliderInput("slider2", "Slider 2:", min = 0, max = 1, value = 0.5),
           actionButton("pointview", "View Points"),
           actionButton("regenerate", "(Re)generate Plot")
    ),
    column(6,
           plotlyOutput("gaussian_plot")
    )
  )
)


server <- function(input, output, session) {
  plot_data <- eventReactive(input$regenerate,{
    # Population Params
    sigma1 <- matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE)
    mu1 <- c(input$sld1x, input$sld1y)
    mu2 <- c(input$sld2x, input$sld2y)
  
    # DGP
    group1 <- data.frame(rmvnorm(100, mean = mu1, sigma = sigma1))
    group2 <- data.frame(rmvnorm(100, mean = mu2, sigma = sigma1))
    groups <- c(rep("G1", 100), rep("G2", 100))
    
    dgp <-
      data.frame(
        "G" = groups,
        "X1" = c(group1$X1, group2$X1),
        "X2" = c(group1$X2, group2$X2)
      )
    
    # Set up grid
    x <- seq(-5, 5, length.out = 100)
    y <- seq(-5, 5, length.out = 100)
    grid <- expand.grid(x = x, y = y)
    colnames(grid) <- c("X1","X2")
    
    # Calculate densities values for dists
    pdf_values1 <- dmvnorm(grid, mean = mu1, sigma = sigma1)
    pdf_values2 <- dmvnorm(grid, mean = mu2, sigma = sigma1)
    
    # Reshape the density values to match the grid
    density1 <- matrix(pdf_values1, nrow = length(x))
    density2 <- matrix(pdf_values2, nrow = length(x))
    
    # Fit LDA model
    lda.out <- lda(G~.,dgp)
    
    # Get grid predictions
    prd = as.numeric(predict(lda.out, newdata = grid)$class)
    
    plot1 <-
      plot_ly(showlegend = FALSE)|>
      add_surface(
        x = x,
        y = y,
        z = density1,
        colors = "viridis",
        opacity = input$slider1,showlegend = FALSE
      )|> add_surface(
        x = x,
        y = y,
        z = density2,
        colors = "viridis",
        opacity = input$slider2,showlegend = FALSE
      ) |> add_markers(
        data = dgp,
        x = dgp$X1,
        y = dgp$X2,
        z = 0,
        opacity=1,
        color = ~dgp$G,
        marker = list(size = 4),showlegend = FALSE
      ) |> add_markers(
        x = grid$X1,
        y = grid$X2,
        z = 0,
        color=~prd,
        opacity=0.01,showlegend = FALSE
        
      ) |> layout(showLegend=F, legend = list())
    
   
    })
    
    output$gaussian_plot <- renderPlotly({
      plot_data()
    })
    
    
  }

shinyApp(ui, server)