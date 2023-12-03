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
    menuItem(tabName = "introduction", "Introduction"),
    menuItem(tabName = "simulation", "Simulation"),
    menuItem(tabName = "eda", "EDA"),
    menuItem(tabName = "pca & nmf", "PCA & NMF")
  )),
  dashboardBody(tabItems(tabItem(tabName = "introduction", box(
    h3(
      withMathJax(
        "Goal of classifcation is the maximization of:
        
        $$P(Y=j | X = x_0)$$
        
        Given multivaraite normality on the groups we assume the following model:
        
        $$P(Y=j|X=x_0)=\\frac{\\pi_j f(x_0 | \\mu_j, \\Sigma_j)}{\\sum^G_{g=1}\\pi_g f(x_0|\\mu_g,\\Sigma_g)}$$
        
        
        Regularized Discriminant Analysis imposes the following constraints:
        
        $$ \\begin{align}
                                                    \\hat{\\Sigma}_k(\\lambda) &:= (1-\\lambda)\\hat{\\Sigma}_k+\\lambda\\hat{\\Sigma} \\\\
                                  \\hat{\\Sigma}_k(\\lambda,\\gamma) &= (1-\\gamma)\\hat{\\Sigma}_k(\\lambda) + \\gamma \\frac{1}{d} \\text{tr}[\\hat{\\Sigma}_k(\\lambda)]I \\\\

                                                   \\end{align} $$
        
        Resulting in a compromise between LDA and QDA. The extremes of the constraints give:
        
        $$ \\begin{align} 
                       (\\gamma = 0, \\lambda = 0)&\\text{: QDA} \\\\
                       (\\gamma = 0, \\lambda = 1)&\\text{: LDA} \\\\
                       (\\gamma = 1, \\lambda = 0)&\\text{: Independent QDA} \\\\
                       (\\gamma = 1, \\lambda = 1)&\\text{: Independent LDA} 
                       \\end{align} $$"
      )
    ), width = 16
  )), 
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
                       (\\gamma = 1, \\lambda = 1)&\\text{: Independent LDA} 
                       \\end{align} $$")),numericInput("lambda", "Change lambda:", value = 0),
        numericInput("gamma", "Change gamma:", value = 0),
        actionButton("regenerate", "(Re)generate Plot"),
        width = 4
      ),
      box(
        matrixInput(
          "matrix1",
          label = "G1",
          value = matrix(
            c(2, 0, 0, 3),
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
            c(0.5, 0, 0, 0.5),
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
        ),
        sliderInput(
          "slider4",
          "Slider 4:",
          min = 0,
          max = 1,
          value = 1
        ),width = 4
      )
    ),
    tabItem(tabName = "eda",fluidRow(
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
    )),tabItem(tabName = "pca & nmf",fluidRow(
      box(selectInput("diminput", "Select Dim", choices =c("PCA","NMF")),
          tableOutput("dimout"),width = 5)
      ,box(plotlyOutput("plot2"),width = 8),box(tableOutput("lam"),width = 3))
  ))
))
   
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

    # Get mixture model density
    unpooled_sigma <- rda.out$covariances
    mean_mle <- rda.out$means
    pooled_sigma <- rda.out$covpooled
    gamma <- rda.out$regularization[1]
    lambda <-  rda.out$regularization[2]
    
    sigma_lambda_1 <- (1- lambda) * unpooled_sigma[,,1] + lambda * pooled_sigma
    sigma_lambda_2 <- (1- lambda) * unpooled_sigma[,,2] + lambda * pooled_sigma
    sigma_lambda_3 <- (1- lambda) * unpooled_sigma[,,3] + lambda * pooled_sigma
    
    d <- 2
    sigma_mixed_1 <- (1 - gamma) * sigma_lambda_1 + gamma * (1/d) * sum(diag((sigma_lambda_1))) *  diag(2)
    sigma_mixed_2 <- (1 - gamma) * sigma_lambda_2 + gamma * (1/d) * sum(diag((sigma_lambda_2))) *  diag(2)
    sigma_mixed_3 <- (1 - gamma) * sigma_lambda_3 + gamma * (1/d) * sum(diag((sigma_lambda_3))) *  diag(2)
    
    # rda.out
    
    # Assuming `data` is your simulated dataset
    pdf_group1 <- dmvnorm(grid, mean = mean_mle[,1], sigma = sigma_mixed_1)
    pdf_group2 <- dmvnorm(grid, mean = mean_mle[,2], sigma = sigma_mixed_2)
    pdf_group3 <- dmvnorm(grid, mean = mean_mle[,3], sigma = sigma_mixed_3)
    
    prior_group1 <- rda.out$prior[1] 
    prior_group2 <- rda.out$prior[2]
    prior_group3 <- rda.out$prior[3]
    
    mixed_pdf <-  prior_group1*pdf_group1 + prior_group2*pdf_group2 + prior_group3*pdf_group3
    
    mixed_density <- matrix(mixed_pdf,nrow=length(x))
    
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
      )|> add_surface(
        x = x,
        y = y,
        z = mixed_density,
        colors = "viridis",
        opacity = input$slider4,
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
  
  
  # Tab3
  output$dimout <- renderTable({
    selected_method <- input$diminput
    
    if(selected_method == "PCA"){
      pca.out <- prcomp(bc[,-1],scale. = TRUE)
      scores <- as.data.frame(pca.out$x)[,1:2]
      
      cum_prop_var <- summary(pca.out)$importance[3,2]
      loadi <- round(pca.out$rotation[,1:2],2)
      
      
      cbind(`row names` = rownames(loadi), loadi)
      
    }else if(selected_method == "NMF"){
      set.seed(87460945)
      # NMF preds
      bc_temp <- scale(bc[,-1], center = FALSE, scale = TRUE) 
      nmf.out <- nmf(bc_temp,2)
      scores <- as.data.frame(nmf.out$w)
      loadi.nmf <- round(nmf.out$h,2)
      
      
      cbind(`row names` = rownames(t(loadi.nmf)), t(loadi.nmf))
      }
    
  })
  
  # NMF
  #x: the non-negative data (if any negatives exist, the whole data is shifted)
  #q: the number of factors/bases
  #eps: convergence criteria (lack of progress on sum squared error)
  #maxit: convergence criteria (max iteration when lack of progress not met)
  #w: n by q scores on factors
  #h: q by p observed factors/bases 
  #By default (when w, h NULL) both w and h are initialized randomly
  nmf <- function(x, q, eps=0.001, maxit=2000, w=NULL, h=NULL){
    n <- nrow(x)
    p <- ncol(x)
    if(any(x<0)){x <- as.matrix(x)+abs(min(x))}
    else{x <- as.matrix(x)}
    if(is.null(w)){
      w <- matrix(runif(n*q, min(x), max(x)), n, q)
    }
    if(is.null(h)){
      h <- matrix(runif(p*q, min(x), max(x)), q, p)
    }
    ed <- sum((x-w%*%h)^2)
    conv <- FALSE
    ctr <- 1
    while(!conv){
      ctr <- ctr+1
      h <- h * (t(w) %*% x) / (t(w) %*% w %*% h) 
      w <- w * (x %*% t(h)) / (w %*% h %*% t(h))
      wh <- w%*%h
      ed[ctr] <- sum((x-wh)^2)
      if((ed[ctr-1]-ed[ctr] < eps)|(ctr==maxit)){
        conv <- TRUE
      }
    }
    list(ed=ed, w=w, h=h, x=x)
  }
  
  
  
  output$lam <- renderTable({
    selected_method <- input$diminput
    if(selected_method == "PCA"){
      
      # PCA preds
      pca.out <- prcomp(bc[,-1],scale. = TRUE)
      scores <- as.data.frame(pca.out$x)[,1:2]
      df <- cbind(scores,bc$diagnosis)
      colnames(df)[3] <- "diagnosis"
      rda_out.pca <- rda(diagnosis~.,df)
      
      
      
      data.frame("Gamma" = rda_out.pca$regularization[1], "Lambda" = rda_out.pca$regularization[2])
      
    } else if(selected_method == "NMF"){
      
      set.seed(87460945)
      # NMF preds
      bc_temp <- scale(bc[,-1], center = FALSE, scale = TRUE) 
      nmf.out <- nmf(bc_temp,2)
      scores <- as.data.frame(nmf.out$w)
      scores$diagnosis <- bc$diagnosis
      rda_out.nmf <- rda(diagnosis~.,scores)
      
      data.frame("Gamma" = rda_out.nmf$regularization[1], "Lambda" = rda_out.nmf$regularization[2])
      
    }
  })
  
  output$plot2 <- renderPlotly({
    selected_method <- input$diminput
   if(selected_method == "PCA"){
     
     # PCA preds
     pca.out <- prcomp(bc[,-1],scale. = TRUE)
     scores <- as.data.frame(pca.out$x)[,1:2]
     df <- cbind(scores,bc$diagnosis)
     colnames(df)[3] <- "diagnosis"
     rda_out.pca <- rda(diagnosis~.,df)
     x <- seq(-10, 4, length.out = 100)
     y <- seq(-8, 4, length.out = 100)
     grid <- expand.grid(x = x, y = y)
     colnames(grid) <- c("PC1", "PC2")
     
     # Get grid predictions
     prd = as.numeric(predict(rda_out.pca, newdata = grid)$class)
     
     
 
     
     plot1 <-
       plot_ly()|> add_markers(
         data = scores,
         x = scores$PC1,
         y = scores$PC2,
         z = 0,
         opacity = 1,
         color = ~ bc$diagnosis,
         marker = list(size = 4),
         colors = "viridis",
         showlegend = FALSE
       ) |> add_markers(
         x = grid$PC1,
         y = grid$PC2,
         z = 0,
         color = ~prd,
         opacity=0.03,
         showlegend = FALSE
       )
     
     
     
   } else if(selected_method == "NMF"){
     
     set.seed(87460945)
     # NMF preds
     bc_temp <- scale(bc[,-1], center = FALSE, scale = TRUE) 
     nmf.out <- nmf(bc_temp,2)
     scores <- as.data.frame(nmf.out$w)
     scores$diagnosis <- bc$diagnosis
     rda_out.nmf <- rda(diagnosis~.,scores)
     
     x <- seq(0, 8, length.out = 100)
     y <- seq(0, 4, length.out = 100)
     grid2 <- expand.grid(x = x, y = y)
     colnames(grid2) <- c("V1", "V2")
     
     # Get grid predictions
     prd = as.numeric(predict(rda_out.nmf, newdata = grid2)$class)
     
     
     
     plot1 <-
       plot_ly()|> add_markers(
         data = scores,
         x = scores$V1,
         y = scores$V2,
         z = 0,
         opacity = 1,
         color = ~ bc$diagnosis,
         marker = list(size = 4),
         colors = "viridis",
         showlegend = FALSE
       ) |> add_markers(
         x = grid2$V1,
         y = grid2$V2,
         z = 0,
         color = ~prd,
         opacity=0.01,
         showlegend = FALSE
       )

     
   }

  })
  

  
  # # NMF preds
  # scores <- as.data.frame(nmf.out$w)
  # df <- cbind(scores,bc$diagnosis)
  # colnames(df)[3] <- "diagnosis"
  # rda_out.nmf <- rda(diagnosis~.,df)
  
}
# thematic_shiny()
shinyApp(ui, server)