# rsconnect::setAccountInfo(name='efriedlander', token='17AB86DD6AC747D5C9F63D6859126735', secret='G77GPVbTFfUHEqXfScVnW4waFa+ES0U4gbZrPzFa')
# https://efriedlander.shinyapps.io/ClassificationMetrics/
# https://efriedlander.shinyapps.io/BiasVarianceTradeOffApp/
# https://efriedlander.shinyapps.io/BVTappClassification/
library(shinydashboard)
library(tidyverse)
library(shiny)
# library(gridExtra)
library(ggpubr)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(Rfast)
library(shinyjs)
library(mvtnorm)
# library(DT)
library(gt)
library(MASS)
library(ggformula)
library(shinythemes)
library(shinyWidgets)
library(ipred)
library(randomForest)
library(grid)
library(patchwork)
library(glmnet)
library(e1071)
library(ranger)
library(memoise)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Bias-Variance Trade-Off (Classification)", titleWidth = 400),
  
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Regression Models", tabName = "regression_models",
      #          menuSubItem("Polynomial Regression", tabName = "polynomial"),
      #          menuSubItem("Smoothing Splines", tabName = "spline"),
      #          menuSubItem("KNN", tabName = "knn_reg"),
      #          menuSubItem("Single Tree", tabName = "tree_reg"),
      #          menuSubItem("Regularization", tabName = "lasso"),
      #          menuSubItem("Random Forest", tabName = "rf")
      # ),
      menuItem("Classification Models", tabName = "classification_models",
               menuSubItem("Logistic Regression", tabName = "logreg"),
               menuSubItem("KNN", tabName = "knn_class"),
               menuSubItem("Single Tree", tabName = "tree_class"),
               menuSubItem("SVM (Linear)", tabName = "svm"),
               menuSubItem("Random Forest", tabName = "rf_class")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ######## polynomial ########
      # tabItem(tabName = "polynomial", 
      #         
      #         h4(strong("Polynomial Regression")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", 
      #               selectInput(inputId = "dataset_polynomial",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_polynomial",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_polynomial",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong("Model Fit - This figure displays a training dataset and polynomial fits of different degrees. The true function that generated the data can also be visualized for comparison.")),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_polynomial_p1", height = 350), width = 8, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 4, status = "primary", 
      #             
      #             tags$style(HTML(".js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar {background: #D55E00}")),
      #             sliderInput("degrees_1", "Select a polynomial degree to fit:",
      #                         min = 1, max = 8, value = 1, step = 1), 
      #             
      #             tags$style(HTML(".js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar {background: #F0E442}")),
      #             sliderInput("degrees_2", "Select a different degree to fit:",
      #                         min = 1, max = 8, value = 2, step = 1),
      #             
      #             tags$style(HTML(".js-irs-15 .irs-single, .js-irs-15 .irs-bar-edge, .js-irs-15 .irs-bar {background: #56B4E9}")),
      #             sliderInput("degrees_3", "Select another degree to fit:",
      #                         min = 1, max = 8, value = 3, step = 1),
      #             checkboxInput("plot_true_polynomial", "Plot true function f(x)"),
      #             # checkboxInput("compare_polynomial", "Compare fits (max 3 at a time)")
      #             
      #           )
      #         ),
      #         
      #         h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training MSE (mean squared error), test MSE, squared bias, and variance for polynomial fits of different degrees. These results have been estimated from a test dataset and multiple training datasets simulated from the same distribution as the original training dataset above.")),
      #         
      #         fluidRow(
      #           box(width = 3, status = "danger", plotOutput("plot_polynomial_p2a", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_polynomial_p2b", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_polynomial_p2c", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_polynomial_p2d", height = 300))),
      #         
      #         h5(strong("Bias-Variance Plots - This figure displays the predictions from the multiple replicated training datasets. Users can visualize the bias by comparing the average fit with the true data generating function, and the variance by observing the variability in the replicated fits.")),
      #         
      #         fluidRow(box(width = 12, status = "warning", plotOutput("plot_polynomial_p3", height = 300)))
      # ),
      # 
      # ################
      # 
      # ######## spline ########
      # tabItem(tabName = "spline", 
      #         
      #         h4(strong("Smoothing Splines Regression")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", # COULD BE REPLACED BY box 
      #               selectInput(inputId = "dataset_spline",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_spline",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_spline",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong("Model Fit - This figure displays a training dataset and smoothing spline fits of different degrees of freedom. The true function that generated the data can also be visualized for comparison.")),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_spline_p1", height = 350), width = 8, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 4, status = "primary", 
      #             
      #             tags$style(HTML(".js-irs-17 .irs-single, .js-irs-17 .irs-bar-edge, .js-irs-17 .irs-bar {background: #D55E00}")),
      #             sliderInput("degrees_of_freedom_1", "Select a degrees of freedom to fit:",
      #                         min = 2, max = 92, value = 17, step = 15), 
      #             
      #             tags$style(HTML(".js-irs-18 .irs-single, .js-irs-18 .irs-bar-edge, .js-irs-18 .irs-bar {background: #F0E442}")),
      #             sliderInput("degrees_of_freedom_2", "Select a different degrees of freedom to fit:",
      #                         min = 2, max = 92, value = 32, step = 15),
      #             
      #             tags$style(HTML(".js-irs-19 .irs-single, .js-irs-19 .irs-bar-edge, .js-irs-19 .irs-bar {background: #56B4E9}")),
      #             sliderInput("degrees_of_freedom_3", "Select another degrees of freedom to fit:",
      #                         min = 2, max = 92, value = 47, step = 15),
      #             checkboxInput("plot_true_spline", "Plot true function f(x)"),
      #             
      #             # sliderInput("degrees_of_freedom", "Select degrees of freedom to plot:",   # USE SLIDER TEXT INPUT
      #             #             min = 2, max = 92, value = 20, step = 18),
      #             # checkboxInput("plot_true_spline", "Plot true function f(x)"),
      #             # checkboxInput("compare_spline", "Compare fits (max 3 at a time)")
      #             
      #           )
      #         ),
      #         
      #         h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training MSE (mean squared error), test MSE, squared bias, and variance for smoothing spline fits with different degrees of freedom. These results have been estimated from a test dataset and multiple training datasets simulated from the same distribution as the original training dataset above.")),
      #         
      #         fluidRow(
      #           box(width = 3, status = "danger", plotOutput("plot_spline_p2a", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_spline_p2b", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_spline_p2c", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_spline_p2d", height = 300))),
      #         
      #         h5(strong("Bias-Variance Plots - This figure displays the predictions from the multiple replicated training datasets. Users can visualize the bias by comparing the average fit with the true data generating function, and the variance by observing the variability in the replicated fits.")),
      #         
      #         fluidRow(box(width = 12, status = "warning", plotOutput("plot_spline_p3", height = 300)))
      # ),
      # 
      # ################
      # 
      # ######## knn_reg ########
      # tabItem(tabName = "knn_reg", 
      #         
      #         h4(strong("K-Nearest Neighbors Regression")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", # COULD BE REPLACED BY box 
      #               selectInput(inputId = "dataset_knn_reg",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_knn_reg",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_knn_reg",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong("Model Fit - This figure displays a training dataset and KNN regression fits for different K values. The true function that generated the data can also be visualized for comparison.")),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_knn_reg_p1", height = 350), width = 8, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 4, status = "primary", 
      #             
      #             tags$style(HTML(".js-irs-21 .irs-single, .js-irs-21 .irs-bar-edge, .js-irs-21 .irs-bar {background: #D55E00}")),
      #             sliderInput("k_values_1", "Select a K to fit:",
      #                         min = 1, max = 15, value = 3, step = 2), 
      #             
      #             tags$style(HTML(".js-irs-22 .irs-single, .js-irs-22 .irs-bar-edge, .js-irs-22 .irs-bar {background: #F0E442}")),
      #             sliderInput("k_values_2", "Select a different K to fit:",
      #                         min = 1, max = 15, value = 5, step = 2),
      #             
      #             tags$style(HTML(".js-irs-23 .irs-single, .js-irs-23 .irs-bar-edge, .js-irs-23 .irs-bar {background: #56B4E9}")),
      #             sliderInput("k_values_3", "Select another K to fit:",
      #                         min = 1, max = 15, value = 7, step = 2),
      #             checkboxInput("plot_true_knn_reg", "Plot true function f(x)"),
      #             
      #             # sliderInput("k_values", "Select K to plot:",
      #             #             min = 1, max = 15, value = 3, step = 2),
      #             # checkboxInput("plot_true_knn_reg", "Plot true function f(x)"),
      #             # checkboxInput("compare_knn_reg", "Compare fits (max 3 at a time)")
      #             
      #           )
      #         ),
      #         
      #         h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training MSE (mean squared error), test MSE, squared bias, and variance for KNN regression fits with different K values. These results have been estimated from a test dataset and multiple training datasets simulated from the same distribution as the original training dataset above.")),
      #         
      #         fluidRow(
      #           box(width = 3, status = "danger", plotOutput("plot_knn_reg_p2a", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_knn_reg_p2b", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_knn_reg_p2c", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_knn_reg_p2d", height = 300))),
      #         
      #         h5(strong("Bias-Variance Plots - This figure displays the predictions from the multiple replicated training datasets. Users can visualize the bias by comparing the average fit with the true data generating function, and the variance by observing the variability in the replicated fits.")),
      #         
      #         fluidRow(box(width = 12, status = "warning", plotOutput("plot_knn_reg_p3", height = 300)))
      # ),
      # 
      # ################
      # 
      # ######## tree_reg ########
      # tabItem(tabName = "tree_reg", 
      #         
      #         h4(strong("Single Regression Tree")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", # COULD BE REPLACED BY box 
      #               selectInput(inputId = "dataset_tree_reg",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_tree_reg",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_tree_reg",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong("Model Fit - This figure displays a training dataset and single regression tree fits of different depths. The true function that generated the data can also be visualized for comparison.")),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_tree_reg_p1a", height = 350), width = 8, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 4, status = "primary", 
      #             
      #             tags$style(HTML(".js-irs-25 .irs-single, .js-irs-25 .irs-bar-edge, .js-irs-25 .irs-bar {background: #D55E00}")),
      #             sliderInput("depths_1", "Select a tree depth to fit:",
      #                         min = 2, max = 14, value = 2, step = 2), 
      #             
      #             tags$style(HTML(".js-irs-26 .irs-single, .js-irs-26 .irs-bar-edge, .js-irs-26 .irs-bar {background: #F0E442}")),
      #             sliderInput("depths_2", "Select a different tree depth to fit:",
      #                         min = 2, max = 14, value = 4, step = 2),
      #             
      #             tags$style(HTML(".js-irs-27 .irs-single, .js-irs-27 .irs-bar-edge, .js-irs-27 .irs-bar {background: #56B4E9}")),
      #             sliderInput("depths_3", "Select another tree depth to fit:",
      #                         min = 2, max = 14, value = 6, step = 2),
      #             checkboxInput("plot_true_tree_reg", "Plot true function f(x)"),
      #             
      #             # sliderInput("depths", "Select tree depth to plot:",
      #             #             min = 2, max = 12, value = 2, step = 2),
      #             # checkboxInput("plot_true_tree_reg", "Plot true function f(x)"),
      #             # checkboxInput("compare_tree_reg", "Compare fits (max 3 at a time)")
      #             
      #           )
      #         ),
      #         
      #         h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training MSE (mean squared error), test MSE, squared bias, and variance for single regression tree fits of different depths. These results have been estimated from a test dataset and multiple training datasets simulated from the same distribution as the original training dataset above.")),
      #         
      #         fluidRow(
      #           box(width = 3, status = "danger", plotOutput("plot_tree_reg_p2a", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_tree_reg_p2b", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_tree_reg_p2c", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_tree_reg_p2d", height = 300))),
      #         
      #         h5(strong("Bias-Variance Plots - This figure displays the predictions from the multiple replicated training datasets. Users can visualize the bias by comparing the average fit with the true data generating function, and the variance by observing the variability in the replicated fits.")),
      #         
      #         fluidRow(box(width = 12, status = "warning", plotOutput("plot_tree_reg_p3", height = 300)))
      # ),
      # 
      # 
      # 
      # ################
      # 
      # ######## lasso ########
      # tabItem(tabName = "lasso", 
      #         
      #         h4(strong("Ridge (L2) Regularization")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", # COULD BE REPLACED BY box 
      #               selectInput(inputId = "dataset_lasso",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_lasso",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_lasso",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong(div(HTML("Model Fit - This figure displays a training dataset and regularized model fits for different values of the regularization parameter &lambda;. The models are based on a fifth degree polynomial. The true function that generated the data can also be visualized for comparison.")))),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_lasso_p1", height = 350), width = 8, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 4, status = "primary", 
      #             
      #             tags$style(HTML(".js-irs-29 .irs-single, .js-irs-29 .irs-bar-edge, .js-irs-29 .irs-bar {background: #D55E00}")),
      #             sliderInput("lambda_lasso_1", div(HTML("Select a &lambda; value to fit (power of 10):")),
      #                         min = -4, max = 3, value = -2, step = 1),
      # 
      #             tags$style(HTML(".js-irs-30 .irs-single, .js-irs-30 .irs-bar-edge, .js-irs-30 .irs-bar {background: #F0E442}")),
      #             sliderInput("lambda_lasso_2", div(HTML("Select a different &lambda; value to fit (power of 10):")),
      #                         min = -4, max = 3, value = -1, step = 1),
      # 
      #             tags$style(HTML(".js-irs-31 .irs-single, .js-irs-31 .irs-bar-edge, .js-irs-31 .irs-bar {background: #56B4E9}")),
      #             sliderInput("lambda_lasso_3", div(HTML("Select another &lambda; value to fit (power of 10):")),
      #                         min = -4, max = 3, value = 0, step = 1),
      #             checkboxInput("plot_true_lasso", "Plot true function f(x)"),
      #             
      #             # sliderTextInput("lambda_lasso", "Select regularization parameter:",
      #             #                 choices = c(0, 10^seq(-3,1,1)), selected = 1, grid = TRUE),
      #             # checkboxInput("plot_true_lasso", "Plot true function f(x)"),
      #             # checkboxInput("compare_lasso", "Compare fits (max 3 at a time)")
      #             
      #           )
      #         ),
      #         
      #         h5(strong(div(HTML("Bias-Variance Trade-Off - These figures display the behavior of the training MSE (mean squared error), test MSE, squared bias, and variance for regularized model fits (based on a fifth degree polynomial) with different values of the regularization parameter &lambda;. These results have been estimated from a test dataset and multiple training datasets simulated from the same distribution as the original training dataset above.")))),
      #         
      #         fluidRow(
      #           box(width = 3, status = "danger", plotOutput("plot_lasso_p2a", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_lasso_p2b", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_lasso_p2c", height = 300)),
      #           box(width = 3, status = "danger", plotOutput("plot_lasso_p2d", height = 300))),
      #         
      #         h5(strong(div(HTML("Bias-Variance Plots - The first figure displays the predictions from the multiple replicated training datasets. Users can visualize the bias by comparing the average fit with the true data generating function, and the variance by observing the variability in the replicated fits. In the second figure, users can visualize the behavior of the coefficient estimates, the &beta;'s of a fifth degree polynomial, as obtained from the replicated datasets.")))),
      #         
      #         fluidRow(box(width = 12, status = "warning", plotOutput("plot_lasso_p3a", height = 300)),
      #                  box(width = 12, status = "warning", plotOutput("plot_lasso_p3b", height = 300)))
      # ),
      # 
      # ################
      # 
      # ######## rf ########
      # tabItem(tabName = "rf", 
      #         
      #         h4(strong("Random Forest Regression")),
      #         
      #         fluidRow(
      #           box(width = 4, status = "success", # COULD BE REPLACED BY box 
      #               selectInput(inputId = "dataset_rf",
      #                           label = "Select a dataset:",
      #                           choices = c("Dataset 1", "Dataset 2","Dataset 3", "Dataset 4", "Dataset 5"),
      #                           selected = "Dataset 1")),
      #           
      #           box(width = 4, status = "success",
      #               sliderInput(inputId = "num_points_rf",
      #                           label = "Select number of training data points:",
      #                           min = 100,
      #                           max = 500,
      #                           value = 100,
      #                           step = 200)),
      #           
      #           box(width = 4, status = "success",
      #               sliderTextInput(inputId = "epsilon_rf",
      #                               label = "Select noise level:",
      #                               choices = c("Low", "Medium", "High"),
      #                               selected = "Low",
      #                               grid = TRUE))
      #         ),
      #         
      #         h5(strong("Bootstrapping - This figure displays six individual trees fit to bootstrapped training datasets. For clarity, thirty observations from the original training data have been considered. Users can visualize the number of times each training data point appears in the bootstrapped sample, and the individual trees that form the random forest.")),
      #         
      #         fluidRow(
      #           box(plotOutput("plot_rf_p1", height = 300), width = 9, 
      #               status = "primary"),
      #           
      #           box(
      #             width = 3, status = "primary", 
      #             # sliderInput("degrees", "Select rf degree to plot:",
      #             #             min = 1, max = 6, value = 1, step = 1),
      #             checkboxInput("plot_true_rf", "Plot true function f(x)"),
      #             checkboxInput("plot_rf_trees", "Plot individual trees", value = TRUE),
      #             
      #           )
      #         ),
      #         
      #         h5(strong("Bias-Variance Plots - This figure displays the predictions from the multiple replicated training datasets obtained using a single regression tree and a random forest model. Users can visualize that a random forest provides an improvement over a single tree by reducing the variability in the predictions.")),
      #         
      #         fluidRow(
      #           # box(width = 4, status = "danger", plotOutput("plot_rf_p2a", height = 250)),
      #           # box(width = 4, status = "danger", plotOutput("plot_rf_p2b", height = 250)),
      #           box(width = 12, status = "danger", plotOutput("plot_rf_p2c", height = 333))),
      #         
      #         # h5(strong("Replicated Datasets")),
      #         # 
      #         # fluidRow(box(width = 12, status = "warning", plotOutput("plot_rf_p3", height = 300)))
      # ),
      # 
      # ################
      # 
      # 
      
      
      ######## logreg ########
      tabItem(tabName = "logreg", 
              
              h4(strong("Logistic Regression")),
              
              fluidRow(
                box(width = 4,   status = "success", # COULD BE REPLACED BY box 
                    sliderInput(inputId = "num_points_class_logreg",
                                label = "Select number of training data points:",
                                min = 20,
                                max = 100,
                                value = 60,
                                step = 40)),
                
                
                box(width = 4, status = "success",
                    sliderTextInput(inputId = "epsilon_logreg",
                                    label = "Select noise level:",
                                    choices = c("Low", "Medium", "High"),
                                    selected = "Low",
                                    grid = TRUE)),
                
                box(width = 3, status = "success",
                    actionButton(inputId = "action_logreg",
                                 label = "Generate Data"))
                
              ),
              
              h5(strong("Model Fit - These figures display a training dataset and the decision boundary of the logistic regression classifier.")),
              
              fluidRow(
                box(plotOutput("plot_logreg_p1a", height = 250), width = 5, status = "primary"),
                
                box(plotOutput("plot_logreg_p1b", height = 250), width = 5, status = "primary")
                
                # box(
                #   width = 4, status = "primary", background = "blue", 
                #   sliderInput("degrees", "Select polynomial degree to plot:",
                #               min = 1, max = 6, value = 1, step = 1),
                #   checkboxInput("plot_true_polynomial", "Plot true function f(x)"),
                #   checkboxInput("compare_polynomial", "Compare fits (max 3 at a time)")
                
                # )
              ),
              
              h5(strong("Bias-Variance Trade-Off - This table displays the training error (misclassification rate), test error, squared bias, and variance for the logistic regression classifier.")),
              
              fluidRow(
                column(12, 
                       # DT::dataTableOutput("table_logreg_p2")
                       gt_output("table_logreg_p2"))),
              fluidRow(
                column(12, 
                       h5(strong(textOutput("text_logreg")))))
              
              # h5(strong("Replicated Datasets")),
              # 
              # fluidRow(box(width = 12, status = "warning", plotOutput("plot_polynomial_p3", height = 300)))
      ),
      
      ################
      
      ######## knn_class ########
      tabItem(tabName = "knn_class", 
              
              h4(strong("K-Nearest Neighbors Classification")),
              
              fluidRow(
                box(width = 4,   status = "success", # COULD BE REPLACED BY box 
                    sliderInput(inputId = "num_points_class_knn",
                                label = "Select number of training data points:",
                                min = 20,
                                max = 100,
                                value = 60,
                                step = 40)),
                
                
                box(width = 4, status = "success",
                    sliderTextInput(inputId = "epsilon_knn_class",
                                    label = "Select noise level:",
                                    choices = c("Low", "Medium", "High"),
                                    selected = "Low",
                                    grid = TRUE)),
                
                box(width = 3, status = "success",
                    actionButton(inputId = "action_knn_class",
                                 label = "Generate Data"))
                
              ),
              
              h5(strong("Model Fit - These figures display a training dataset and the decision boundary of the nearest-neighbor classifier for different values of K.")),
              
              fluidRow(
                box(plotOutput("plot_knn_class_p1a", height = 250), width = 5, status = "primary"),
                
                box(plotOutput("plot_knn_class_p1b", height = 250), width = 5, status = "primary"),
                
                box(width = 2, status = "primary", sliderInput("k_values_class", "Select K:",
                                                               min = 1, max = 15, value = 1, step = 2))
                
                # box(
                #   width = 4, status = "primary", background = "blue", 
                #   sliderInput("degrees", "Select polynomial degree to plot:",
                #               min = 1, max = 6, value = 1, step = 1),
                #   checkboxInput("plot_true_polynomial", "Plot true function f(x)"),
                #   checkboxInput("compare_polynomial", "Compare fits (max 3 at a time)")
                
                # )
              ),
              
              h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training error (misclassification rate), test error, squared bias, and variance for the nearest-neighbor classifier with different values of K.")),
              
              fluidRow(
                # column(4, plotOutput("plot_knn_class_p2a"))
                # box(width = 4, status = "danger", plotOutput("plot_knn_class_p2c", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_knn_class_p2a", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_knn_class_p2b", height = 250)),
                box(width = 12, status = "danger", plotlyOutput("plot_knn_class_p2", height = 350))),
              fluidRow(
                column(12, 
                       h5(strong(textOutput("text_knn_class")))
                )
              )
              
              # h5(strong("Replicated Datasets")),
              # 
              # fluidRow(box(width = 12, status = "warning", plotOutput("plot_polynomial_p3", height = 300)))
      ),
      
      ################
      
      ######## tree_class ########
      tabItem(tabName = "tree_class", 
              
              h4(strong("Single Classification Tree")),
              
              fluidRow(
                box(width = 4,   status = "success", # COULD BE REPLACED BY box 
                    sliderInput(inputId = "num_points_class_tree",
                                label = "Select number of training data points:",
                                min = 20,
                                max = 100,
                                value = 60,
                                step = 40)),
                
                
                box(width = 4, status = "success",
                    sliderTextInput(inputId = "epsilon_tree_class",
                                    label = "Select noise level:",
                                    choices = c("Low", "Medium", "High"),
                                    selected = "Low",
                                    grid = TRUE)),
                
                box(width = 3, status = "success",
                    actionButton(inputId = "action_tree_class",
                                 label = "Generate Data"))
                
                
              ),
              
              h5(strong("Model Fit - These figures display a training dataset and the decision boundary of the single classification tree of different depths.")),
              
              fluidRow(
                box(plotOutput("plot_tree_class_p1a", height = 250), width = 5, status = "primary"),
                
                box(plotOutput("plot_tree_class_p1b", height = 250), width = 5, status = "primary"),
                
                box(width = 2, status = "primary", sliderInput("depths_class", "Select tree depth:",
                                                               min = 2, max = 14, value = 2, step = 2))
                
           ),
              
              h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training error (misclassification rate), test error, squared bias, and variance for the single classification tree of different depths.")),
              
              fluidRow(
                # box(width = 4, status = "danger", plotOutput("plot_tree_class_p2c", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_tree_class_p2a", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_tree_class_p2b", height = 250)),
                box(width = 12, status = "danger", plotlyOutput("plot_tree_class_p2", height = 350))),
              fluidRow(
                column(12, 
                       h5(strong(textOutput("text_tree_class")))
                )
              )
              
      ),
      
      ################
 
      ######## svm ########
      tabItem(tabName = "svm", 
              
              h4(strong("Support Vectors Classification")),
              
              fluidRow(
                box(width = 4,   status = "success", # COULD BE REPLACED BY box 
                    sliderInput(inputId = "num_points_class_svm",
                                label = "Select number of training data points:",
                                min = 20,
                                max = 100,
                                value = 60,
                                step = 40)),
                
                
                box(width = 4, status = "success",
                    sliderTextInput(inputId = "epsilon_svm",
                                    label = "Select noise level:",
                                    choices = c("Low", "Medium", "High"),
                                    selected = "Low",
                                    grid = TRUE)),
                
                box(width = 3, status = "success",
                    actionButton(inputId = "action_svm",
                                 label = "Generate Data"))
                
              ),
              
              h5(strong("Model Fit - These figures display a training dataset and the decision boundary of the SVM classifier for different values of C.")),
              
              fluidRow(
                box(plotOutput("plot_svm_p1a", height = 250), width = 5, status = "primary"),
                
                box(plotOutput("plot_svm_p1b", height = 250), width = 5, status = "primary"),
                
                box(width = 2, status = "primary", sliderTextInput("C_values", "Select C:",
                                                                   choices = c(0.01, 0.1, 1, 10, 100),
                                                                   selected = 1, grid = TRUE))
                
              ),
              
              h5(strong("Bias-Variance Trade-Off - These figures display the behavior of the training error (misclassification rate), test error, squared bias, and variance for the SVM classifier with different values of C.")),
              
              fluidRow(
                # box(width = 4, status = "danger", plotOutput("plot_svm_p2c", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_svm_p2a", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_svm_p2b", height = 250)),
                box(width = 12, status = "danger", plotlyOutput("plot_svm_p2", height = 350))),
              fluidRow(
                column(12, 
                       h5(strong(textOutput("text_svm")))
                )
              )
              
      ),
      
      ################
      
      ######## rf_class ########
      tabItem(tabName = "rf_class", 
              
              h4(strong("Random Forest Classification")),
              
              fluidRow(
                box(width = 4,   status = "success", # COULD BE REPLACED BY box 
                    sliderInput(inputId = "num_points_class_rf",
                                label = "Select number of training data points:",
                                min = 20,
                                max = 100,
                                value = 60,
                                step = 40)),
                
                
                box(width = 4, status = "success",
                    sliderTextInput(inputId = "epsilon_rf_class",
                                    label = "Select noise level:",
                                    choices = c("Low", "Medium", "High"),
                                    selected = "Low",
                                    grid = TRUE)),
                
                box(width = 3, status = "success",
                    actionButton(inputId = "action_rf_class",
                                 label = "Generate Data"))
                
              ),
              
              h5(strong("Model Fit - These figures display a training dataset and the decision boundary of the random forest classifier.")),
              
              fluidRow(
                box(plotOutput("plot_rf_class_p1a", height = 250), width = 5, status = "primary"),
                
                box(plotOutput("plot_rf_class_p1b", height = 250), width = 5, status = "primary"),
                
                # box(width = 2, status = "primary", sliderInput("k_values_class", "Select K:",
                #                                                min = 1, max = 81, value = 1, step = 20))
                
              ),
              
              h5(strong("Individual Trees - This figure displays six individual trees from the random forest classifier built on bootstrapped training samples. Users can visualize the dissimilarity between individual trees (decorrelating) which leads to a reduction in the variance of the random forest classifier over a single classification tree.")),
              
              fluidRow(
                # box(width = 4, status = "danger", plotOutput("plot_rf_class_p2a", height = 250)),
                # box(width = 4, status = "danger", plotOutput("plot_rf_class_p2b", height = 250)),
                box(width = 12, status = "danger", plotOutput("plot_rf_class_p2c", height = 400))),
              fluidRow(
                column(12, 
                       h5(strong(textOutput("text_rf_class")))
                )
              )
              
      )
      
      ################
      
      
    )
  )
)


      