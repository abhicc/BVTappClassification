num_rep_sets_class <- 50    # number of replicated datasets
# num_training_points_class <-  100
num_test_points_class <-  600

generate_data_classification <- function(mu0, mu1, num_training_points, num_test_points, num_rep_sets, input_noise_sd)
{
  # noise_sd <- input_noise_sd          # standard deviation of epsilon/noise (USER CAN CHANGE THIS - 1, 4, 7, 10)
  noise_sd <- case_when(input_noise_sd == "Low" ~ 0.05, 
                        input_noise_sd == "Medium" ~ 0.3,
                        input_noise_sd == "High" ~ 0.8)
  
  mu_blue <- mu0
  mu_orange <- mu1

  cov_mat_1 <- matrix(c(1,0,0,1), nrow=2, ncol=2)
  
  means_blue <- Rfast::rmvnorm(n = 10, mu = mu_blue, sigma = cov_mat_1)
  means_orange <- Rfast::rmvnorm(n = 10, mu = mu_orange, sigma = cov_mat_1)
  
  
  # REPLICATED TRAINING DATASETS
  x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
  x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
  y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
  total_iter_train <- num_training_points/2
  
  for(j in 1:num_rep_sets)
  {
    
    rep_training_obs_blue <- matrix(nrow=num_training_points/2, ncol=2)
    rep_training_obs_orange <- matrix(nrow=num_training_points/2, ncol=2)
    cov_mat_2 <- matrix(c(noise_sd,0,0,noise_sd), nrow=2, ncol=2)
    s_train <- sample(1:10, size = num_training_points, prob = rep(1/10, 10), replace = TRUE)
    
    
    for(i in 1:total_iter_train)
    {
      # SCENARIO 2 - USING MIXTURES
      # s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      rep_training_obs_blue[i,] <- Rfast::rmvnorm(n=1, mu=means_blue[s_train[i],],
                                                  sigma = cov_mat_2)
      
      # s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      rep_training_obs_orange[i,] <- Rfast::rmvnorm(n=1, mu=means_orange[s_train[total_iter_train+i],],
                                                    sigma = cov_mat_2)
    }
    x1_matrix[, j] <- c(rep_training_obs_blue[,1], rep_training_obs_orange[,1])
    x2_matrix[, j] <- c(rep_training_obs_blue[,2], rep_training_obs_orange[,2])
    y_matrix[, j] <- rep(c("0", "1"), each=total_iter_train)
    # DON'T NEED Y_MATRIX, COULD JUST HAVE A VECTOR INSTEAD
    
  }
  
  df_training <- data.frame(cbind(x1_matrix[,1], x2_matrix[,1]),
                            y_orig = factor(y_matrix[,1],levels =c("0", "1")))
  names(df_training) <- c("x1", "x2", "y_orig")
  
  

  
  
  # TEST DATASET
  test_obs_blue <- matrix(nrow=num_test_points/2, ncol=2)
  test_obs_orange <- matrix(nrow=num_test_points/2, ncol=2)
  cov_mat_2 <- matrix(c(noise_sd,0,0,noise_sd), nrow=2, ncol=2)
  s_test <- sample(1:10, size = num_test_points, prob = rep(1/10, 10), replace = TRUE)
  total_iter_test <- num_test_points/2
  
  for(i in 1:total_iter_test)
  {
    # SCENARIO 2 - USING MIXTURES
    # s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    test_obs_blue[i,] <- Rfast::rmvnorm(n=1, mu=means_blue[s_test[i],],
                                        sigma = cov_mat_2)
    
    # s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    test_obs_orange[i,] <- Rfast::rmvnorm(n=1, mu=means_orange[s_test[total_iter_test+i],],
                                          sigma = cov_mat_2)
  }
  df_test_mat <- rbind(test_obs_blue, test_obs_orange)
  df_test <- data.frame(rbind(test_obs_blue, test_obs_orange),
                        y_orig = factor(rep(c("0", "1"),each=total_iter_test),
                                        levels =c("0", "1")))
  names(df_test) <- c("x1", "x2", "y_orig")
  
  
  # CALCULATING THE BAYES ERROR RATE FOR TEST DATA
  df_test_probs <- matrix(nrow = nrow(df_test_mat), ncol = 2)
  cov_mat_2 <- matrix(c(noise_sd,0,0,noise_sd), nrow=2, ncol=2)
  
  for(i in 1:nrow(df_test_mat))
  {
    obs <- df_test_mat[i,]
    d_blue <- numeric(10); d_orange <- numeric(10)
    
    for(k in 1:10)
    {
      d_blue[k] <- Rfast::dmvnorm(x = obs, mu = means_blue[k,],
                                  sigma = cov_mat_2)
      d_orange[k] <- Rfast::dmvnorm(x = obs, mu = means_orange[k,],
                                    sigma = cov_mat_2)
    }
    
    df_test_probs[i,] <- c(mean(d_blue)/(mean(d_blue)+mean(d_orange)),
                           mean(d_orange)/(mean(d_blue)+mean(d_orange)))
  }
  
  # bayes_error <- 1 - pmax(df_test_probs[,1], df_test_probs[,2])
  # bayes_error <- apply(df_test_probs, 1, function(x) 1-max(x))
  bayes_error <- mean(df_test$y_orig != apply(df_test_probs, 1, function(x) which.max(x)-1))
  
  
  # GENERATING GRID TO PLOT
  alldf <- expand.grid(xeg=seq(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3, length.out = 200),
                       yeg=seq(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3, length.out = 200))
  
  
  return(list(df_training, x1_matrix, x2_matrix, y_matrix, df_test, df_test_probs, mean(bayes_error), alldf))
}

generate_data_classification_m <- memoise(generate_data_classification, cache = getShinyOption("cache"))


bias_variance_error_plots <- function(df_data, x_axis_lab) {
  
  results <- df_data[[1]]

 
  
  error_plot <- plot_ly(data = results, x = ~parameter_values, y = ~Test, 
                      name = 'Test Error', type = "scatter", mode = "lines+markers", color = I("#000000")) %>% 
    add_trace(y = ~Training, name = 'Training Error', mode = 'lines+markers', color = I("#E69F00")) %>% 
    layout(xaxis = list(title = x_axis_lab, showline = TRUE, zeroline = FALSE),
           yaxis = list(title = "Misclassification Error Rate", showline = TRUE, zeroline = FALSE),
           legend = list(orientation = 'h', x = 0.1, y = 1.3)) 
  
  

  bias_plot <- plot_ly(data = results, x = ~parameter_values, y = ~bias_sq_kw, 
                       name = 'Squared Bias', type = "bar", color = I("#CC79A7")) %>% 
    # add_trace(y = ~Variance, name = 'Variance', type = "bar", color = I("#009E73")) %>%
    # add_trace(y = ~Variance, name = 'Variance', mode = 'lines+markers', color = I("#009E73")) %>% 
    layout(xaxis = list(title = x_axis_lab, showline = TRUE, zeroline = FALSE),
           yaxis = list(title = "Approximate Squared Bias", showline = TRUE, zeroline = FALSE))
  
  

  variance_plot <- plot_ly(data = results, x = ~parameter_values, y = ~var_kw, 
                           name = 'Variance', type = "bar", color = I("#009E73")) %>% 
    layout(xaxis = list(title = x_axis_lab, showline = TRUE, zeroline = FALSE),
           yaxis = list(title = "Approximate Variance", showline = TRUE, zeroline = FALSE))
  
  
  
  allplots2 <- subplot(error_plot, bias_plot, variance_plot, nrows = 1, shareX = TRUE)
  
  
  return(allplots2)
  
}

bias_variance_error_plots_m <- memoise(bias_variance_error_plots, cache = getShinyOption("cache"))


function(input, output, session) {
  
  # DO YOU REALLY WANT TO START FROM BEGINNING EVERY TIME A USER CHANGES MODELS
  # OR DO YOU KEEP THE INPUTS AS THEY WERE
  # INCLUDE A BUTTON TO RESET INPUTS
  # observeEvent(input$models, {
  #     updateSelectInput(inputId = "dataset", selected = "Dataset 1")
  #     updateSliderInput(inputId = "num_points", value = 100)
  #     updateSliderInput(inputId = "epsilon", value = 0.1)
  #     reset("dataset"); reset("num_points"); reset("epsilon")
  # })
  
  
  ####################
  # generate data for regression
  
  # a <- 3; b <- 0.87; c <- 0.5
  # num_test_points_reg <- 1000      # number of test data points
  # num_rep_sets_reg <- 50                 # number of replicated datasets
  # sample_reps <- sample(1:num_rep_sets_reg, 20)
  # 
  # generate_data_regression <- function(input_dataset, input_num_training_points, input_noise_sd)
  # {
  #   # num_training_points <- input$num_points      # number of training data points
  #   num_training_points <- input_num_training_points
  #   num_total_points <- num_training_points + num_test_points_reg
  #   
  #   if(input_dataset == "Dataset 1")
  #   {
  #     x <- runif(n = num_total_points, min = 0, max = 20)   # input/predictor
  #     fx <- a + (b * sqrt(x)) + (c * sin(x))   # true fx
  #   }
  #   else if(input_dataset == "Dataset 2")
  #   {
  #     x <- runif(n = num_total_points, min = -1, max = 1)   # input/predictor
  #     fx <- sin(pi*x) # true fx
  #   }
  #   else if(input_dataset == "Dataset 3")
  #   {
  #     x <- runif(n = num_total_points, min = -5, max = 10)   # input/predictor
  #     fx <- ifelse(0.1*(x^2)<3, 0.1*(x^2), 3)   # true fx
  #   }
  #   else if(input_dataset == "Dataset 4")
  #   {
  #     x <- runif(n = num_total_points, min = 0, max = 1)   # input/predictor
  #     fx <- sin(12*(x+0.2))/(x+0.2)   # true fx
  #   }
  #   else if(input_dataset == "Dataset 5")
  #   {
  #     x <- runif(n = num_total_points, min = -5, max = 5)   # input/predictor
  #     fx <- a + (b * x^2) + (c * x^3)   # true fx
  #   }
  #   # else if(input$dataset == "Dataset 6")
  #   # {
  #   #   x <- runif(n = num_total_points, min = 0, max = 20)   # input/predictor
  #   #   fx <- function(x) a + (b * x) + (c * x^2)   # true fx
  #   # }
  #   
  #   
  #   x_train <- x[1:num_training_points]; fx_train <- fx[1:num_training_points]
  #   x_test <- x[-(1:num_training_points)]; fx_test <- fx[-(1:num_training_points)]
  #   
  #   # noise_sd <- input$epsilon          # standard deviation of epsilon
  #   
  #   if(input_dataset == "Dataset 5")
  #   {
  #     noise_sd <- case_when(input_noise_sd == "Low" ~ 1, 
  #                           input_noise_sd == "Medium" ~ 5,
  #                           input_noise_sd == "High" ~ 10)
  #   }
  #   else
  #   {
  #     noise_sd <- case_when(input_noise_sd == "Low" ~ 0.1, 
  #                           input_noise_sd == "Medium" ~ 0.5,
  #                           input_noise_sd == "High" ~ 1)
  #   }
  #   
  #   e_train <- matrix(rnorm(num_training_points * num_rep_sets_reg, mean = 0, sd = noise_sd), 
  #                     nrow = num_training_points, ncol = num_rep_sets_reg)
  #   y_train <- fx_train + e_train
  #   
  #   e_test <- rnorm(num_test_points_reg, mean = 0, sd = noise_sd)
  #   y_test <- fx_test + e_test    # say, original y
  #   
  #   df_training <- data.frame(x = x_train, fx = fx_train, y_orig = y_train[,1])
  #   df_test <- data.frame(x = x_test, fx = fx_test, y_orig = y_test)
  #   df_training_xs <- df_training$x; df_training_ys <- y_train
  #   
  #   rm(e_train, y_train, x_train, fx_train, e_test, x_test, fx_test, y_test)
  #   
  #   return(list(df_training, df_test, df_training_xs, df_training_ys, noise_sd, num_training_points))
  #   
  # }
  # 
  # df_data_regression <- reactive({
  #   
  #   dflist <- generate_data_regression(input_dataset = input$dataset_polynomial,
  #                                      input_num_training_points = input$num_points_polynomial,
  #                                      input_noise_sd = input$epsilon_polynomial)
  #   
  #   return(dflist)
  #   
  # })
  # 
  # observeEvent(input$dataset_polynomial, {
  #   updateSelectInput(inputId = "dataset_spline", selected = input$dataset_polynomial)
  #   updateSelectInput(inputId = "dataset_knn_reg", selected = input$dataset_polynomial)
  #   updateSelectInput(inputId = "dataset_tree_reg", selected = input$dataset_polynomial)
  #   updateSelectInput(inputId = "dataset_lasso", selected = input$dataset_polynomial)
  #   updateSelectInput(inputId = "dataset_rf", selected = input$dataset_polynomial)
  # })
  # 
  # observeEvent(input$dataset_spline, {
  #   updateSelectInput(inputId = "dataset_polynomial", selected = input$dataset_spline)
  #   updateSelectInput(inputId = "dataset_knn_reg", selected = input$dataset_spline)
  #   updateSelectInput(inputId = "dataset_tree_reg", selected = input$dataset_spline)
  #   updateSelectInput(inputId = "dataset_lasso", selected = input$dataset_spline)
  #   updateSelectInput(inputId = "dataset_rf", selected = input$dataset_spline)
  # })
  # 
  # observeEvent(input$dataset_knn_reg, {
  #   updateSelectInput(inputId = "dataset_spline", selected = input$dataset_knn_reg)
  #   updateSelectInput(inputId = "dataset_polynomial", selected = input$dataset_knn_reg)
  #   updateSelectInput(inputId = "dataset_tree_reg", selected = input$dataset_knn_reg)
  #   updateSelectInput(inputId = "dataset_lasso", selected = input$dataset_knn_reg)
  #   updateSelectInput(inputId = "dataset_rf", selected = input$dataset_knn_reg)
  # })
  # 
  # observeEvent(input$dataset_tree_reg, {
  #   updateSelectInput(inputId = "dataset_spline", selected = input$dataset_tree_reg)
  #   updateSelectInput(inputId = "dataset_knn_reg", selected = input$dataset_tree_reg)
  #   updateSelectInput(inputId = "dataset_polynomial", selected = input$dataset_tree_reg)
  #   updateSelectInput(inputId = "dataset_lasso", selected = input$dataset_tree_reg)
  #   updateSelectInput(inputId = "dataset_rf", selected = input$dataset_tree_reg)
  # })
  # 
  # observeEvent(input$dataset_lasso, {
  #   updateSelectInput(inputId = "dataset_spline", selected = input$dataset_lasso)
  #   updateSelectInput(inputId = "dataset_knn_reg", selected = input$dataset_lasso)
  #   updateSelectInput(inputId = "dataset_tree_reg", selected = input$dataset_lasso)
  #   updateSelectInput(inputId = "dataset_polynomial", selected = input$dataset_lasso)
  #   updateSelectInput(inputId = "dataset_rf", selected = input$dataset_lasso)
  # })
  # 
  # observeEvent(input$dataset_rf, {
  #   updateSelectInput(inputId = "dataset_lasso", selected = input$dataset_rf)
  #   updateSelectInput(inputId = "dataset_spline", selected = input$dataset_rf)
  #   updateSelectInput(inputId = "dataset_knn_reg", selected = input$dataset_rf)
  #   updateSelectInput(inputId = "dataset_tree_reg", selected = input$dataset_rf)
  #   updateSelectInput(inputId = "dataset_polynomial", selected = input$dataset_rf)
  # })
  # 
  # 
  # 
  # observeEvent(input$num_points_polynomial, {
  #   updateSliderInput(inputId = "num_points_spline", value = input$num_points_polynomial)
  #   updateSliderInput(inputId = "num_points_knn_reg", value = input$num_points_polynomial)
  #   updateSliderInput(inputId = "num_points_tree_reg", value = input$num_points_polynomial)
  #   updateSliderInput(inputId = "num_points_lasso", value = input$num_points_polynomial)
  #   updateSliderInput(inputId = "num_points_rf", value = input$num_points_polynomial)
  # })
  # 
  # observeEvent(input$num_points_spline, {
  #   updateSliderInput(inputId = "num_points_polynomial", value = input$num_points_spline)
  #   updateSliderInput(inputId = "num_points_knn_reg", value = input$num_points_spline)
  #   updateSliderInput(inputId = "num_points_tree_reg", value = input$num_points_spline)
  #   updateSliderInput(inputId = "num_points_lasso", value = input$num_points_spline)
  #   updateSliderInput(inputId = "num_points_rf", value = input$num_points_spline)
  # })
  # 
  # observeEvent(input$num_points_knn_reg, {
  #   updateSliderInput(inputId = "num_points_spline", value = input$num_points_knn_reg)
  #   updateSliderInput(inputId = "num_points_polynomial", value = input$num_points_knn_reg)
  #   updateSliderInput(inputId = "num_points_tree_reg", value = input$num_points_knn_reg)
  #   updateSliderInput(inputId = "num_points_lasso", value = input$num_points_knn_reg)
  #   updateSliderInput(inputId = "num_points_rf", value = input$num_points_knn_reg)
  # })
  # 
  # observeEvent(input$num_points_tree_reg, {
  #   updateSliderInput(inputId = "num_points_spline", value = input$num_points_tree_reg)
  #   updateSliderInput(inputId = "num_points_knn_reg", value = input$num_points_tree_reg)
  #   updateSliderInput(inputId = "num_points_polynomial", value = input$num_points_tree_reg)
  #   updateSliderInput(inputId = "num_points_lasso", value = input$num_points_tree_reg)
  #   updateSliderInput(inputId = "num_points_rf", value = input$num_points_tree_reg)
  # })
  # 
  # observeEvent(input$num_points_lasso, {
  #   updateSliderInput(inputId = "num_points_spline", value = input$num_points_lasso)
  #   updateSliderInput(inputId = "num_points_knn_reg", value = input$num_points_lasso)
  #   updateSliderInput(inputId = "num_points_tree_reg", value = input$num_points_lasso)
  #   updateSliderInput(inputId = "num_points_polynomial", value = input$num_points_lasso)
  #   updateSliderInput(inputId = "num_points_rf", value = input$num_points_lasso)
  # })
  # 
  # observeEvent(input$num_points_rf, {
  #   updateSliderInput(inputId = "num_points_spline", value = input$num_points_rf)
  #   updateSliderInput(inputId = "num_points_knn_reg", value = input$num_points_rf)
  #   updateSliderInput(inputId = "num_points_tree_reg", value = input$num_points_rf)
  #   updateSliderInput(inputId = "num_points_polynomial", value = input$num_points_rf)
  #   updateSliderInput(inputId = "num_points_lasso", value = input$num_points_rf)
  # })
  # 
  # 
  # 
  # observeEvent(input$epsilon_polynomial, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_spline", selected = input$epsilon_polynomial)
  #   updateSliderTextInput(session = session, inputId = "epsilon_knn_reg", selected = input$epsilon_polynomial)
  #   updateSliderTextInput(session = session, inputId = "epsilon_tree_reg", selected = input$epsilon_polynomial)
  #   updateSliderTextInput(session = session, inputId = "epsilon_lasso", selected = input$epsilon_polynomial)
  #   updateSliderTextInput(session = session, inputId = "epsilon_rf", selected = input$epsilon_polynomial)
  # })
  # 
  # observeEvent(input$epsilon_spline, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_polynomial", selected = input$epsilon_spline)
  #   updateSliderTextInput(session = session, inputId = "epsilon_knn_reg", selected = input$epsilon_spline)
  #   updateSliderTextInput(session = session, inputId = "epsilon_tree_reg", selected = input$epsilon_spline)
  #   updateSliderTextInput(session = session, inputId = "epsilon_lasso", selected = input$epsilon_spline)
  #   updateSliderTextInput(session = session, inputId = "epsilon_rf", selected = input$epsilon_spline)
  # })
  # 
  # observeEvent(input$epsilon_knn_reg, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_spline", selected = input$epsilon_knn_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_polynomial", selected = input$epsilon_knn_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_tree_reg", selected = input$epsilon_knn_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_lasso", selected = input$epsilon_knn_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_rf", selected = input$epsilon_knn_reg)
  # })
  # 
  # observeEvent(input$epsilon_tree_reg, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_spline", selected = input$epsilon_tree_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_knn_reg", selected = input$epsilon_tree_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_polynomial", selected = input$epsilon_tree_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_lasso", selected = input$epsilon_tree_reg)
  #   updateSliderTextInput(session = session, inputId = "epsilon_rf", selected = input$epsilon_tree_reg)
  # })
  # 
  # observeEvent(input$epsilon_lasso, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_spline", selected = input$epsilon_lasso)
  #   updateSliderTextInput(session = session, inputId = "epsilon_knn_reg", selected = input$epsilon_lasso)
  #   updateSliderTextInput(session = session, inputId = "epsilon_tree_reg", selected = input$epsilon_lasso)
  #   updateSliderTextInput(session = session, inputId = "epsilon_polynomial", selected = input$epsilon_lasso)
  #   updateSliderTextInput(session = session, inputId = "epsilon_rf", selected = input$epsilon_lasso)
  # })
  # 
  # observeEvent(input$epsilon_rf, {
  #   updateSliderTextInput(session = session, inputId = "epsilon_spline", selected = input$epsilon_rf)
  #   updateSliderTextInput(session = session, inputId = "epsilon_knn_reg", selected = input$epsilon_rf)
  #   updateSliderTextInput(session = session, inputId = "epsilon_tree_reg", selected = input$epsilon_rf)
  #   updateSliderTextInput(session = session, inputId = "epsilon_polynomial", selected = input$epsilon_rf)
  #   updateSliderTextInput(session = session, inputId = "epsilon_lasso", selected = input$epsilon_rf)
  # })
  
  
  ####################
  # generate data for classification
  
  
  df_data_classification <- eventReactive(input$action_logreg | input$action_knn_class | input$action_tree_class | input$action_svm | input$action_rf_class,
  {
    
    dflist <- generate_data_classification_m(mu0 = c(0.5,-0.5), mu1 = c(-0.5,0.5),
                                           num_training_points = input$num_points_class_logreg,
                                           num_test_points = num_test_points_class,
                                           num_rep_sets = num_rep_sets_class,
                                           input_noise_sd = input$epsilon_logreg)
    
    return(dflist)
    
  }) 
  
  # %>% bindCache(input$num_points_class_logreg, input$epsilon_logreg)
  
  observeEvent(input$num_points_class_logreg, {
    updateSliderInput(inputId = "num_points_class_knn", value = input$num_points_class_logreg)
    updateSliderInput(inputId = "num_points_class_tree", value = input$num_points_class_logreg)
    updateSliderInput(inputId = "num_points_class_svm", value = input$num_points_class_logreg)
    updateSliderInput(inputId = "num_points_class_rf", value = input$num_points_class_logreg)
  })
  
  observeEvent(input$num_points_class_knn, {
    updateSliderInput(inputId = "num_points_class_logreg", value = input$num_points_class_knn)
    updateSliderInput(inputId = "num_points_class_tree", value = input$num_points_class_knn)
    updateSliderInput(inputId = "num_points_class_svm", value = input$num_points_class_knn)
    updateSliderInput(inputId = "num_points_class_rf", value = input$num_points_class_knn)
  })
  
  observeEvent(input$num_points_class_tree, {
    updateSliderInput(inputId = "num_points_class_knn", value = input$num_points_class_tree)
    updateSliderInput(inputId = "num_points_class_logreg", value = input$num_points_class_tree)
    updateSliderInput(inputId = "num_points_class_svm", value = input$num_points_class_tree)
    updateSliderInput(inputId = "num_points_class_rf", value = input$num_points_class_tree)
  })
  
  observeEvent(input$num_points_class_svm, {
    updateSliderInput(inputId = "num_points_class_knn", value = input$num_points_class_svm)
    updateSliderInput(inputId = "num_points_class_tree", value = input$num_points_class_svm)
    updateSliderInput(inputId = "num_points_class_logreg", value = input$num_points_class_svm)
    updateSliderInput(inputId = "num_points_class_rf", value = input$num_points_class_svm)
  })
  
  observeEvent(input$num_points_class_rf, {
    updateSliderInput(inputId = "num_points_class_knn", value = input$num_points_class_rf)
    updateSliderInput(inputId = "num_points_class_tree", value = input$num_points_class_rf)
    updateSliderInput(inputId = "num_points_class_svm", value = input$num_points_class_rf)
    updateSliderInput(inputId = "num_points_class_logreg", value = input$num_points_class_rf)
  })
  
  
  
  
  observeEvent(input$epsilon_logreg, {
    updateSliderTextInput(session = session, inputId = "epsilon_knn_class", selected = input$epsilon_logreg)
    updateSliderTextInput(session = session, inputId = "epsilon_tree_class", selected = input$epsilon_logreg)
    updateSliderTextInput(session = session, inputId = "epsilon_svm", selected = input$epsilon_logreg)
    updateSliderTextInput(session = session, inputId = "epsilon_rf_class", selected = input$epsilon_logreg)
  })
  
  observeEvent(input$epsilon_knn_class, {
    updateSliderTextInput(session = session, inputId = "epsilon_logreg", selected = input$epsilon_knn_class)
    updateSliderTextInput(session = session, inputId = "epsilon_tree_class", selected = input$epsilon_knn_class)
    updateSliderTextInput(session = session, inputId = "epsilon_svm", selected = input$epsilon_knn_class)
    updateSliderTextInput(session = session, inputId = "epsilon_rf_class", selected = input$epsilon_knn_class)
    
  })
  
  observeEvent(input$epsilon_tree_class, {
    updateSliderTextInput(session = session, inputId = "epsilon_logreg", selected = input$epsilon_tree_class)
    updateSliderTextInput(session = session, inputId = "epsilon_knn_class", selected = input$epsilon_tree_class)
    updateSliderTextInput(session = session, inputId = "epsilon_svm", selected = input$epsilon_tree_class)
    updateSliderTextInput(session = session, inputId = "epsilon_rf_class", selected = input$epsilon_tree_class)
    
  })
  
  observeEvent(input$epsilon_svm, {
    updateSliderTextInput(session = session, inputId = "epsilon_logreg", selected = input$epsilon_svm)
    updateSliderTextInput(session = session, inputId = "epsilon_knn_class", selected = input$epsilon_svm)
    updateSliderTextInput(session = session, inputId = "epsilon_tree_class", selected = input$epsilon_svm)
    updateSliderTextInput(session = session, inputId = "epsilon_rf_class", selected = input$epsilon_svm)
    
  })
  
  observeEvent(input$epsilon_rf_class, {
    updateSliderTextInput(session = session, inputId = "epsilon_logreg", selected = input$epsilon_rf_class)
    updateSliderTextInput(session = session, inputId = "epsilon_knn_class", selected = input$epsilon_rf_class)
    updateSliderTextInput(session = session, inputId = "epsilon_tree_class", selected = input$epsilon_rf_class)
    updateSliderTextInput(session = session, inputId = "epsilon_svm", selected = input$epsilon_rf_class)
    
  })
  
  
  ####################
  # calculate bias and variance 
  
  
  # df_bvt_calc_polynomial <- reactive({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   noise_sd <- df_data_regression()[[5]]
  #   
  #   num_training_points <- input$num_points_polynomial
  #   # noise_sd <- input$epsilon_polynomial
  #   
  #   df_training_preds <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse <- rep(NA, num_rep_sets_reg)
  #   # df_test_preds_selected_pivot <- list()
  #   df_test_preds_list <- list()
  #   # train_mse_list <- list()
  #   
  #   parameter_values <- 1:8; l <- length(parameter_values)
  #   bias_sq <- numeric(l); variance <- numeric(l); training_mse <- numeric(l)
  #   
  #   for(count in seq_along(parameter_values))
  #   {
  #     d <- parameter_values[count]
  #     
  #     # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #     for(j in 1:num_rep_sets_reg)
  #     {
  #       
  #       # fit model of d degrees of freedom to each replicated (training) set
  #       # get predictions (y_hat's) for each replicated (training) set and test set
  #       y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #       m <- lm(y_rep ~ poly(x_rep, degree = d, raw = TRUE))
  #       y_hat_training <- m$fitted.values
  #       y_hat_test <- predict(m, newdata = data.frame(x_rep = df_test$x))
  #       
  #       # store predictions for each set
  #       df_training_preds[,j] <- y_hat_training
  #       df_test_preds[,j] <- y_hat_test
  #       
  #       # calculate training MSE for each replicated (training) dataset
  #       train_mse[j] <- mean((y_rep-y_hat_training)^2)
  #       
  #     }
  #     
  #     df_test_preds_list[[count]] <- df_test_preds
  #     # train_mse_list[[count]] <- train_mse
  #     
  #     # df_test_preds_selected <- data.frame(df_test_preds[,s])
  #     # df_test_preds_selected$mean_pred <- apply(df_test_preds_selected,1,mean)
  #     # df_test_preds_selected$x <- df_test$x
  #     # df_test_preds_selected_pivot[[count]] <- df_test_preds_selected %>% 
  #     #   pivot_longer(cols = 1:20, names_to = "rep_data", values_to = "preds")
  #     
  #     # calculate bias squared and variance for test MSE
  #     E_y_hat <- apply(df_test_preds, 1, mean)
  #     V_y_hat <- apply(df_test_preds, 1, var)
  #     bias_squared <- (E_y_hat - df_test$fx)^2
  #     
  #     # store bias squared, variance, and training MSE values
  #     training_mse[count] <- mean(train_mse)
  #     bias_sq[count] <- mean(bias_squared)
  #     variance[count] <- mean(V_y_hat)
  #     
  #     
  #   }
  #   
  #   
  #   # store results
  #   results <- data.frame(parameter_values = factor(parameter_values),
  #                         Bias_sq = bias_sq,
  #                         Variance = variance,
  #                         Test = bias_sq + variance + (noise_sd^2),
  #                         Training = training_mse)
  #   
  #   # # store results
  #   # results <- data.frame(parameter_values = factor(parameter_values),
  #   #                       Bias_sq = apply((sapply(df_test_preds_list, FUN = function(x) apply(x,1,mean)) - df_test$fx)^2, 2, mean),
  #   #                       Variance = apply(sapply(df_test_preds_list, FUN = function(x) apply(x,1,var)),2,mean),
  #   #                       # Test = bias_sq + variance + (noise_sd^2),
  #   #                       Training = sapply(train_mse_list, FUN = mean)) %>%
  #   #   mutate(Test = Bias_sq + Variance + (noise_sd^2))
  #   
  #   
  #   # HAVING LISTS (TO CALCULATE THE 'RESULTS') TAKES UP MEMORY AND IS PROBABLY NOT SIGNIFICANTLY FASTER
  #   # THE DF FOR REPLICATED DATASETS ALSO DOESN'T TAKE MUCH TIME
  #   
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(results, parameter_values, df_test_preds_list, noise_sd))
  #   
  # })
  # 
  # df_bvt_calc_spline <- reactive({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   noise_sd <- df_data_regression()[[5]]
  #   
  #   num_training_points <- input$num_points_polynomial
  #   # ; noise_sd <- input$epsilon_polynomial
  #   
  #   df_training_preds <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse <- rep(NA, num_rep_sets_reg)
  #   # df_test_preds_selected_pivot <- list()
  #   df_test_preds_list <- list()
  #   # train_mse_list <- list()
  #   
  #   parameter_values <- seq(2, 95, 15); l <- length(parameter_values)
  #   bias_sq <- numeric(l); variance <- numeric(l); training_mse <- numeric(l)
  #   
  #   for(count in seq_along(parameter_values))
  #   {
  #     d <- parameter_values[count]
  #     
  #     # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #     for(j in 1:num_rep_sets_reg)
  #     {
  #       
  #       # fit model of d degrees of freedom to each replicated (training) set
  #       # get predictions (y_hat's) for each replicated (training) set and test set
  #       y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #       m <- smooth.spline(x = x_rep, y = y_rep, df = d)
  #       y_hat_training <- predict(m, x = x_rep)$y
  #       y_hat_test <- predict(m, x = df_test$x)$y
  #       
  #       # store predictions for each set
  #       df_training_preds[,j] <- y_hat_training
  #       df_test_preds[,j] <- y_hat_test
  #       
  #       # calculate training MSE for each replicated (training) dataset
  #       train_mse[j] <- mean((y_rep-y_hat_training)^2)
  #       
  #     }
  #     
  #     df_test_preds_list[[count]] <- df_test_preds
  #     # train_mse_list[[count]] <- train_mse
  #     # 
  #     # df_test_preds_selected <- data.frame(df_test_preds[,s])
  #     # df_test_preds_selected$mean_pred <- apply(df_test_preds_selected,1,mean)
  #     # df_test_preds_selected$x <- df_test$x
  #     # df_test_preds_selected_pivot[[count]] <- df_test_preds_selected %>% 
  #     #   pivot_longer(cols = 1:20, names_to = "rep_data", values_to = "preds")
  #     
  #     # calculate bias squared and variance for test MSE
  #     E_y_hat <- apply(df_test_preds, 1, mean)
  #     V_y_hat <- apply(df_test_preds, 1, var)
  #     bias_squared <- (E_y_hat - df_test$fx)^2
  #     
  #     # store bias squared, variance, and training MSE values
  #     training_mse[count] <- mean(train_mse)
  #     bias_sq[count] <- mean(bias_squared)
  #     variance[count] <- mean(V_y_hat)
  #     
  #     
  #   }
  #   
  #   
  #   # store results
  #   results <- data.frame(parameter_values = factor(parameter_values),
  #                         Bias_sq = bias_sq,
  #                         Variance = variance,
  #                         Test = bias_sq + variance + (noise_sd^2),
  #                         Training = training_mse)
  #   
  #   # # store results
  #   # results <- data.frame(parameter_values = factor(parameter_values),
  #   #                       Bias_sq = apply((sapply(df_test_preds_list, FUN = function(x) apply(x,1,mean)) - df_test$fx)^2, 2, mean),
  #   #                       Variance = apply(sapply(df_test_preds_list, FUN = function(x) apply(x,1,var)),2,mean),
  #   #                       # Test = bias_sq + variance + (noise_sd^2),
  #   #                       Training = sapply(train_mse_list, FUN = mean)) %>%
  #   #   mutate(Test = Bias_sq + Variance + (noise_sd^2))
  #   
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(results, parameter_values, df_test_preds_list, noise_sd))
  #   
  # })
  # 
  # df_bvt_calc_knn_reg <- reactive({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   noise_sd <- df_data_regression()[[5]]
  #   
  #   num_training_points <- input$num_points_polynomial
  #   # ; noise_sd <- input$epsilon_polynomial
  #   
  #   df_training_preds <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse <- rep(NA, num_rep_sets_reg)
  #   # df_test_preds_selected_pivot <- list()
  #   df_test_preds_list <- list()
  #   # train_mse_list <- list()
  #   
  #   parameter_values <- seq(1, 15, 2); l <- length(parameter_values)
  #   bias_sq <- numeric(l); variance <- numeric(l); training_mse <- numeric(l)
  #   
  #   for(count in seq_along(parameter_values))
  #   {
  #     d <- parameter_values[count]
  #     
  #     # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #     for(j in 1:num_rep_sets_reg)
  #     {
  #       
  #       # fit model of d degrees of freedom to each replicated (training) set
  #       # get predictions (y_hat's) for each replicated (training) set and test set
  #       y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #       y_hat_training <- Rfast::knn(xnew = matrix(x_rep), y = y_rep, 
  #                                    x = matrix(x_rep), k = d, type = "R")
  #       y_hat_test <- Rfast::knn(xnew = matrix(df_test$x), y = y_rep, 
  #                                x = matrix(x_rep), k = d, type = "R")
  #       
  #       # store predictions for each set
  #       df_training_preds[,j] <- y_hat_training
  #       df_test_preds[,j] <- y_hat_test
  #       
  #       # calculate training MSE for each replicated (training) dataset
  #       train_mse[j] <- mean((y_rep-y_hat_training)^2)
  #       
  #     }
  #     
  #     df_test_preds_list[[count]] <- df_test_preds
  #     # train_mse_list[[count]] <- train_mse
  #     # 
  #     # df_test_preds_selected <- data.frame(df_test_preds[,s])
  #     # df_test_preds_selected$mean_pred <- apply(df_test_preds_selected,1,mean)
  #     # df_test_preds_selected$x <- df_test$x
  #     # df_test_preds_selected_pivot[[count]] <- df_test_preds_selected %>% 
  #     #   pivot_longer(cols = 1:20, names_to = "rep_data", values_to = "preds")
  #     
  #     # calculate bias squared and variance for test MSE
  #     E_y_hat <- apply(df_test_preds, 1, mean)
  #     V_y_hat <- apply(df_test_preds, 1, var)
  #     bias_squared <- (E_y_hat - df_test$fx)^2
  #     
  #     # store bias squared, variance, and training MSE values
  #     training_mse[count] <- mean(train_mse)
  #     bias_sq[count] <- mean(bias_squared)
  #     variance[count] <- mean(V_y_hat)
  #     
  #     
  #   }
  #   
  #   
  #   # store results
  #   results <- data.frame(parameter_values = factor(parameter_values),
  #                         Bias_sq = bias_sq,
  #                         Variance = variance,
  #                         Test = bias_sq + variance + (noise_sd^2),
  #                         Training = training_mse)
  #   
  #   # # store results
  #   # results <- data.frame(parameter_values = factor(parameter_values),
  #   #                       Bias_sq = apply((sapply(df_test_preds_list, FUN = function(x) apply(x,1,mean)) - df_test$fx)^2, 2, mean),
  #   #                       Variance = apply(sapply(df_test_preds_list, FUN = function(x) apply(x,1,var)),2,mean),
  #   #                       # Test = bias_sq + variance + (noise_sd^2),
  #   #                       Training = sapply(train_mse_list, FUN = mean)) %>%
  #   #   mutate(Test = Bias_sq + Variance + (noise_sd^2))
  #   
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(results, parameter_values, df_test_preds_list, noise_sd))
  #   
  # })
  # 
  # df_bvt_calc_tree_reg <- reactive({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   noise_sd <- df_data_regression()[[5]]
  #   
  #   num_training_points <- input$num_points_polynomial
  #   # ; noise_sd <- input$epsilon_polynomial
  #   
  #   df_training_preds <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse <- rep(NA, num_rep_sets_reg)
  #   # df_test_preds_selected_pivot <- list()
  #   df_test_preds_list <- list()
  #   # train_mse_list <- list()
  #   
  #   parameter_values <- seq(2, 14, 2); l <- length(parameter_values)
  #   bias_sq <- numeric(l); variance <- numeric(l); training_mse <- numeric(l)
  #   
  #   for(count in seq_along(parameter_values))
  #   {
  #     d <- parameter_values[count]
  #     
  #     # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #     for(j in 1:num_rep_sets_reg)
  #     {
  #       
  #       # fit model of d degrees of freedom to each replicated (training) set
  #       # get predictions (y_hat's) for each replicated (training) set and test set
  #       y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #       m <- rpart(y_rep ~ x_rep, method = "anova", 
  #                  control = rpart.control(cp = 0, xval = 0, minbucket = 1, maxdepth = d))
  #       y_hat_training <- predict(m, newdata = data.frame(x_rep))
  #       y_hat_test <- predict(m, newdata = data.frame(x_rep = df_test$x))
  #       
  #       # store predictions for each set
  #       df_training_preds[,j] <- y_hat_training
  #       df_test_preds[,j] <- y_hat_test
  #       
  #       # calculate training MSE for each replicated (training) dataset
  #       train_mse[j] <- mean((y_rep-y_hat_training)^2)
  #       
  #     }
  #     
  #     df_test_preds_list[[count]] <- df_test_preds
  #     # train_mse_list[[count]] <- train_mse
  #     # 
  #     # df_test_preds_selected <- data.frame(df_test_preds[,s])
  #     # df_test_preds_selected$mean_pred <- apply(df_test_preds_selected,1,mean)
  #     # df_test_preds_selected$x <- df_test$x
  #     # df_test_preds_selected_pivot[[count]] <- df_test_preds_selected %>% 
  #     #   pivot_longer(cols = 1:20, names_to = "rep_data", values_to = "preds")
  #     
  #     # calculate bias squared and variance for test MSE
  #     E_y_hat <- apply(df_test_preds, 1, mean)
  #     V_y_hat <- apply(df_test_preds, 1, var)
  #     bias_squared <- (E_y_hat - df_test$fx)^2
  #     
  #     # store bias squared, variance, and training MSE values
  #     training_mse[count] <- mean(train_mse)
  #     bias_sq[count] <- mean(bias_squared)
  #     variance[count] <- mean(V_y_hat)
  #     
  #     
  #   }
  #   
  #   
  #   # store results
  #   results <- data.frame(parameter_values = factor(parameter_values),
  #                         Bias_sq = bias_sq,
  #                         Variance = variance,
  #                         Test = bias_sq + variance + (noise_sd^2),
  #                         Training = training_mse)
  #   
  #   # # store results
  #   # results <- data.frame(parameter_values = factor(parameter_values),
  #   #                       Bias_sq = apply((sapply(df_test_preds_list, FUN = function(x) apply(x,1,mean)) - df_test$fx)^2, 2, mean),
  #   #                       Variance = apply(sapply(df_test_preds_list, FUN = function(x) apply(x,1,var)),2,mean),
  #   #                       # Test = bias_sq + variance + (noise_sd^2),
  #   #                       Training = sapply(train_mse_list, FUN = mean)) %>%
  #   #   mutate(Test = Bias_sq + Variance + (noise_sd^2))
  #   
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(results, parameter_values, df_test_preds_list, noise_sd))
  #   
  # })
  # 
  # df_bvt_calc_lasso <- reactive({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   noise_sd <- df_data_regression()[[5]]
  #   
  #   num_training_points <- input$num_points_polynomial
  #   # ; noise_sd <- input$epsilon_polynomial
  #   
  #   poly_degree <- 5
  #   df_training_preds <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse <- rep(NA, num_rep_sets_reg)
  #   # df_test_preds_selected_pivot <- list()
  #   df_test_preds_list <- list()
  #   # train_mse_list <- list()
  #   model_coefficients <- list()
  #   
  #   # parameter_values <- c(0, 10^seq(-3,1,1))
  #   parameter_values <- 10^seq(-4,3,1)
  #   l <- length(parameter_values)
  #   bias_sq <- numeric(l); variance <- numeric(l); training_mse <- numeric(l)
  #   
  #   for(count in seq_along(parameter_values))
  #   {
  #     d <- parameter_values[count]
  #     m_coefs <- matrix(nrow = num_rep_sets_reg, ncol = poly_degree)
  #     
  #     # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #     for(j in 1:num_rep_sets_reg)
  #     {
  #       
  #       # fit model of d degrees of freedom to each replicated (training) set
  #       # get predictions (y_hat's) for each replicated (training) set and test set
  #       y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #       
  #       # if(d == 0)
  #       # {
  #       #   m <- lm(y_rep ~ poly(x_rep, degree = poly_degree, raw = TRUE))
  #       #   y_hat_training <- predict(m, newdata = data.frame(x_rep))
  #       #   y_hat_test <- predict(m, newdata = data.frame(x_rep = df_test$x))
  #       #   m_coefs[j,] <- as.vector(m$coefficients)[-1]
  #       # }
  #       # else
  #       # {
  #         x_rep_mat <- model.matrix(~ poly(x_rep, degree = poly_degree, raw = TRUE))[,-1]
  #         m <- glmnet(x = x_rep_mat, y = y_rep, family = "gaussian", alpha = 0, lambda = d)
  #         y_hat_training <- predict(m, newx = x_rep_mat)
  #         y_hat_test <- predict(m, newx = model.matrix(~ poly(df_test$x, degree = poly_degree, raw = TRUE))[,-1])
  #         m_coefs[j,] <- as.vector(m$beta)
  #       # }
  #       
  #       # store predictions for each set
  #       df_training_preds[,j] <- y_hat_training
  #       df_test_preds[,j] <- y_hat_test
  #       
  #       # calculate training MSE for each replicated (training) dataset
  #       train_mse[j] <- mean((y_rep-y_hat_training)^2)
  #       
  #     }
  #     
  #     df_test_preds_list[[count]] <- df_test_preds
  #     model_coefficients[[count]] <- m_coefs
  #     # train_mse_list[[count]] <- train_mse
  #     # 
  #     # df_test_preds_selected <- data.frame(df_test_preds[,s])
  #     # df_test_preds_selected$mean_pred <- apply(df_test_preds_selected,1,mean)
  #     # df_test_preds_selected$x <- df_test$x
  #     # df_test_preds_selected_pivot[[count]] <- df_test_preds_selected %>% 
  #     #   pivot_longer(cols = 1:20, names_to = "rep_data", values_to = "preds")
  #     
  #     # calculate bias squared and variance for test MSE
  #     E_y_hat <- apply(df_test_preds, 1, mean)
  #     V_y_hat <- apply(df_test_preds, 1, var)
  #     bias_squared <- (E_y_hat - df_test$fx)^2
  #     
  #     # store bias squared, variance, and training MSE values
  #     training_mse[count] <- mean(train_mse)
  #     bias_sq[count] <- mean(bias_squared)
  #     variance[count] <- mean(V_y_hat)
  #     
  #     
  #   }
  #   
  #   
  #   # store results
  #   results <- data.frame(parameter_values = factor(log10(parameter_values)),
  #                         Bias_sq = bias_sq,
  #                         Variance = variance,
  #                         Test = bias_sq + variance + (noise_sd^2),
  #                         Training = training_mse)
  #   
  #   # # store results
  #   # results <- data.frame(parameter_values = factor(parameter_values),
  #   #                       Bias_sq = apply((sapply(df_test_preds_list, FUN = function(x) apply(x,1,mean)) - df_test$fx)^2, 2, mean),
  #   #                       Variance = apply(sapply(df_test_preds_list, FUN = function(x) apply(x,1,var)),2,mean),
  #   #                       # Test = bias_sq + variance + (noise_sd^2),
  #   #                       Training = sapply(train_mse_list, FUN = mean)) %>%
  #   #   mutate(Test = Bias_sq + Variance + (noise_sd^2))
  #   
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(results, parameter_values, df_test_preds_list, noise_sd, model_coefficients))
  #   
  # })
  # 
  # df_bvt_calc_rf <- reactive({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   # df_test <- df_data_regression()[[2]]
  #   # df_training_xs <- df_data_regression()[[3]]
  #   # df_training_ys <- df_data_regression()[[4]]
  #   # noise_sd <- df_data_regression()[[5]]
  #   # 
  #   # num_training_points <- input$num_points_polynomial
  #   # # ; noise_sd <- input$epsilon_polynomial
  #   
  #   df_training_subset <- df_training[1:30, ]
  #   df_training_subset$Var1 <- rownames(df_training_subset)
  #   
  #   x_seq <- seq(min(df_training_subset$x, na.rm = TRUE), max(df_training_subset$x, na.rm = TRUE), 0.001)
  #   
  #   rffit <- ranger(formula = y_orig ~ x, data = df_training_subset,
  #                   # x = data.frame(df_training_subset$x), y = df_training_subset$y_orig, xtest = data.frame(x_seq), #data = data.frame(y_rep = y_rep, x_rep = x_rep),
  #                   num.trees = 10, # number of trees to grow (bootstrap samples) usually 500
  #                   mtry = 1, 
  #                   min.bucket = 1,
  #                   keep.inbag=TRUE)
  #   preds_rf <- predict(rffit, data = data.frame(x = x_seq), predict.all = TRUE)                        # rffit$test$predicted
  #   
  #   # return(list(results, parameter_values, df_test_preds_selected_pivot, noise_sd))
  #   return(list(df_training_subset, x_seq, rffit, preds_rf))
  #   
  # })
  
  ####################
  
  df_bvt_calc_logreg <- reactive({
    
    df_training <- df_data_classification()[[1]]
    x1_matrix <- df_data_classification()[[2]]
    x2_matrix <- df_data_classification()[[3]]
    y_matrix <- df_data_classification()[[4]]
    df_test <- df_data_classification()[[5]]
    df_test_probs <- df_data_classification()[[6]]
    
    
    df_logreg_training_preds <- matrix(nrow = input$num_points_class_logreg, ncol = num_rep_sets_class)
    df_logreg_test_preds <- matrix(nrow = num_test_points_class, ncol = num_rep_sets_class)
    logreg_train_error <- rep(NA, num_rep_sets_class); logreg_test_error <- rep(NA, num_rep_sets_class)
    
    for(j in 1:num_rep_sets_class)
    {
      
      # fit model of d degrees of freedom or degree to each replicated (training) set
      # get predictions (y_hat's) for each replicated (training) set and test set
      y_rep <- factor(y_matrix[,j]); x1_rep <- x1_matrix[,j]; x2_rep <- x2_matrix[,j]
      d_rep <- data.frame(x1 = x1_rep, x2 = x2_rep, y = y_rep)
      
      m <- glm(y ~ x1 + x2, data = d_rep, family = binomial)
      
      # store predictions for each set
      df_logreg_training_preds[,j] <- factor(ifelse(m$fitted.values >= 0.5, 1, 0))
      test_probs <- predict(object = m, newdata = df_test, type = "response")
      df_logreg_test_preds[,j] <- factor(ifelse(test_probs >= 0.5, 1, 0))
      
      # calculate training MSE for each replicated (training) dataset
      logreg_train_error[j] <- mean(d_rep$y != df_logreg_training_preds[,j]-1)
      logreg_test_error[j] <- mean(df_test$y_orig != df_logreg_test_preds[,j]-1)
    }
    
    
    logreg_training_error <- mean(logreg_train_error)
    logreg_testing_error <- mean(logreg_test_error)
    t_logreg <- df_logreg_test_preds-1; p1_logreg <- apply(t_logreg, 1, mean); p0_logreg <- 1-p1_logreg
    # from KW, 1996
    logreg_variance_kw <- mean((1-(p0_logreg^2)-(p1_logreg^2))/2)
    logreg_bias_sq_kw <- mean((((df_test_probs[,1]-p0_logreg)^2) + ((df_test_probs[,2]-p1_logreg)^2))/2)
    # from Tibshirani, 1996
    # logreg_variance_t <- mean((abs(p1_logreg-0.5))*(1-abs(2*p1_logreg-1)))
    # logreg_bias_sq_t <- mean(ifelse(((df_test_probs[,2] >= 0.5 & p1_logreg < 0.5) | (df_test_probs[,2] < 0.5 & p1_logreg >= 0.5)),
    #                                 abs(2*df_test_probs[,2] - 1), 0))  
    
    results <- data.frame(Model = "logistic",
                          `Test Error` = round(logreg_testing_error, 3),
                          `Training Error` = round(logreg_training_error, 3),
                          `Approximate Squared Bias` = round(logreg_bias_sq_kw, 3),
                          # bias_sq_t = c(knn_bias_sq_t, logreg_bias_sq_t),
                          `Approximate Variance` = round(logreg_variance_kw, 3), 
                          check.names = FALSE)
    # var_t = c(knn_variance_t, logreg_variance_t))
    
    # alldf <- expand.grid(xeg=seq(round(min(df_training$x1),2), round(max(df_training$x1),2), 0.02), 
    #                      yeg=seq(round(min(df_training$x2),2), round(max(df_training$x2),2), 0.02))
    # 
    
    return(list(results))
    
  }) %>% bindCache(df_data_classification())
  
  df_bvt_calc_knn_class <- reactive({
    
    df_training <- df_data_classification()[[1]]
    x1_matrix <- df_data_classification()[[2]]
    x2_matrix <- df_data_classification()[[3]]
    y_matrix <- df_data_classification()[[4]]
    df_test <- df_data_classification()[[5]]
    df_test_probs <- df_data_classification()[[6]]
    
    parameter_values <- seq(1, 15, by = 2); l <- length(parameter_values)
    
    # to store
    knn_training_error <- numeric(l); knn_testing_error <- numeric(l)
    knn_variance_kw <- numeric(l); knn_bias_sq_kw <- numeric(l)
    # knn_variance_t <- c(); knn_bias_sq_t <- c()
    
    
    for(count in seq_along(parameter_values))
    {
      d <- parameter_values[count]
      
      # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
      # for storage
      df_knn_training_preds <- matrix(nrow = nrow(df_training), ncol = num_rep_sets_class)
      df_knn_test_preds <- matrix(nrow = num_test_points_class, ncol = num_rep_sets_class)
      knn_train_error <- rep(NA, num_rep_sets_class); knn_test_error <- rep(NA, num_rep_sets_class)
      
      for(j in 1:num_rep_sets_class)
      {
        
        # fit model of d degrees of freedom or degree to each replicated (training) set
        # get predictions (y_hat's) for each replicated (training) set and test set
        y_rep <- factor(y_matrix[,j]); x1_rep <- x1_matrix[,j]; x2_rep <- x2_matrix[,j]
        d_rep <- data.frame(x1 = x1_rep, x2 = x2_rep, y = y_rep)
        
        m <- knn3(y ~ x1 + x2, data = d_rep, k = d)
        
        # store predictions for each set
        df_knn_training_preds[,j] <- predict(m, newdata = d_rep, type = "class")
        df_knn_test_preds[,j] <- predict(m, newdata = df_test, type = "class")
        
        # calculate training MSE for each replicated (training) dataset
        knn_train_error[j] <- mean(d_rep$y != df_knn_training_preds[,j]-1)
        knn_test_error[j] <- mean(df_test$y_orig != df_knn_test_preds[,j]-1)
      }
      
      
      # store bias squared, variance, and training MSE values
      knn_training_error[count] <- mean(knn_train_error)
      knn_testing_error[count] <- mean(knn_test_error)
      
      t_knn <- df_knn_test_preds-1; p1_knn <- apply(t_knn, 1, mean); p0_knn <- 1-p1_knn
      # from KW, 1996
      knn_variance_kw[count] <- mean((1-(p0_knn^2)-(p1_knn^2))/2)
      knn_bias_sq_kw[count] <- mean((((df_test_probs[,1]-p0_knn)^2) + ((df_test_probs[,2]-p1_knn)^2))/2)   
      # from Tibshirani, 1996
      # knn_variance_t <- c(knn_variance_t, mean((abs(p1_knn-0.5))*(1-abs(2*p1_knn-1))))    
      # knn_bias_sq_t <- c(knn_bias_sq_t, mean(ifelse(((df_test_probs[,2] >= 0.5 & p1_knn < 0.5) | (df_test_probs[,2] < 0.5 & p1_knn >= 0.5)),
      #                                               abs(2*df_test_probs[,2] - 1), 0)))   
      
      
    }
    
    results <- data.frame(parameter_values = factor(parameter_values),
                          # Model = paste(parameter_values, "-NN", sep=""),
                          Test = round(knn_testing_error, 3),
                          Training = round(knn_training_error, 3),
                          bias_sq_kw = round(knn_bias_sq_kw, 3),
                          # bias_sq_t = c(knn_bias_sq_t, logreg_bias_sq_t),
                          var_kw = round(knn_variance_kw, 3))
    # var_t = c(knn_variance_t, logreg_variance_t))
    
    
    
    return(list(results, parameter_values))
    
  }) %>% bindCache(df_data_classification())
  
  df_bvt_calc_tree_class <- reactive({
    
    df_training <- df_data_classification()[[1]]
    x1_matrix <- df_data_classification()[[2]]
    x2_matrix <- df_data_classification()[[3]]
    y_matrix <- df_data_classification()[[4]]
    df_test <- df_data_classification()[[5]]
    df_test_probs <- df_data_classification()[[6]]
    
    parameter_values <- seq(2, 14, 2); l <- length(parameter_values)
    
    # to store
    tree_training_error <- numeric(l); tree_testing_error <- numeric(l)
    tree_variance_kw <- numeric(l); tree_bias_sq_kw <- numeric(l)
    # tree_variance_t <- c(); tree_bias_sq_t <- c()
    
    
    for(count in seq_along(parameter_values))
    {
      d <- parameter_values[count]
      
      # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
      # for storage
      df_tree_training_preds <- matrix(nrow = nrow(df_training), ncol = num_rep_sets_class)
      df_tree_test_preds <- matrix(nrow = num_test_points_class, ncol = num_rep_sets_class)
      tree_train_error <- rep(NA, num_rep_sets_class); tree_test_error <- rep(NA, num_rep_sets_class)
      
      for(j in 1:num_rep_sets_class)
      {
        
        # fit model of d degrees of freedom or degree to each replicated (training) set
        # get predictions (y_hat's) for each replicated (training) set and test set
        y_rep <- factor(y_matrix[,j]); x1_rep <- x1_matrix[,j]; x2_rep <- x2_matrix[,j]
        d_rep <- data.frame(x1 = x1_rep, x2 = x2_rep, y = y_rep)
        
        m <- rpart(y ~ x1 + x2, data = d_rep, method = "class", 
                   control = rpart.control(cp = 0, xval = 0, minbucket = 7, maxdepth = d))
        
        # store predictions for each set
        df_tree_training_preds[,j] <- predict(m, newdata = d_rep, type = "class")
        df_tree_test_preds[,j] <- predict(m, newdata = df_test, type = "class")
        
        # calculate training MSE for each replicated (training) dataset
        tree_train_error[j] <- mean(d_rep$y != df_tree_training_preds[,j]-1)
        tree_test_error[j] <- mean(df_test$y_orig != df_tree_test_preds[,j]-1)
      }
      
      
      # store bias squared, variance, and training MSE values
      tree_training_error[count] <- mean(tree_train_error)
      tree_testing_error[count] <- mean(tree_test_error)
      
      t_tree <- df_tree_test_preds-1; p1_tree <- apply(t_tree, 1, mean); p0_tree <- 1-p1_tree
      # from KW, 1996
      tree_variance_kw[count] <- mean((1-(p0_tree^2)-(p1_tree^2))/2)
      tree_bias_sq_kw[count] <- mean((((df_test_probs[,1]-p0_tree)^2) + ((df_test_probs[,2]-p1_tree)^2))/2)   
      # from Tibshirani, 1996
      # tree_variance_t <- c(tree_variance_t, mean((abs(p1_tree-0.5))*(1-abs(2*p1_tree-1))))    
      # tree_bias_sq_t <- c(tree_bias_sq_t, mean(ifelse(((df_test_probs[,2] >= 0.5 & p1_tree < 0.5) | (df_test_probs[,2] < 0.5 & p1_tree >= 0.5)),
      #                                               abs(2*df_test_probs[,2] - 1), 0)))   
      
      
    }
    
    results <- data.frame(parameter_values = factor(parameter_values),
                          # paste("tree depth ", parameter_values, sep = ""),
                          Test = round(tree_testing_error, 3),
                          Training = round(tree_training_error, 3),
                          bias_sq_kw = round(tree_bias_sq_kw, 3),
                          # bias_sq_t = c(tree_bias_sq_t, logreg_bias_sq_t),
                          var_kw = round(tree_variance_kw, 3))
    # var_t = c(tree_variance_t, logreg_variance_t))
    
    
    
    return(list(results, parameter_values))
    
  }) %>% bindCache(df_data_classification())
  
  df_bvt_calc_svm <- reactive({
    
    df_training <- df_data_classification()[[1]]
    x1_matrix <- df_data_classification()[[2]]
    x2_matrix <- df_data_classification()[[3]]
    y_matrix <- df_data_classification()[[4]]
    df_test <- df_data_classification()[[5]]
    df_test_probs <- df_data_classification()[[6]]
    
    parameter_values <- 10^seq(-2,2,1); l <- length(parameter_values)
    
    # to store
    svm_training_error <- numeric(l); svm_testing_error <- numeric(l)
    svm_variance_kw <- numeric(l); svm_bias_sq_kw <- numeric(l)
    # svm_variance_t <- c(); svm_bias_sq_t <- c()
    
    
    for(count in seq_along(parameter_values))
    {
      d <- parameter_values[count]
      
      # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
      # for storage
      df_svm_training_preds <- matrix(nrow = nrow(df_training), ncol = num_rep_sets_class)
      df_svm_test_preds <- matrix(nrow = num_test_points_class, ncol = num_rep_sets_class)
      svm_train_error <- rep(NA, num_rep_sets_class); svm_test_error <- rep(NA, num_rep_sets_class)
      
      for(j in 1:num_rep_sets_class)
      {
        
        # fit model of d degrees of freedom or degree to each replicated (training) set
        # get predictions (y_hat's) for each replicated (training) set and test set
        y_rep <- factor(y_matrix[,j]); x1_rep <- x1_matrix[,j]; x2_rep <- x2_matrix[,j]
        d_rep <- data.frame(x1 = x1_rep, x2 = x2_rep, y = y_rep)
        
        m <- svm(y ~ x1 + x2, data = d_rep, cost = d, kernel = "linear", scale = TRUE)
        
        # store predictions for each set
        df_svm_training_preds[,j] <- m$fitted
        df_svm_test_preds[,j] <- predict(m, newdata = df_test)
        
        # calculate training MSE for each replicated (training) dataset
        svm_train_error[j] <- mean(d_rep$y != df_svm_training_preds[,j]-1)
        svm_test_error[j] <- mean(df_test$y_orig != df_svm_test_preds[,j]-1)
      }
      
      
      # store bias squared, variance, and training MSE values
      svm_training_error[count] <- mean(svm_train_error)
      svm_testing_error[count] <- mean(svm_test_error)
      
      t_svm <- df_svm_test_preds-1; p1_svm <- apply(t_svm, 1, mean); p0_svm <- 1-p1_svm
      # from KW, 1996
      svm_variance_kw[count] <- mean((1-(p0_svm^2)-(p1_svm^2))/2)
      svm_bias_sq_kw[count] <- mean((((df_test_probs[,1]-p0_svm)^2) + ((df_test_probs[,2]-p1_svm)^2))/2)   
      # from Tibshirani, 1996
      # svm_variance_t <- c(svm_variance_t, mean((abs(p1_svm-0.5))*(1-abs(2*p1_svm-1))))    
      # svm_bias_sq_t <- c(svm_bias_sq_t, mean(ifelse(((df_test_probs[,2] >= 0.5 & p1_svm < 0.5) | (df_test_probs[,2] < 0.5 & p1_svm >= 0.5)),
      #                                               abs(2*df_test_probs[,2] - 1), 0)))   
      
      
    }
    
    results <- data.frame(parameter_values = factor(parameter_values),
                          # Model = paste(parameter_values, "-NN", sep=""),
                          Test = round(svm_testing_error, 3),
                          Training = round(svm_training_error, 3),
                          bias_sq_kw = round(svm_bias_sq_kw, 3),
                          # bias_sq_t = c(svm_bias_sq_t, logreg_bias_sq_t),
                          var_kw = round(svm_variance_kw, 3))
    # var_t = c(svm_variance_t, logreg_variance_t))
    
    
    
    return(list(results, parameter_values))
    
  }) %>% bindCache(df_data_classification())
  
  df_bvt_calc_rf_class <- reactive({
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_data_classification()[[8]]
    
    m_rf <- ranger(formula = y_orig ~ x1 + x2, data = df_training,
                   num.trees = 50, # number of trees to grow (bootstrap samples) usually 500
                   mtry = 1, 
                   # nodesize = 1,
                   max.depth = 20,
                   keep.inbag=TRUE)
    
    alldf$rf_preds <- predict(m_rf, data = data.frame(x1 = alldf$xeg, x2 = alldf$yeg))$predictions
    
    preds_rf <- predict(m_rf, data = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), predict.all = TRUE) 
    
    return(list(alldf, preds_rf))
    
  }) %>% bindCache(df_data_classification())
  
  
  ###########################
  # plots in first tab - Model Fits/Bootstrapping
  # INCLUDE A CHECKBOX INPUT TO SELECT AND COMPARE DIFFERENT FITS
  
  
  # output$plot_polynomial_p1 <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   
  #   # parameter_selected <- as.numeric(input$degrees)
  #   
  #   training_data_plot <- ggplot(data = df_training, aes(x=x, y=y_orig)) +
  #     geom_point(alpha = 0.4, size = 2) +
  #     stat_smooth(method = "lm", se = FALSE,
  #                 formula = y ~ poly(x, as.numeric(input$degrees_1), raw = TRUE), color = cb_pallete[7], linewidth = 1.5) +
  #     stat_smooth(method = "lm", se = FALSE,
  #                 formula = y ~ poly(x, as.numeric(input$degrees_2), raw = TRUE), color = cb_pallete[5], linewidth = 1.5) +
  #     stat_smooth(method = "lm", se = FALSE,
  #                 formula = y ~ poly(x, as.numeric(input$degrees_3), raw = TRUE), color = cb_pallete[3], linewidth = 1.5) +
  #     theme_bw() +
  #     labs(x = "Predictor x", y = "Response y", title = "Training Data")
  #   
  #   if(input$plot_true_polynomial == TRUE){training_data_plot + geom_line(aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1.5)}
  #   else{training_data_plot}
  #   
  #   
  # })
  # 
  # output$plot_spline_p1 <- renderPlot({
  #   
  #   
  #   df_training <- df_data_regression()[[1]]
  #   
  #   # parameter_selected <- as.numeric(input$degrees_of_freedom)
  #   
  #   training_data_plot <- ggplot(data = df_training, aes(x=x, y=y_orig)) + 
  #     geom_point(alpha = 0.4, size = 2) + 
  #     geom_spline(df = as.numeric(input$degrees_of_freedom_1), color = cb_pallete[7], linewidth = 1.5) +
  #     geom_spline(df = as.numeric(input$degrees_of_freedom_2), color = cb_pallete[5], linewidth = 1.5) +
  #     geom_spline(df = as.numeric(input$degrees_of_freedom_3), color = cb_pallete[3], linewidth = 1.5) +
  #     theme_bw() +
  #     labs(x = "Predictor x", y = "Response y", title = "Training Data")
  #   
  #   if(input$plot_true_spline == TRUE){training_data_plot + geom_line(aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1.5)}
  #   else{training_data_plot}
  #   
  #   
  # })
  # 
  # output$plot_knn_reg_p1 <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   
  #   # parameter_selected <- as.numeric(input$k_values)
  #   
  #   x_seq <- seq(min(df_training$x, na.rm = TRUE), max(df_training$x, na.rm = TRUE), 0.001)
  #   knnfit_1 <- knnreg(y_orig ~ x, data = df_training, k = as.numeric(input$k_values_1))
  #   knnfit_2 <- knnreg(y_orig ~ x, data = df_training, k = as.numeric(input$k_values_2))
  #   knnfit_3 <- knnreg(y_orig ~ x, data = df_training, k = as.numeric(input$k_values_3))
  #   # preds <- predict(knnfit, newdata = data.frame(x = x_seq))
  #   predictions <- data.frame(x_seq, preds_1 = predict(knnfit_1, newdata = data.frame(x = x_seq)),
  #                             preds_2 = predict(knnfit_2, newdata = data.frame(x = x_seq)),
  #                             preds_3 = predict(knnfit_3, newdata = data.frame(x = x_seq)))
  #   
  #   training_data_plot <- ggplot(data = df_training, aes(x = x, y = y_orig)) +
  #     geom_point(alpha = 0.4, size = 2) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_1), color = cb_pallete[7], linewidth = 1) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_2), color = cb_pallete[5], linewidth = 1) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_3), color = cb_pallete[3], linewidth = 1) +
  #     theme_bw() +
  #     labs(x = "Predictor x", y = "Response y", title = "Training Data")
  #   
  #   if(input$plot_true_knn_reg == TRUE){training_data_plot + geom_line(aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)}
  #   else{training_data_plot}
  # })
  # 
  # output$plot_tree_reg_p1a <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   
  #   # parameter_selected <- as.numeric(input$depths)
  #   
  #   x_seq <- seq(min(df_training$x, na.rm = TRUE), max(df_training$x, na.rm = TRUE), 0.001)
  #   treefit_1 <- rpart(y_orig ~ x, data = df_training, method = "anova",
  #                      control = rpart.control(cp = 0, xval = 0,
  #                                              minbucket = 1, maxdepth = input$depths_1))
  #   treefit_2 <- rpart(y_orig ~ x, data = df_training, method = "anova",
  #                      control = rpart.control(cp = 0, xval = 0,
  #                                              minbucket = 1, maxdepth = input$depths_2))
  #   treefit_3 <- rpart(y_orig ~ x, data = df_training, method = "anova",
  #                      control = rpart.control(cp = 0, xval = 0,
  #                                              minbucket = 1, maxdepth = input$depths_3))
  #   # preds <- predict(treefit, newdata = data.frame(x = x_seq))
  #   predictions <- data.frame(x_seq, preds_1 = predict(treefit_1, newdata = data.frame(x = x_seq)),
  #                             preds_2 = predict(treefit_2, newdata = data.frame(x = x_seq)),
  #                             preds_3 = predict(treefit_3, newdata = data.frame(x = x_seq)))
  #   
  #   training_data_plot <- ggplot(data = df_training, aes(x = x, y = y_orig)) +
  #     geom_point(alpha = 0.4, size = 2) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_1), color = cb_pallete[7], linewidth = 1) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_2), color = cb_pallete[5], linewidth = 1) +
  #     geom_line(data = predictions, aes(x = x_seq, y = preds_3), color = cb_pallete[3], linewidth = 1) +
  #     theme_bw() +
  #     labs(x = "Predictor x", y = "Response y", title = "Training Data")
  #   
  #   if(input$plot_true_tree_reg == TRUE){training_data_plot + geom_line(aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)}
  #   else{training_data_plot}
  # })
  # 
  # output$plot_tree_reg_p1b <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   
  #   parameter_selected <- as.numeric(input$depths)
  #   
  #   x_seq <- seq(min(df_training$x, na.rm = TRUE), max(df_training$x, na.rm = TRUE), 0.001)
  #   treefit <- rpart(y_orig ~ x, data = df_training, method = "anova",
  #                    control = rpart.control(cp = 0, xval = 0,
  #                                            minbucket = 1, maxdepth = parameter_selected))
  #   preds <- predict(treefit, newdata = data.frame(x = x_seq))
  #   predictions <- data.frame(x_seq, preds)
  #   
  #   rpart.plot(treefit)
  # })
  # 
  # output$plot_lasso_p1 <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   poly_degree <- 5
  #   
  #   parameter_selected <- as.numeric(input$lambda_lasso)
  #   
  #   # if(parameter_selected == 0)
  #   # {
  #   #   regularized_fit <- lm(formula = y_orig ~ poly(x, degree = poly_degree, raw = TRUE), data = df_training) 
  #   #   df_training$preds <- predict(regularized_fit, newdata = df_training)
  #   # }
  #   # else
  #   # {
  #     regularized_fit_1 <- glmnet(x = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1], 
  #                               y = df_training$y_orig,
  #                               alpha = 0,
  #                               lambda = 10^as.numeric(input$lambda_lasso_1))
  #     df_training$preds_1 <- predict(regularized_fit_1, newx = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1])
  #   # }
  #   
  #     regularized_fit_2 <- glmnet(x = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1], 
  #                                 y = df_training$y_orig,
  #                                 alpha = 0,
  #                                 lambda = 10^as.numeric(input$lambda_lasso_2))
  #     df_training$preds_2 <- predict(regularized_fit_2, newx = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1])
  #     
  #     
  #     regularized_fit_3 <- glmnet(x = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1], 
  #                                 y = df_training$y_orig,
  #                                 alpha = 0,
  #                                 lambda = 10^as.numeric(input$lambda_lasso_3))
  #     df_training$preds_3 <- predict(regularized_fit_3, newx = model.matrix(~ poly(df_training$x, degree = poly_degree, raw = TRUE))[,-1])
  #     
  #     
  #     training_data_plot <- ggplot(data = df_training, aes(x = x, y = y_orig)) +
  #     geom_point(alpha = 0.4, size = 2) +
  #     geom_line(aes(x = x, y = preds_1), color = cb_pallete[7], linewidth = 1) +
  #       geom_line(aes(x = x, y = preds_2), color = cb_pallete[5], linewidth = 1) +
  #       geom_line(aes(x = x, y = preds_3), color = cb_pallete[3], linewidth = 1) +
  #     theme_bw() +
  #     labs(x = "Predictor x", y = "Response y", title = "Training Data")
  #   
  #   if(input$plot_true_lasso == TRUE){training_data_plot + geom_line(aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)}
  #   else{training_data_plot}
  # })
  # 
  # output$plot_rf_p1 <- renderPlot({
  #   
  #   df_training_subset <- df_bvt_calc_rf()[[1]]
  #   x_seq <- df_bvt_calc_rf()[[2]]
  #   rffit <- df_bvt_calc_rf()[[3]]
  #   preds_rf <- df_bvt_calc_rf()[[4]]
  #   
  #   # trying to replicate F.7.1b from Lindholm et al. (could use to demonstrating bootstrapping, could use without true funtion and individual tree fits)
  #   ylimit <- c(min(df_training_subset$y_orig) - 0.3, max(df_training_subset$y_orig) + 0.5)
  #   xlimit <- c(min(df_training_subset$x) - 0.3, max(df_training_subset$x) + 0.3)
  #   
  #   # g1a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[1]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(
  #   #     alpha = 0.4,
  #   #     # size = 2,
  #   #     aes(size = Freq)) + 
  #   #   # geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.7) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 1")
  #   
  #   # ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[1]]) %>% filter(Freq!=0), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(alpha = 0.4, size = 2) +
  #   #   geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.5) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 1")
  #   g1a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[1]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 1")
  #   g2a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[2]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 2")
  #   g3a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[3]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 3")
  #   g4a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[4]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 4")
  #   g5a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[5]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 5")
  #   g6a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[6]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #     geom_point(
  #       alpha = 0.4,
  #       # size = 2,
  #       aes(size = Freq)) + 
  #     geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), size = 3) +
  #     theme_bw() + xlim(xlimit) + ylim(ylimit) + theme(legend.position = "none") +
  #     labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 6")
  #   # g3a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[3]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(
  #   #     alpha = 0.4,
  #   #     size = 2,
  #   #     aes(color = Freq)) + scale_color_colorblind() +
  #   #   # geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.7) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 3")
  #   # g4a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[4]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(
  #   #     alpha = 0.4,
  #   #     size = 2,
  #   #     aes(color = Freq)) + scale_color_colorblind() +
  #   #   geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.7) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 4")
  #   # g5a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[5]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(
  #   #     # alpha = 0.4,
  #   #     size = 2,
  #   #     aes(alpha = Freq)) + 
  #   #   # geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.7) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 5")
  #   # g6a <- ggplot(data = inner_join(data.frame(Var1=rownames(df_training_subset), Freq=rffit$inbag.counts[[6]]) %>% filter(Freq!=0) %>% mutate(Freq = as.factor(Freq)), df_training_subset), aes(x = x, y = y_orig)) +
  #   #   geom_point(
  #   #     # alpha = 0.4,
  #   #     size = 2,
  #   #     aes(alpha = Freq)) + 
  #   #   geom_text(aes(label = ifelse(Freq != 1, Freq, NA)), vjust = -0.7) +
  #   #   theme_bw() + xlim(xlimit) + ylim(ylimit) +
  #   #   labs(x = "Predictor x", y = "Response y", title = "Bootstrapped dataset 6")
  #   
  #   # (g1a | g2a | g3a)/(g4a | g5a | g6a)
  #   
  #   g1b <- g1a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,1]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   g2b <- g2a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,2]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   g3b <- g3a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,3]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   g4b <- g4a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,4]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   g5b <- g5a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,5]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   g6b <- g6a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,6]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1)
  #   
  #   # (g1b | g2b | g3b)/(g4b | g5b | g6b)
  #   
  #   g1c <- g1a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g2c <- g2a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g3c <- g3a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g4c <- g4a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g5c <- g5a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g6c <- g6a + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   
  #   # (g1c | g2c | g3c)/(g4c | g5c | g6c)
  #   
  #   g1d <- g1a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,1]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g2d <- g2a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,2]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g3d <- g3a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,3]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g4d <- g4a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,4]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g5d <- g5a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,5]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   g6d <- g6a + geom_line(data = data.frame(x_seq, y = preds_rf$predictions[,6]), aes(x = x_seq, y = y), color = cb_pallete[7], linewidth = 1) + geom_line(data = df_training_subset, aes(x = x, y = fx), color = cb_pallete[1], linewidth = 1)
  #   
  #   
  #   # figure <- ggarrange(g1b + rremove("ylab") + rremove("xlab"), 
  #   #                     g2b + rremove("ylab") + rremove("xlab"), 
  #   #                     g3b + rremove("ylab") + rremove("xlab"), 
  #   #                     g4b + rremove("ylab") + rremove("xlab"), 
  #   #                     g5b + rremove("ylab") + rremove("xlab"), 
  #   #                     g6b + rremove("ylab") + rremove("xlab"), 
  #   #                     ncol = 2, nrow = 3, common.legend = TRUE)
  #   # 
  #   # annotate_figure(figure, left = textGrob("Response y", rot = 90, vjust = 1, gp = gpar(cex = 1)),
  #   #                 bottom = textGrob("Predictor x", gp = gpar(cex = 1)))
  #   
  #   if(input$plot_rf_trees == TRUE & input$plot_true_rf == FALSE){(g1b | g2b | g3b)/(g4b | g5b | g6b)}
  #   else if(input$plot_rf_trees == FALSE & input$plot_true_rf == TRUE){(g1c | g2c | g3c)/(g4c | g5c | g6c)}
  #   else if(input$plot_rf_trees == TRUE & input$plot_true_rf == TRUE){(g1d | g2d | g3d)/(g4d | g5d | g6d)}
  #   else if(input$plot_rf_trees == FALSE & input$plot_true_rf == FALSE){(g1a | g2a | g3a)/(g4a | g5a | g6a)}
  # })   
  # 
  # 
  # ###########################
  # # bias plots (first bar graph in second tab - BV plots)
  # 
  # output$plot_polynomial_p2a <- renderPlot({
  #   
  #   results <- df_bvt_calc_polynomial()[[1]]
  #   noise_sd <- df_bvt_calc_polynomial()[[4]]
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = "Degree") 
  #   
  #   # if(input$models == "polynomial"){bias_plot }
  #   # else if(input$models == "spline"){bias_plot + labs(x = "Degree of Freedom")}
  #   # else if(input$models == "knn_reg"){bias_plot + labs(x = "K")}
  #   # else if(input$models == "tree_reg"){bias_plot + labs(x = "Tree Depth")}
  #   
  #   # results_pivot <- results %>%
  #   #   pivot_longer(cols = 2:5, names_to = "type", values_to = "values")
  #   # 
  #   # ggplot(data = results_pivot %>% filter(type != "Training")) +
  #   #   geom_point(aes(x = parameter_values, y = values, color = type)) +
  #   #   geom_line(aes(x = parameter_values, y = values, group = type, color = type)) +
  #   #   scale_color_manual(values = cb_pallete[c(8, 1, 4)]) +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top") +
  #   #   labs(y = "", color = "", x = "Degree") +
  #   #   ylim(c(min(results_pivot$values) - 0.03, max(results_pivot$values) + 0.03))
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_colorblind(labels = c("Test MSE", "Training MSE")) +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank()) +
  #     labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Degree") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   mse_plot
  #   
  # })
  # 
  # output$plot_spline_p2a <- renderPlot({
  #   
  #   results <- df_bvt_calc_spline()[[1]]
  #   noise_sd <- df_bvt_calc_spline()[[4]]
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = "Degree of Freedom") 
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_colorblind(labels = c("Test MSE", "Training MSE")) +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank()) +
  #     labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Degrees of Freedom") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   mse_plot
  #   
  #   
  # })
  # 
  # output$plot_knn_reg_p2a <- renderPlot({
  #   
  #   results <- df_bvt_calc_knn_reg()[[1]]
  #   noise_sd <- df_bvt_calc_knn_reg()[[4]]
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = "K") 
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_colorblind(labels = c("Test MSE", "Training MSE")) +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank()) +
  #     labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "K") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   mse_plot
  #   
  # })
  # 
  # output$plot_tree_reg_p2a <- renderPlot({
  #   
  #   results <- df_bvt_calc_tree_reg()[[1]]
  #   noise_sd <- df_bvt_calc_tree_reg()[[4]]
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_colorblind(labels = c("Test MSE", "Training MSE")) +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank()) +
  #     labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Tree Depth") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   mse_plot
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = "Tree Depth")
  #   
  # })
  # 
  # output$plot_lasso_p2a <- renderPlot({
  #   
  #   results <- df_bvt_calc_lasso()[[1]]
  #   noise_sd <- df_bvt_calc_lasso()[[4]]
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_colorblind(labels = c("Test MSE", "Training MSE")) +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank()) +
  #     labs(y = "Mean Squared Error (MSE)", color = "MSE", x = bquote(log[10](lambda))) +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   mse_plot
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = bquote(log[10](lambda)))
  #   
  # })
  # 
  # ###########################
  # # variance plots (second bar graph in second tab - BV plots)
  # 
  # output$plot_polynomial_p2b <- renderPlot({
  #   
  #   results <- df_bvt_calc_polynomial()[[1]]
  #   noise_sd <- df_bvt_calc_polynomial()[[4]]
  #   
  #   # ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance", x = "Degree")
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_manual(values = cb_pallete[c(8, 1, 4)], labels = c(bquote(Bias^2), "Test MSE", "Variance")) +
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.key.spacing.x = unit(0.1, 'cm'), legend.key.size = unit(0.5, "cm")) +
  #     labs(y = "", color = "", x = "Degree") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_test_var_plot
  #   
  # })
  # 
  # output$plot_spline_p2b <- renderPlot({
  #   
  #   results <- df_bvt_calc_spline()[[1]]
  #   noise_sd <- df_bvt_calc_spline()[[4]]
  #   
  #   # ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance", x = "Degree of Freedom")
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_manual(values = cb_pallete[c(8, 1, 4)], labels = c(bquote(Bias^2), "Test MSE", "Variance")) +
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.key.spacing.x = unit(0.1, 'cm'), legend.key.size = unit(0.5, "cm")) +
  #     labs(y = "", color = "", x = "Degrees of Freedom") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_test_var_plot
  #   
  #   
  # })
  # 
  # output$plot_knn_reg_p2b <- renderPlot({
  #   
  #   results <- df_bvt_calc_knn_reg()[[1]]
  #   noise_sd <- df_bvt_calc_knn_reg()[[4]]
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_manual(values = cb_pallete[c(8, 1, 4)], labels = c(bquote(Bias^2), "Test MSE", "Variance")) +
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.key.spacing.x = unit(0.1, 'cm'), legend.key.size = unit(0.5, "cm")) +
  #     labs(y = "", color = "", x = "K") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_test_var_plot
  #   
  #   # ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance", x = "K")
  #   
  #   
  # })
  # 
  # output$plot_tree_reg_p2b <- renderPlot({
  #   
  #   results <- df_bvt_calc_tree_reg()[[1]]
  #   noise_sd <- df_bvt_calc_tree_reg()[[4]]
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_manual(values = cb_pallete[c(8, 1, 4)], labels = c(bquote(Bias^2), "Test MSE", "Variance")) +
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.key.spacing.x = unit(0.1, 'cm'), legend.key.size = unit(0.5, "cm")) +
  #     labs(y = "", color = "", x = "Tree Depth") +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_test_var_plot
  #   
  #   
  #   # ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance", x = "Tree Depth")
  #   
  #   
  # })
  # 
  # output$plot_lasso_p2b <- renderPlot({
  #   
  #   results <- df_bvt_calc_lasso()[[1]]
  #   noise_sd <- df_bvt_calc_lasso()[[4]]
  #   
  #   results_mse <- results %>%
  #     pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   
  #   bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #     geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #     geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #     scale_color_manual(values = cb_pallete[c(8, 1, 4)], labels = c(bquote(Bias^2), "Test MSE", "Variance")) +
  #     geom_hline(yintercept = noise_sd^2, linetype = 2) + theme_bw() +
  #     theme(legend.position = "top", legend.key.spacing.x = unit(0.1, 'cm'), legend.key.size = unit(0.5, "cm")) +
  #     labs(y = "", color = "", x = bquote(log[10](lambda))) +
  #     ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_test_var_plot
  #   
  #   # ggplot(data = results) + 
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance") + labs(x = bquote(log[10](lambda)))
  #   
  # })
  # 
  # 
  # ###########################
  # # Test-Training MSE plots (line graph in second tab - BV plots)
  # 
  # output$plot_polynomial_p2c <- renderPlot({
  #   
  #   # noise_sd <- input$epsilon          # standard deviation of epsilon
  #   
  #   results <- df_bvt_calc_polynomial()[[1]]
  #   # noise_sd <- df_bvt_calc_polynomial()[[4]]
  #   
  #   # # results_mse <- results %>% 
  #   # #   pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # results_mse <- results %>%
  #   #   pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top") +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Degree") +
  #   #   ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   # 
  #   # # mse_plot
  #   # 
  #   # bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_manual(values = cb_pallete[c(8, 1, 4)]) +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top") +
  #   #   labs(y = "", color = "", x = "Degree") +
  #   #   ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   bias_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Bias_sq), fill = cb_pallete[8]) + theme_bw() +
  #     labs(y = "Estimated Squared Bias") + labs(x = "Degree")
  #   
  #   bias_plot
  #   
  #   # variance_plot <- ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Variance)) +
  #   #   labs(y = "Estimated Variance", x = "Degree")
  #   # 
  #   # (mse_plot | bias_test_var_plot) / (bias_plot | variance_plot)
  #   
  #   
  #   
  #   # results_bvt <- results %>% 
  #   #   pivot_longer(cols = c(2,3,5), names_to = "mse_type", values_to = "mse")
  #   # 
  #   # bvt_plot <- ggplot(data = results_bvt) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE")
  #   
  # })
  # 
  # output$plot_polynomial_p2d <- renderPlot({
  #   
  #   # noise_sd <- input$epsilon          # standard deviation of epsilon
  #   
  #   results <- df_bvt_calc_polynomial()[[1]]
  #   # noise_sd <- df_bvt_calc_polynomial()[[4]]
  #   
  #   # # results_mse <- results %>% 
  #   # #   pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # results_mse <- results %>%
  #   #   pivot_longer(cols = 2:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse %>% filter(mse_type %in% c("Test", "Training"))) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() +          # scale_color_manual(values = colorblind_pal()(6)[-c(1:3, 5)])
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top") +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Degree") +
  #   #   ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   # 
  #   # # mse_plot
  #   # 
  #   # bias_test_var_plot <- ggplot(data = results_mse %>% filter(mse_type != "Training")) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_manual(values = cb_pallete[c(8, 1, 4)]) +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top") +
  #   #   labs(y = "", color = "", x = "Degree") +
  #   #   ylim(c(min(results_mse$mse) - 0.03, max(results_mse$mse) + 0.03))
  #   
  #   # bias_plot <- ggplot(data = results) +
  #   #   geom_col(aes(x = parameter_values, y = Bias_sq)) +
  #   #   labs(y = "Estimated Squared Bias") + labs(x = "Degree")
  #   # 
  #   # bias_plot
  #   
  #   variance_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Variance), fill = cb_pallete[4]) + theme_bw() +
  #     labs(y = "Estimated Variance", x = "Degree") 
  # 
  #   
  #   variance_plot
  #   # (mse_plot | bias_test_var_plot) / (bias_plot | variance_plot)
  #   
  #   
  #   
  #   # results_bvt <- results %>% 
  #   #   pivot_longer(cols = c(2,3,5), names_to = "mse_type", values_to = "mse")
  #   # 
  #   # bvt_plot <- ggplot(data = results_bvt) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE")
  #   
  # })
  # 
  # 
  # output$plot_spline_p2c <- renderPlot({
  #   
  #   results <- df_bvt_calc_spline()[[1]]
  #   
  #   bias_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Bias_sq), fill = cb_pallete[8]) + theme_bw() +
  #     labs(y = "Estimated Squared Bias") + labs(x = "Degrees of Freedom")
  #   
  #   bias_plot
  #   
  #   # noise_sd <- df_bvt_calc_spline()[[4]]
  # 
  #   # results_mse <- results %>%
  #   #   pivot_longer(cols = 4:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() +
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Degree of Freedom")
  #   # 
  #   # mse_plot
  #   
  # })
  # 
  # output$plot_spline_p2d <- renderPlot({
  #   
  #   results <- df_bvt_calc_spline()[[1]]
  # 
  #   variance_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Variance), fill = cb_pallete[4]) + theme_bw() +
  #     labs(y = "Estimated Variance", x = "Degrees of Freedom") 
  #   
  #   variance_plot
  # 
  # })
  # 
  # 
  # output$plot_knn_reg_p2c <- renderPlot({
  #   
  #   results <- df_bvt_calc_knn_reg()[[1]]
  #   
  #   bias_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Bias_sq), fill = cb_pallete[8]) + theme_bw() +
  #     labs(y = "Estimated Squared Bias") + labs(x = "K")
  #   
  #   bias_plot
  #   
  #   # noise_sd <- df_bvt_calc_knn_reg()[[4]]
  #   # 
  #   # results_mse <- results %>% 
  #   #   pivot_longer(cols = 4:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() + 
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "K")
  #   # 
  #   # mse_plot
  #   
  # })
  # 
  # output$plot_knn_reg_p2d <- renderPlot({
  #   
  #   results <- df_bvt_calc_knn_reg()[[1]]
  #   
  #   variance_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Variance), fill = cb_pallete[4]) + theme_bw() +
  #     labs(y = "Estimated Variance", x = "K") 
  #   
  #   variance_plot
  #   
  # })
  # 
  # 
  # output$plot_tree_reg_p2c <- renderPlot({
  #   
  #   results <- df_bvt_calc_tree_reg()[[1]]
  #   
  #   bias_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Bias_sq), fill = cb_pallete[8]) + theme_bw() +
  #     labs(y = "Estimated Squared Bias") + labs(x = "Tree Depth")
  #   
  #   bias_plot
  #   
  #   # noise_sd <- df_bvt_calc_tree_reg()[[4]]
  #   # 
  #   # results_mse <- results %>% 
  #   #   pivot_longer(cols = 4:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() + 
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = "Tree Depth")
  #   # 
  #   # mse_plot
  #   
  # })
  # 
  # output$plot_tree_reg_p2d <- renderPlot({
  #   
  #   results <- df_bvt_calc_tree_reg()[[1]]
  #   
  #   variance_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Variance), fill = cb_pallete[4]) + theme_bw() +
  #     labs(y = "Estimated Variance", x = "Tree Depth") 
  #   
  #   variance_plot
  #   
  # })
  # 
  # 
  # output$plot_lasso_p2c <- renderPlot({
  #   
  #   results <- df_bvt_calc_lasso()[[1]]
  #   
  #   bias_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Bias_sq), fill = cb_pallete[8]) + theme_bw() +
  #     labs(y = "Estimated Squared Bias") + labs(x = bquote(log[10](lambda)))
  #   
  #   bias_plot
  #   
  #   # noise_sd <- df_bvt_calc_lasso()[[4]]
  #   # 
  #   # results_mse <- results %>% 
  #   #   pivot_longer(cols = 4:5, names_to = "mse_type", values_to = "mse")
  #   # 
  #   # mse_plot <- ggplot(data = results_mse) +
  #   #   geom_point(aes(x = parameter_values, y = mse, color = mse_type)) +
  #   #   geom_line(aes(x = parameter_values, y = mse, group = mse_type, color = mse_type)) +
  #   #   scale_color_colorblind() + 
  #   #   geom_hline(yintercept = noise_sd^2, linetype = 2) +
  #   #   theme(legend.position = "top", legend.text = element_text("MSE")) +
  #   #   labs(y = "Mean Squared Error (MSE)", color = "MSE", x = bquote(log[10](lambda)))
  #   # 
  #   # mse_plot
  #   
  # })
  # 
  # output$plot_lasso_p2d <- renderPlot({
  #   
  #   results <- df_bvt_calc_lasso()[[1]]
  #   
  #   variance_plot <- ggplot(data = results) +
  #     geom_col(aes(x = parameter_values, y = Variance), fill = cb_pallete[4]) + theme_bw() +
  #     labs(y = "Estimated Variance", x = bquote(log[10](lambda))) 
  #   
  #   variance_plot
  #   
  # })
  # 
  # 
  # output$plot_rf_p2c <- renderPlot({
  #   
  #   df_training <- df_data_regression()[[1]]
  #   df_test <- df_data_regression()[[2]]
  #   df_training_xs <- df_data_regression()[[3]]
  #   df_training_ys <- df_data_regression()[[4]]
  #   num_training_points <- df_data_regression()[[6]]
  #   
  #   # results <- df_bvt_calc_lasso()[[1]]
  #   # noise_sd <- df_bvt_calc_lasso()[[4]]
  #   
  #   df_training_preds_tree <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds_tree <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse_tree <- rep(NA, num_rep_sets_reg)
  #   
  #   # FOR AVERAGE TRAINING AND TEST MSE FROM REPLICATED DATASETS AND TEST SET
  #   for(j in 1:num_rep_sets_reg)
  #   {
  #     
  #     # fit model of d degrees of freedom to each replicated (training) set
  #     # get predictions (y_hat's) for each replicated (training) set and test set
  #     y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #     m <- rpart(y_rep ~ x_rep, method = "anova", 
  #                control = rpart.control(cp = 0, xval = 0, minbucket = 1))
  #     y_hat_training <- predict(m, newdata = data.frame(x_rep))
  #     y_hat_test <- predict(m, newdata = data.frame(x_rep = df_test$x))
  #     
  #     # store predictions for each set
  #     df_training_preds_tree[,j] <- y_hat_training
  #     df_test_preds_tree[,j] <- y_hat_test
  #     
  #     # calculate training MSE for each replicated (training) dataset
  #     train_mse_tree[j] <- mean((y_rep-y_hat_training)^2)
  #     
  #   }
  #   
  #   
  #   min_y_limit <- min(sapply(df_test_preds_tree, min))
  #   max_y_limit <- max(sapply(df_test_preds_tree, max))
  #   
  #   df_test_preds_selected_tree <- data.frame(df_test_preds_tree)
  #   df_test_preds_selected_tree$mean_pred <- apply(df_test_preds_tree,1,mean)
  #   df_test_preds_selected_tree$expl <- df_test$x
  #   df_test_preds_selected_pivot_tree <- df_test_preds_selected_tree %>%
  #     pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #   
  #   # g_tree <- ggplot() +
  #   #   geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #   #   geom_line(data = df_test_preds_selected_pivot_tree,
  #   #             aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #   #   geom_line(data = df_test_preds_selected_pivot_tree,
  #   #             aes(x = expl, y = mean_pred, color = "average fit")) +
  #   #   labs(y = "y", title = paste0("Single Deep Tree Fit")) +
  #   #   scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #   #                       values = c("black", "orange", "red")) +
  #   #   theme_bw() +
  #   #   theme(legend.position = "top", legend.title = element_blank(),
  #   #         legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #   #         legend.key = element_rect(color = NA, fill = NA)) +
  #   #   ylim(c(min_y_limit, max_y_limit))
  #   
  #   
  #   
  #   df_training_preds_rf <- matrix(nrow = num_training_points, ncol = num_rep_sets_reg)
  #   df_test_preds_rf <- matrix(nrow = num_test_points_reg, ncol = num_rep_sets_reg)
  #   train_mse_rf <- rep(NA, num_rep_sets_reg)
  #   
  #   for(j in 1:num_rep_sets_reg)
  #   {
  #     
  #     # fit model of d degrees of freedom to each replicated (training) set
  #     # get predictions (y_hat's) for each replicated (training) set and test set
  #     y_rep <- df_training_ys[,j]; x_rep <- df_training_xs
  #     
  #     m_rf <- randomForest(x = data.frame(x_rep), y = y_rep, xtest = data.frame(df_test$x), ytest = df_test$y_orig, #data = data.frame(y_rep = y_rep, x_rep = x_rep),
  #                          ntree = 50, # number of trees to grow (bootstrap samples) usually 500
  #                          mtry = 1, 
  #                          nodesize = 1)
  #     
  #     y_hat_training_rf <- m_rf$predicted
  #     y_hat_test_rf <- m_rf$test$predicted
  #     
  #     # store predictions for each set
  #     df_training_preds_rf[,j] <- y_hat_training_rf
  #     df_test_preds_rf[,j] <- y_hat_test_rf
  #     
  #     # calculate training MSE for each replicated (training) dataset
  #     train_mse_rf[j] <- mean((y_rep-y_hat_training_rf)^2)
  #     
  #   }
  #   
  #   df_test_preds_selected_rf <- data.frame(df_test_preds_rf)
  #   df_test_preds_selected_rf$mean_pred <- apply(df_test_preds_rf,1,mean)
  #   df_test_preds_selected_rf$expl <- df_test$x
  #   df_test_preds_selected_pivot_rf <- df_test_preds_selected_rf %>% 
  #     pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #   
  #   # g_rf <- ggplot() + 
  #   #   geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #   #   geom_line(data = df_test_preds_selected_pivot_rf, 
  #   #             aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #   #   geom_line(data = df_test_preds_selected_pivot_rf, 
  #   #             aes(x = expl, y = mean_pred, color = "average fit")) +
  #   #   labs(y = "y", title = paste0("Random Forest Fit")) +
  #   #   scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #   #                       values = c("black", "orange", "red")) +
  #   #   theme_bw() +
  #   #   theme(legend.position = "top", legend.title = element_blank(), 
  #   #         legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #   #         legend.key = element_rect(color = NA, fill = NA)) +
  #   #   ylim(c(min_y_limit, max_y_limit)) 
  #   
  #   
  #   g_bias <- ggplot() + 
  #     geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1) +
  #     # geom_line(data = df_test_preds_selected_pivot_tree, 
  #     #           aes(x = expl, y = preds, group = rep_data, color = "replicated fits 1"), linetype = 3) +
  #     geom_line(data = df_test_preds_selected_pivot_tree, 
  #               aes(x = expl, y = mean_pred, color = "single tree average fit"), linewidth = 1.2) +
  #     # geom_line(data = df_test_preds_selected_pivot_rf,
  #     #           aes(x = expl, y = preds, group = rep_data, color = "replicated fits 2"), linetype = 3) +
  #     geom_line(data = df_test_preds_selected_pivot_rf,
  #               aes(x = expl, y = mean_pred, color = "RF average fit"), linewidth = 1.2) +
  #     labs(y = "y", title = paste0("Bias Comparison")) +
  #     scale_colour_manual(breaks = c("true f(x)", "single tree average fit", "RF average fit"),
  #                         values = c("black", "orange", "darkgreen")) +
  #     theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank(), 
  #           legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #           legend.key = element_rect(color = NA, fill = NA)) +
  #     ylim(c(min_y_limit, max_y_limit)) 
  #   
  #   # g_variance <- ggplot() + 
  #   #   geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 2) +
  #   #   geom_line(data = df_test_preds_selected_pivot_tree,
  #   #             aes(x = expl, y = preds, group = rep_data, color = "single tree replicated fits"), linetype = 3) +
  #   #   # geom_line(data = df_test_preds_selected_pivot_tree, 
  #   #   #           aes(x = expl, y = mean_pred, color = "average fit 1")) +
  #   #   geom_line(data = df_test_preds_selected_pivot_rf,
  #   #             aes(x = expl, y = preds, group = rep_data, color = "random forest replicated fits"), linetype = 3) +
  #   #   # geom_line(data = df_test_preds_selected_pivot_rf,
  #   #   #           aes(x = expl, y = mean_pred, color = "average fit 2")) +
  #   #   labs(y = "y", title = paste0("Variance Comparison")) +
  #   #   scale_colour_manual(breaks = c("true f(x)", "single tree replicated fits", "random forest replicated fits"),
  #   #                       values = c("black", "orange", "darkgreen")) +
  #   #   theme_bw() +
  #   #   theme(legend.position = "top", legend.title = element_blank(), 
  #   #         legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #   #         legend.key = element_rect(color = NA, fill = NA)) +
  #   #   ylim(c(min_y_limit, max_y_limit)) 
  #   
  #   # g_tree + g_rf
  #   # g_bias / g_variance
  #   # ggarrange(g_tree, g_rf,
  #   #           ncol = 3, nrow = 1, common.legend = TRUE)
  #   # 
  #   
  #   
  #   df_test_preds_selected_rf <- data.frame(df_test_preds_rf)
  #   df_test_preds_selected_rf <- df_test_preds_selected_rf %>% 
  #     mutate(max_pred = apply(df_test_preds_rf,1,max),
  #            min_pred = apply(df_test_preds_rf,1,min),
  #            mean_pred = apply(df_test_preds_rf,1,mean),
  #            expl = df_test$x)
  #   df_test_preds_selected_tree <- data.frame(df_test_preds_tree)
  #   df_test_preds_selected_tree <- df_test_preds_selected_tree %>% 
  #     mutate(max_pred = apply(df_test_preds_tree,1,max),
  #            min_pred = apply(df_test_preds_tree,1,min),
  #            mean_pred = apply(df_test_preds_tree,1,mean),
  #            expl = df_test$x)
  #   g_variance <- ggplot() + 
  #     geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #     geom_linerange(data = df_test_preds_selected_tree,
  #                    aes(x = expl, y = mean_pred, ymin = min_pred, ymax = max_pred, color = "single tree fit"), linewidth = 0.3) +
  #     geom_linerange(data = df_test_preds_selected_rf,
  #                    aes(x = expl, y = mean_pred, ymin = min_pred, ymax = max_pred, color = "RF fit"), linewidth = 0.3) +
  #     labs(y = "y", title = paste0("Variance Comparison")) +
  #     scale_colour_manual(breaks = c("true f(x)", "single tree fit", "RF fit"),
  #                         values = c("black", "orange", "darkgreen")) +
  #     theme_bw() +
  #     theme(legend.position = "top", legend.title = element_blank(), 
  #           legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #           legend.key = element_rect(color = NA, fill = NA)) +
  #     ylim(c(min_y_limit, max_y_limit)) 
  #   
  #   
  #   g_bias + g_variance
  #   
  #   
  #   
  # })   
  # 
  # ###########################
  # # plots in third tab - Replicated datasets
  # 
  # output$plot_polynomial_p3 <- renderPlot({
  #   
  #   # if(input$bvgraphs == TRUE){
  #   
  #   # degrees_of_freedom <- df()[[3]]
  #   df_test <- df_data_regression()[[2]]
  #   parameter_values <- df_bvt_calc_polynomial()[[2]]
  #   df_test_preds_list <- df_bvt_calc_polynomial()[[3]]
  #   min_y_limit <- min(sapply(df_test_preds_list, min))
  #   max_y_limit <- max(sapply(df_test_preds_list, max))
  #   
  #   
  #   replicated_datasets_graphs <- list()
  #   for(count in seq_along(parameter_values))
  #   {
  #     # df_test_preds_selected <- data.frame(df_test_preds_list[[count]][,sample_reps])
  #     df_test_preds_selected <- data.frame(df_test_preds_list[[count]])
  #     df_test_preds_selected$mean_pred <- apply(df_test_preds_list[[count]],1,mean)
  #     df_test_preds_selected$expl <- df_test$x
  #     df_test_preds_selected_pivot <- df_test_preds_selected %>% 
  #       pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #     
  #     replicated_datasets_graphs[[count]] <- ggplot() + 
  #       geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +  # MAYBE CHANGE LINEWIDTH AND LAYER THIS AFTER THE AVERAGE FIT
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = mean_pred, color = "average fit")) +    # ADD LINEWIDTH = 1.3
  #       # geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 0.8) +
  #       labs(y = "y", title = paste0("Degree ", parameter_values[count], " Fit")) +
  #       scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #                           values = c("black", "orange", "red")) +    # colorblind_pal()(7)[-c(3:6)]
  #       theme_bw() +
  #       theme(legend.position = "top", legend.title = element_blank(), 
  #             legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
  #             legend.key.spacing.x = unit(2.0, "cm"), legend.key = element_rect(color = NA, fill = NA)) +
  #       ylim(c(min_y_limit, max_y_limit))
  #     
  #     # NEED TO WORK ON THE LEGEND SPACING
  #     
  #   }
  #   
  #   # g1 <- ggplot() + geom_line(data = df_test, aes(x = x, y = fx)) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[1]], 
  #   #             aes(x = x, y = preds, group = rep_data), linetype = 3) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[1]], 
  #   #             aes(x = x, y = mean_pred), color = "red")
  #   # 
  #   # g2 <- ggplot() + geom_line(data = df_test, aes(x = x, y = fx)) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[2]], 
  #   #             aes(x = x, y = preds, group = rep_data), linetype = 3) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[2]], 
  #   #             aes(x = x, y = mean_pred), color = "red")
  #   # 
  #   # g3 <- ggplot() + geom_line(data = df_test, aes(x = x, y = fx)) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[3]], 
  #   #             aes(x = x, y = preds, group = rep_data), linetype = 3) +
  #   #   geom_line(data = df_test_preds_selected_pivot[[3]], 
  #   #             aes(x = x, y = mean_pred), color = "red")
  #   
  #   
  #   # ggarrange(replicated_datasets_graphs[[1]], 
  #   #           replicated_datasets_graphs[[2]],
  #   #           replicated_datasets_graphs[[3]],
  #   #           replicated_datasets_graphs[[4]],
  #   #           replicated_datasets_graphs[[5]], 
  #   #           replicated_datasets_graphs[[6]], 
  #   #           replicated_datasets_graphs[[7]], 
  #   #           replicated_datasets_graphs[[8]], 
  #   #           ncol = 4, nrow = 2, common.legend = TRUE)
  #   
  #   (replicated_datasets_graphs[[1]] | replicated_datasets_graphs[[2]] | replicated_datasets_graphs[[3]] | replicated_datasets_graphs[[4]]) /
  #   (replicated_datasets_graphs[[5]] | replicated_datasets_graphs[[6]] | replicated_datasets_graphs[[7]] | replicated_datasets_graphs[[8]]) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  #   
  #   
  #   # }
  #   
  #   
  # })
  # 
  # output$plot_spline_p3 <- renderPlot({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   parameter_values <- df_bvt_calc_spline()[[2]]
  #   df_test_preds_list <- df_bvt_calc_spline()[[3]]
  #   min_y_limit <- min(sapply(df_test_preds_list, min))
  #   max_y_limit <- max(sapply(df_test_preds_list, max))
  #   
  #   replicated_datasets_graphs <- list()
  #   for(count in seq_along(parameter_values))
  #   {
  #     # df_test_preds_selected <- data.frame(df_test_preds_list[[count]][,sample_reps])
  #     df_test_preds_selected <- data.frame(df_test_preds_list[[count]])
  #     df_test_preds_selected$mean_pred <- apply(df_test_preds_list[[count]],1,mean)
  #     df_test_preds_selected$expl <- df_test$x
  #     df_test_preds_selected_pivot <- df_test_preds_selected %>% 
  #       pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #     
  #     replicated_datasets_graphs[[count]] <- ggplot() + 
  #       geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = mean_pred, color = "average fit")) +
  #       labs(y = "y", title = paste0("Degrees of Freedom = ", parameter_values[count], " Fit")) +
  #       scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #                           values = c("black", "orange", "red")) +
  #       theme_bw() +
  #       theme(legend.position = "top", legend.title = element_blank(), 
  #             legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #             legend.key.spacing.x = unit(2.0, "cm"), legend.key = element_rect(color = NA, fill = NA)) +
  #       ylim(c(min_y_limit, max_y_limit)) 
  #   }
  #   
  #   ggarrange(replicated_datasets_graphs[[1]], 
  #             replicated_datasets_graphs[[2]],
  #             replicated_datasets_graphs[[3]],
  #             replicated_datasets_graphs[[4]],
  #             replicated_datasets_graphs[[5]], 
  #             replicated_datasets_graphs[[6]],
  #             replicated_datasets_graphs[[7]],
  #             ncol = 4, nrow = 2, common.legend = TRUE)
  #   
  # })
  # 
  # output$plot_knn_reg_p3 <- renderPlot({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   parameter_values <- df_bvt_calc_knn_reg()[[2]]
  #   df_test_preds_list <- df_bvt_calc_knn_reg()[[3]]
  #   min_y_limit <- min(sapply(df_test_preds_list, min))
  #   max_y_limit <- max(sapply(df_test_preds_list, max))
  #   
  #   replicated_datasets_graphs <- list()
  #   for(count in seq_along(parameter_values))
  #   {
  #     # df_test_preds_selected <- data.frame(df_test_preds_list[[count]][,sample_reps])
  #     df_test_preds_selected <- data.frame(df_test_preds_list[[count]])
  #     df_test_preds_selected$mean_pred <- apply(df_test_preds_list[[count]],1,mean)
  #     df_test_preds_selected$expl <- df_test$x
  #     df_test_preds_selected_pivot <- df_test_preds_selected %>% 
  #       pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #     
  #     replicated_datasets_graphs[[count]] <- ggplot() + 
  #       geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = mean_pred, color = "average fit")) +
  #       # geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 0.8) +
  #       labs(y = "y", title = paste0("K = ", parameter_values[count], " Fit")) +
  #       scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #                           values = c("black", "orange", "red")) +
  #       theme_bw() +
  #       theme(legend.position = "top", legend.title = element_blank(), 
  #             legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #             legend.key.spacing.x = unit(2.0, "cm"), legend.key = element_rect(color = NA, fill = NA)) +
  #       ylim(c(min_y_limit, max_y_limit))
  #     
  #   }
  #   
  #   ggarrange(replicated_datasets_graphs[[1]], 
  #             replicated_datasets_graphs[[2]],
  #             replicated_datasets_graphs[[3]],
  #             replicated_datasets_graphs[[4]],
  #             replicated_datasets_graphs[[5]], 
  #             replicated_datasets_graphs[[6]],
  #             replicated_datasets_graphs[[7]],
  #             replicated_datasets_graphs[[8]], 
  #             ncol = 4, nrow = 2, common.legend = TRUE)
  #   
  # })
  # 
  # output$plot_tree_reg_p3 <- renderPlot({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   parameter_values <- df_bvt_calc_tree_reg()[[2]]
  #   df_test_preds_list <- df_bvt_calc_tree_reg()[[3]]
  #   min_y_limit <- min(sapply(df_test_preds_list, min))
  #   max_y_limit <- max(sapply(df_test_preds_list, max))
  #   
  #   replicated_datasets_graphs <- list()
  #   for(count in seq_along(parameter_values))
  #   {
  #     # df_test_preds_selected <- data.frame(df_test_preds_list[[count]][,sample_reps])
  #     df_test_preds_selected <- data.frame(df_test_preds_list[[count]])
  #     df_test_preds_selected$mean_pred <- apply(df_test_preds_list[[count]],1,mean)
  #     df_test_preds_selected$expl <- df_test$x
  #     df_test_preds_selected_pivot <- df_test_preds_selected %>% 
  #       pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #     
  #     replicated_datasets_graphs[[count]] <- ggplot() + 
  #       geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = mean_pred, color = "average fit")) +
  #       labs(y = "y", title = paste0("Depth = ", parameter_values[count], " Fit")) +
  #       scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #                           values = c("black", "orange", "red")) +
  #       theme_bw() +
  #       theme(legend.position = "top", legend.title = element_blank(), 
  #             legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #             legend.key.spacing.x = unit(2.0, "cm"), legend.key = element_rect(color = NA, fill = NA)) +
  #       ylim(c(min_y_limit, max_y_limit)) 
  #   }
  #   
  #   ggarrange(replicated_datasets_graphs[[1]], 
  #             replicated_datasets_graphs[[2]],
  #             replicated_datasets_graphs[[3]],
  #             replicated_datasets_graphs[[4]],
  #             replicated_datasets_graphs[[5]], 
  #             replicated_datasets_graphs[[6]], 
  #             replicated_datasets_graphs[[7]],
  #             ncol = 4, nrow = 2, common.legend = TRUE)
  #   
  # })
  # 
  # output$plot_lasso_p3a <- renderPlot({
  #   
  #   df_test <- df_data_regression()[[2]]
  #   parameter_values <- df_bvt_calc_lasso()[[2]]
  #   df_test_preds_list <- df_bvt_calc_lasso()[[3]]
  #   # min_y_limit <- min(sapply(df_test_preds_list, min))
  #   # max_y_limit <- max(sapply(df_test_preds_list, max))
  #   min_y_limit <- min(df_test$y_orig)
  #   max_y_limit <- max(max(df_test$y_orig))
  #   
  #   replicated_datasets_graphs <- list()
  #   for(count in seq_along(parameter_values))
  #   {
  #     # df_test_preds_selected <- data.frame(df_test_preds_list[[count]][,sample_reps])
  #     df_test_preds_selected <- data.frame(df_test_preds_list[[count]])
  #     df_test_preds_selected$mean_pred <- apply(df_test_preds_list[[count]],1,mean)
  #     df_test_preds_selected$expl <- df_test$x
  #     df_test_preds_selected_pivot <- df_test_preds_selected %>% 
  #       pivot_longer(cols = starts_with("X"), names_to = "rep_data", values_to = "preds")
  #     
  #     replicated_datasets_graphs[[count]] <- ggplot() + 
  #       geom_line(data = df_test, aes(x = x, y = fx, color = "true f(x)"), linewidth = 1.5) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = preds, group = rep_data, color = "replicated fits"), linetype = 3) +
  #       geom_line(data = df_test_preds_selected_pivot, 
  #                 aes(x = expl, y = mean_pred, color = "average fit")) +
  #       labs(y = "y", title = paste0("\u03bb"," = 10^", log10(parameter_values[count]), " Fit")) +
  #       scale_colour_manual(breaks = c("true f(x)", "replicated fits", "average fit"),
  #                           values = c("black", "orange", "red")) +
  #       theme_bw() +
  #       theme(legend.position = "top", legend.title = element_blank(), 
  #             legend.text=element_text(size=13), legend.key.size = unit(1.0, "cm"),
  #             legend.key.spacing.x = unit(2.0, "cm"), legend.key = element_rect(color = NA, fill = NA)) +
  #       ylim(c(min_y_limit, max_y_limit)) 
  #   }
  #   
  #   ggarrange(replicated_datasets_graphs[[1]], 
  #             replicated_datasets_graphs[[2]],
  #             replicated_datasets_graphs[[3]],
  #             replicated_datasets_graphs[[4]],
  #             replicated_datasets_graphs[[5]], 
  #             replicated_datasets_graphs[[6]], 
  #             replicated_datasets_graphs[[7]],
  #             replicated_datasets_graphs[[8]],
  #             ncol = 4, nrow = 2, common.legend = TRUE)
  #   
  # })
  # 
  # output$plot_lasso_p3b <- renderPlot({
  #   
  #   parameter_values <- df_bvt_calc_lasso()[[2]]
  #   model_coefficients <- df_bvt_calc_lasso()[[5]]
  #   poly_degree <- 5
  #   
  #   coefs <- plyr::ldply(model_coefficients, data.frame)
  #   colnames(coefs) <- paste0("beta", 1:poly_degree)
  #   coefs$lambda <- rep(log10(parameter_values), each = num_rep_sets_reg)
  #   coefs2 <- pivot_longer(coefs, cols = starts_with("b"), names_to = "coef", values_to = "coef_values")
  #   coefs2$coef <- factor(coefs2$coef, levels = paste0("beta", 1:poly_degree), 
  #                         labels = paste0("beta[", 1:poly_degree, "]"))
  #   
  #   ggplot(data = coefs2, aes(x = factor(lambda), y = coef_values)) + 
  #     geom_boxplot() + 
  #     facet_wrap(~ coef, scales = "free_y", nrow = 2, ncol = 5, labeller = label_parsed) +
  #     labs(y = "Coefficient Estimates", x = bquote(log[10](lambda)))    
  # })
  
  ###########################
  # training data plots in classification first tab - Model Fits
  # INCLUDE A CHECKBOX INPUT TO SELECT AND COMPARE DIFFERENT FITS
  
  cb_pallete <- colorblind_pal()(8)
  
  # output$plot_logreg_p1a <- renderPlotly({
  #   
  #   df_training <- df_data_classification()[[1]]
  #   
  #   training_data_plot <- ggplot() + 
  #              geom_point(data = df_training, aes(x=x1, y = x2, color = y_orig), size = 2) +
  #              theme_minimal() + 
  #              theme(legend.position = "top") +
  #              theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
  #                    axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  #              labs(x = expression(x[1]), y = expression(x[2]), title = "Training Data") +
  #              scale_color_colorblind(name = "True Class") +
  #              xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
  #              ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
  #              theme(panel.background = element_rect(color = "black", linewidth = 1))
  #   
  #   ggplotly(training_data_plot)
  #   
  # }) %>% bindCache(df_data_classification())
  
  output$plot_logreg_p1a <- output$plot_knn_class_p1a <- output$plot_tree_class_p1a <- output$plot_svm_p1a <- output$plot_rf_class_p1a <- renderPlot({

    df_training <- df_data_classification()[[1]]

    ggplot() +
      geom_point(data = df_training, aes(x=x1, y = x2, color = y_orig), size = 2) +
      theme_minimal() +
      theme(legend.position = "top") +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = expression(x[1]), y = expression(x[2]), title = "Training Data") +
      # labs(title = "Training Data") +
      scale_color_colorblind(name = "True Class") +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) +
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
      theme(panel.background = element_rect(color = "black", linewidth = 1))

  }) %>% bindCache(df_data_classification())
  
  
  ###########################
  # decision boundary plots in classification first tab - Model Fits
  # INCLUDE A CHECKBOX INPUT TO SELECT AND COMPARE DIFFERENT FITS
  
  output$plot_logreg_p1b <- renderPlot({
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_data_classification()[[8]]
    
    lgfit <- glm(y_orig ~ x1 + x2, data = df_training, family = binomial)
    # df_training$logreg_preds <- factor(ifelse(lgfit$fitted.values >= 0.5, 1, 0))
    
    # alldf$logreg_probs <- as.vector(lgfit$coefficients[1]) + 
    #   (as.vector(lgfit$coefficients[2]) * alldf$xeg) + 
    #   (as.vector(lgfit$coefficients[3]) * alldf$yeg)
    # alldf$logreg_preds <- factor(ifelse(alldf$logreg_probs>0, 1, 0))
    
    alldf$logreg_links <- predict(lgfit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), type = "link")
    alldf$logreg_probs <- predict(lgfit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), type = "response")
    alldf$logreg_preds <- factor(ifelse(alldf$logreg_probs>=0.5, 1, 0))
    
    ggplot(data = alldf)+
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = logreg_preds), alpha = 0.3) + theme_minimal() +
      geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      geom_contour(aes(x = xeg, y = yeg, z = logreg_links), 
                   breaks = 0, color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), title = "Logistic Regression Decision Boundary") +
      # labs(title = "Logistic Regression Decision Boundary") +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      theme(legend.position = "top") +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
      theme(panel.background = element_rect(color = "black", linewidth = 1))
    
    
    
  }) %>% bindCache(df_data_classification())
  
  
  
  output$plot_knn_class_p1b <- renderPlot({
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_data_classification()[[8]]
    parameter_values <- df_bvt_calc_knn_class()[[2]]
    k_selected <- input$k_values_class
    index <- which(parameter_values == k_selected)
    
    knnfit <- knn3(y_orig ~ x1+x2, data = df_training, k = parameter_values[index])
    
    alldf$knn_class_preds <- predict(knnfit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), type = "class")
    # knn_class_preds_actual <- predict(knnfit, newdata = df_training, type = "class")
    
    # df_training$knn_preds <- knn_class_preds_actual
    
    ggplot(data = alldf)+
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = knn_class_preds), alpha = 0.3) + theme_minimal() +
      geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      geom_contour(aes(x = xeg, y = yeg, z = as.numeric(knn_class_preds)), 
                   breaks = 1.5, color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), title = paste(parameter_values[index], "-NN Decision Boundary", sep = "")) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      theme(legend.position = "top") +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
      theme(panel.background = element_rect(color = "black", linewidth = 1))
    
    
    
  }) %>% bindCache(df_data_classification(), df_bvt_calc_knn_class(), input$k_values_class)
  
  
  
  output$plot_tree_class_p1b <- renderPlot({
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_data_classification()[[8]]
    parameter_values <- df_bvt_calc_tree_class()[[2]]
    depth_selected <- input$depths_class
    index <- which(parameter_values == depth_selected)
    
    treefit <- rpart(y_orig ~ x1 + x2, data = df_training, method = "class", 
                     control = rpart.control(cp = 0, xval = 0, minbucket = 1, maxdepth = parameter_values[index]))
    
    alldf$tree_preds <- predict(treefit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), type = "class")
    # tree_class_preds_actual <- predict(treefit, newdata = df_training, type = "class")
    
    # alldf$tree_preds <- tree_class_preds
    # df_training$tree_preds <- tree_class_preds_actual
    
    ggplot(data = alldf)+
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = tree_preds), alpha = 0.3) + theme_minimal() +
      geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      geom_contour(aes(x = xeg, y = yeg, z = as.numeric(tree_preds)), 
                   breaks = c(1.5), color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), title = paste0("Tree depth = ", parameter_values[index], " Decision Boundary")) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      theme(legend.position = "top") +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
      theme(panel.background = element_rect(color = "black", linewidth = 1))
    
  }) %>% bindCache(df_data_classification(), df_bvt_calc_tree_class(), input$depths_class)
  
  output$plot_tree_class_p1c <- renderPlot({
    
    df_training <- df_data_classification()[[1]]
    parameter_values <- df_bvt_calc_tree_class()[[2]]
    depth_selected <- input$depths_class
    index <- which(parameter_values == depth_selected)
    
    treefit <- rpart(y_orig ~ x1 + x2, data = df_training, method = "class", 
                     control = rpart.control(cp = 0, xval = 0, minbucket = 1, maxdepth = parameter_values[index]))
    
    rpart.plot(treefit)
    
  }) %>% bindCache(df_data_classification(), df_bvt_calc_tree_class(), input$depths_class)
  
  
  
  output$plot_svm_p1b <- renderPlot({
    
    # MAKE COMMON XLIM AND YLIM?
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_data_classification()[[8]]
    parameter_values <- df_bvt_calc_svm()[[2]]
    C_selected <- input$C_values
    index <- which(parameter_values == C_selected)
    
    svmfit <- svm(y_orig ~ x1+x2, data = df_training, cost = parameter_values[index], kernel = "linear", scale = TRUE)
    
    svm_class_preds <- predict(svmfit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg))
    # svm_class_preds_actual <- predict(svmfit, newdata = df_training)   # NOT REQUIRED
    
    support_vectors <- df_training[svmfit$index, ]
    decision_values <- attributes(predict(svmfit, newdata = data.frame(x1 = alldf$xeg, x2 = alldf$yeg), decision.values = TRUE))$decision.values
    
    alldf$svm_preds <- svm_class_preds
    # df_training$svm_preds <- svm_class_preds_actual
    
    # cb_palette <- colorblind_pal()(8)   # COLORBLIND PALETTES, TRY OUT DIFFERENT
    ggplot(data = alldf)+
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = svm_preds), alpha = 0.3) + theme_minimal() +
      geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      geom_point(data = support_vectors, aes(x = x1, y = x2), shape = 1, size = 4, color = "black") +
      geom_contour(aes(x = xeg, y = yeg, z = decision_values), 
                   breaks = 0, color = "black") +
      geom_contour(aes(x = xeg, y = yeg, z = decision_values), 
                   breaks = -1, linetype = "dashed", color = "black") +
      geom_contour(aes(x = xeg, y = yeg, z = decision_values), 
                   breaks = 1, linetype = "dashed", color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), 
           title = paste("C = ", parameter_values[index], " Decision Boundary (bold) with Margins (dashed)", sep = ""),
           subtitle = paste("No. of support vectors = ", svmfit$tot.nSV)) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3)) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            legend.position = "top",
            panel.background = element_rect(color = "black", linewidth = 1),
            plot.title = element_text(size=12))
   
    
  }) %>% bindCache(df_data_classification(), df_bvt_calc_svm(), input$C_values)
  
  
  
  output$plot_rf_class_p1b <- renderPlot({
    
    # MAKE COMMON XLIM AND YLIM?
    
    df_training <- df_data_classification()[[1]]
    alldf <- df_bvt_calc_rf_class()[[1]]
    
    ggplot(data = alldf)+
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = rf_preds), alpha = 0.3) + theme_minimal() +
      geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      geom_contour(aes(x = xeg, y = yeg, z = as.numeric(rf_preds)),
                   breaks = 1.5, color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      theme(legend.position = "top") +
      theme(panel.background = element_rect(color = "black", linewidth = 1)) +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
    
  })
  
  ###########################
  # classification second tab - Bias-Variance Table/plots
  

  
  output$table_logreg_p2 <- render_gt({
    
    results <- df_bvt_calc_logreg()[[1]]
    
    results
    
  })
  
  output$plot_knn_class_p2 <- renderPlotly({
    
    bias_variance_error_plots_m(df_data = df_bvt_calc_knn_class(), x_axis_lab = "K")
    
  }) %>% bindCache(df_bvt_calc_knn_class())
  
  output$plot_tree_class_p2 <- renderPlotly({
    
    bias_variance_error_plots_m(df_data = df_bvt_calc_tree_class(), x_axis_lab = "Tree Depth")
    
  }) %>% bindCache(df_bvt_calc_tree_class())
  
  output$plot_svm_p2 <- renderPlotly({
    
    bias_variance_error_plots_m(df_data = df_bvt_calc_svm(), x_axis_lab = "C")
    
  }) %>% bindCache(df_bvt_calc_svm())
  
  output$plot_rf_class_p2c <- renderPlot({
    
    preds_rf <- df_bvt_calc_rf_class()[[2]]
    df_training <- df_data_classification()[[1]]
    alldf <- df_bvt_calc_rf_class()[[1]]
    
    rf_plot <- ggplot(data = alldf)+
      # geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,1])), alpha = 0.3) 
      theme_minimal() +
      # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
      # geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,1]),
      #              breaks = 1.5, color = "black") +
      labs(x = expression(x[1]), y = expression(x[2]), title = "Individual Tree in the Forest") +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
      scale_fill_colorblind(name = "Prediction") +
      scale_color_colorblind(name = "True Class") +
      theme(legend.position = "top") +
      theme(panel.background = element_rect(color = "black", linewidth = 1)) +
      xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
      ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
    
    
    
    rf_plot_1 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,1])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,1]),
                   breaks = 1.5, color = "black")
    rf_plot_2 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,2])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,2]),
                   breaks = 1.5, color = "black")
    rf_plot_3 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,3])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,3]),
                   breaks = 1.5, color = "black")
    rf_plot_4 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,4])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,4]),
                   breaks = 1.5, color = "black")
    rf_plot_5 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,5])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,5]),
                   breaks = 1.5, color = "black")
    rf_plot_6 <- rf_plot +
      geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,6])), alpha = 0.3) +
      geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,6]),
                   breaks = 1.5, color = "black")
    
    ggarrange(rf_plot_1, rf_plot_2, rf_plot_3,  
              rf_plot_4, rf_plot_5, rf_plot_6, 
              nrow = 2, ncol = 3, common.legend = TRUE)
    
    
  })   # COMBINED PLOT TITLE, LABEL NEED TO BE ADJUSTED
  
  ###########################
  # classification second tab - Bias-Variance Table/plots
  # INSTEAD OF WRITING THE BAYES ERROR RATE, MAYBE DISPLAY THE BAYES DECISION BOUNDARY
  # ALTHOUGH IT WOULD TAKE SOME TIME TO GENERATE
  
  
  output$text_logreg <- output$text_knn_class <- output$text_tree_class <- output$text_svm <- renderText({
    
    mean_bayes_error <- df_data_classification()[[7]]
    
    paste0("The Bayes Error Rate is ", round(mean_bayes_error, 3), ". The approximate squared bias and variance values have been calculated based on 'Bias Plus Variance Decomposition for Zero-One Loss Functions' by Kohavi and Wolpert (1996).")
    
  })
  
  
}


###############################################################################################################################
# OLD CODES THAT HAVE BEEN DISCARDED/UPDATED/CHANGED


# # REPLICATED TRAINING DATASETS
# df_training_mat <- rbind(training_obs_blue, training_obs_orange)
# df_training_probs <- matrix(nrow = nrow(df_training_mat), ncol = 2)
# for(i in 1:nrow(df_training_mat))
# {
#   obs <- df_training_mat[i,]
#   d_blue <- numeric(10); d_orange <- numeric(10)
#
#   for(k in 1:10)
#   {
#     d_blue[k] <- dmvnorm(x = obs, mean = means_blue[k,],
#                          sigma = matrix(c(noise_sd,0,0,noise_sd), nrow=2, ncol=2))
#     d_orange[k] <- dmvnorm(x = obs, mean = means_orange[k,],
#                            sigma = matrix(c(noise_sd,0,0,noise_sd), nrow=2, ncol=2))
#   }
#
#   s_d <- mean(d_blue)+mean(d_orange)
#   df_training_probs[i,] <- c(mean(d_blue)/s_d, mean(d_orange)/s_d)
# }
#
# x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
# x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
# y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
#
# for(j in 1:num_rep_sets)
# {
#   y_rep_values <- numeric(num_training_points)
#   for(i in 1:num_training_points)
#   {
#     y_rep_values[i] <- sample(0:1, 1, prob = df_training_probs[i,])
#   }
#   y_matrix[, j] <- y_rep_values
#   x1_matrix[, j] <- df_training_mat[,1]; x2_matrix[, j] <- df_training_mat[,2]
#
# }
#




# output$table_logreg_p2 <- DT::renderDataTable({
#   
#   results <- df_bvt_calc_logreg()[[1]]
#   
#   DT::datatable(results)
#   
# })


# output$plot_knn_class_p2a <- renderPlot({
#   
#   results <- df_bvt_calc_knn_class()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = bias_sq_kw), fill = cb_pallete[8]) + theme_bw() +
#     labs(y = "Approximate Squared Bias", x = "K")
#   
#   
# })
# 
# output$plot_knn_class_p2b <- renderPlot({
#   
#   results <- df_bvt_calc_knn_class()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = var_kw), fill = cb_pallete[4]) + theme_bw() + 
#     labs(y = "Approximate Variance", x = "K")
#   
#   
# })
# 
# output$plot_knn_class_p2c <- renderPlot({
#   
#   results <- df_bvt_calc_knn_class()[[1]]
#   
#   # DT::datatable(results)
#   
#   results_error <- results %>% 
#     pivot_longer(cols = 2:3, names_to = "Error", values_to = "error")
#   
#   error_plot <- ggplot(data = results_error) +
#     geom_point(aes(x = parameter_values, y = error, color = Error)) +
#     geom_line(aes(x = parameter_values, y = error, group = Error, color = Error)) +
#     scale_color_colorblind(labels = c("Test Error", "Training Error")) + theme_bw() +
#     # SEE KW, 1996 FOR NOISE CALCULATION
#     theme(legend.position = "top", legend.title = element_blank()) +
#     labs(y = "Misclassification Error Rate", color = "Error", x = "K")
#   
#   error_plot
#   
# })




# output$plot_tree_class_p2a <- renderPlot({
#   
#   results <- df_bvt_calc_tree_class()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = bias_sq_kw), fill = cb_pallete[8]) + theme_bw() +
#     labs(y = "Approximate Squared Bias", x = "Tree Depth")
#   
#   
# })
# 
# output$plot_tree_class_p2b <- renderPlot({
#   
#   results <- df_bvt_calc_tree_class()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = var_kw), fill = cb_pallete[4]) + theme_bw() +
#     labs(y = "Approximate Variance", x = "Tree Depth")
#   
#   
# })
# 
# output$plot_tree_class_p2c <- renderPlot({
#   
#   results <- df_bvt_calc_tree_class()[[1]]
#   
#   # DT::datatable(results)
#   
#   results_error <- results %>% 
#     pivot_longer(cols = 2:3, names_to = "Error", values_to = "error")
#   
#   error_plot <- ggplot(data = results_error) +
#     geom_point(aes(x = parameter_values, y = error, color = Error)) +
#     geom_line(aes(x = parameter_values, y = error, group = Error, color = Error)) +
#     scale_color_colorblind(labels = c("Test Error", "Training Error")) + theme_bw() +
#     # SEE KW, 1996 FOR NOISE CALCULATION
#     theme(legend.position = "top", legend.title = element_blank()) +
#     labs(y = "Misclassification Error Rate", color = "Error", x = "Tree Depth")
#   
#   error_plot
#   
# })




# output$plot_svm_p2a <- renderPlot({
#   
#   results <- df_bvt_calc_svm()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = bias_sq_kw), fill = cb_pallete[8]) + theme_bw() +
#     labs(y = "Approximate Squared Bias", x = "C")
#   
#   
# })
# 
# output$plot_svm_p2b <- renderPlot({
#   
#   results <- df_bvt_calc_svm()[[1]]
#   
#   ggplot(data = results) +
#     geom_col(aes(x = parameter_values, y = var_kw), fill = cb_pallete[4]) + theme_bw() +
#     labs(y = "Approximate Variance", x = "C")
#   
#   
# })
# 
# output$plot_svm_p2c <- renderPlot({
#   
#   results <- df_bvt_calc_svm()[[1]]
#   
#   # DT::datatable(results)
#   
#   results_error <- results %>% 
#     pivot_longer(cols = 2:3, names_to = "Error", values_to = "error")
#   
#   error_plot <- ggplot(data = results_error) +
#     geom_point(aes(x = parameter_values, y = error, color = Error)) +
#     geom_line(aes(x = parameter_values, y = error, group = Error, color = Error)) +
#     scale_color_colorblind(labels = c("Test Error", "Training Error")) + theme_bw() +
#     # SEE KW, 1996 FOR NOISE CALCULATION
#     theme(legend.position = "top", legend.title = element_blank()) +
#     labs(y = "Misclassification Error Rate", color = "Error", x = "C")
#   
#   error_plot
#   
# })





# rf_plot_1 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,1])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,1]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
# rf_plot_2 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,2])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,2]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
# rf_plot_3 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,3])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,3]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
# rf_plot_4 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,4])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,4]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
# rf_plot_5 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,5])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,5]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))
# rf_plot_6 <- ggplot(data = alldf)+
#   geom_tile(mapping = aes(x=xeg, y=yeg, fill = as.factor(preds_rf$predictions[,6])), alpha = 0.3) + theme_minimal() +
#   # geom_point(data = df_training, mapping = aes(x=x1, y=x2, color = y_orig), size = 2) +
#   geom_contour(aes(x = xeg, y = yeg, z = preds_rf$predictions[,6]),
#                breaks = 1.5, color = "black") +
#   labs(x = expression(x[1]), y = expression(x[2]), title = "Random Forest Decision Boundary") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
#   scale_fill_colorblind(name = "Prediction") +
#   scale_color_colorblind(name = "True Class") +
#   theme(legend.position = "top") +
#   theme(panel.background = element_rect(color = "black", linewidth = 1)) +
#   xlim(c(round(min(df_training$x1),2) - 0.3, round(max(df_training$x1),2) + 0.3)) + 
#   ylim(c(round(min(df_training$x2),2) - 0.3, round(max(df_training$x2),2) + 0.3))





###############################################################################################################################



