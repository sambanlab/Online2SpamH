utils::globalVariables(c(
  "label", "label_old", "confidence",
  "phone_usage", "activity_level", "all_of"
))

#' TwoSpamH
#' 
#' @param data data frame to run 2SpamH on
#' @param passive_variable name of variable to be marked as missing or non-missing
#' @param phone_usage_vars vector of strings of phone usage variable names from data
#' @param activity_level_vars vector of strings of activity level variable names from data
#' @param thresholds data frame with thresholds for activity level and phone usage
#' @param num.neighbor neighbors for KNN algorithm
#' @param check.cor change to correlation value (max correlation of phone usage and activity level vectors for knn training) 
#' @param plot.data whether to plot the data
#' @param seed set seed
#'
#' @return original dataset with column 'label' with "Missing"/"Non-missing" values
#'
#' @description
#' A function to run 2SpamH algorithm and identify all observations in data frame as "Missing" or "Non-missing"
#' 
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile
#' @import ggplot2
#' @export
#' 
#' @examples
#' # Load example data
#' data("example_data")
#'
#' # Define variables
#' passive_variable <- "step_count"
#' activity_level_vars <- "n_uploads"
#' phone_usage_vars <- c("screen_unlocks", "display_events")
#' all_vars <- c(passive_variable, activity_level_vars, phone_usage_vars)
#'
#' # Run TwoSpamH with example thresholds
#' thresholds <- data.frame(
#'   lower_bound_phone_usage = 0.2,
#'   upper_bound_phone_usage = 0.8,
#'   lower_bound_activity_level = 0.3,
#'   upper_bound_activity_level = 0.7
#' )
#'
#' TwoSpamH(
#'   data = example_data,
#'   passive_variable = passive_variable,
#'   phone_usage_vars = phone_usage_vars,
#'   activity_level_vars = activity_level_vars,
#'   thresholds = thresholds,
#'   plot.data = TRUE
#' )
#' 
TwoSpamH <- function(data, 
                     passive_variable,  
                     phone_usage_vars, 
                     activity_level_vars, 
                     thresholds = data.frame( 
                       lower_bound_phone_usage = 0.3, 
                       upper_bound_phone_usage = 0.7, 
                       lower_bound_activity_level = 0.3,
                       upper_bound_activity_level = 0.7),
                     num.neighbor = 5,  
                     check.cor = NULL,
                     plot.data = F, 
                     seed = NULL
){
  
  set.seed(seed)
  
  ## thresholds is a numeric data frame with 4 values: lower_bound_phone_usage, upper_bound_phone_usage, lower_bound_activity_level, upper_bound_activity_level
  ## the values are upper and lower quantiles of phone usage and activity level
  if(!(all(sapply(thresholds, is.numeric)) && all(thresholds >= 0 & thresholds <= 1) && nrow(thresholds) == 1 )){
    stop('incorrect thresholds format')
  }
  ## if there are only 2 values in thresholds, assume lower and upper bounds for phone usage and activity level are the same
  if(ncol(thresholds) == 2){
    thresholds <- data.frame(lower_bound_phone_usage = thresholds[1,1], upper_bound_phone_usage = thresholds[1,2], lower_bound_activity_level = thresholds[1,1], upper_bound_activity_level = thresholds[1,1])
  } else if (ncol(thresholds) == 4){
    colnames(thresholds) <- c("lower_bound_phone_usage", "upper_bound_phone_usage", "lower_bound_activity_level", "upper_bound_activity_level")
  } else {
    stop('incorrect thresholds format')
  }
  
  ## only one passive variable
  if(length(passive_variable) != 1 | !is.character(passive_variable)){
    stop('incorrect passive variable format')
  }
  
  ## num.neighbor for KNN algorithm must be integer greater than 1
  if (!(is.numeric(num.neighbor) && (num.neighbor%%1==0) && (num.neighbor > 1))) stop('num.neighbor is not a positive integer')
  ## correlation must be numeric
  if (!(is.numeric(check.cor) | is.null(check.cor))) stop('check.cor is not numeric or NULL')
  
  # if we want to eliminate highly correlated values from phone_usage_vars or activity_level_vars
  if(is.null(check.cor)){
    delete.cor = F
  }else{
    delete.cor = T
    cor.thresh = check.cor
  }
  
  ## input variables must be strings
  if(!all(sapply(list(passive_variable, phone_usage_vars, activity_level_vars), is.character))){
    stop('passive_variable, phone_usage_vars, activity_level_vars must be character vectors')
  }
  
  ## save the original data
  og.data <- data
  
  ## dealing with missing data among variables of interest
  all_vars <- c(phone_usage_vars, activity_level_vars, passive_variable)
  data <- data %>% 
    dplyr::select(all_of(all_vars)) %>%
    ## filter(if_all(all_of(all_vars), ~ !is.na(.))) ## Option 1: filter out observations with NA values
    dplyr::mutate_if(is.numeric, imputeTS::na_mean) ## Option 2: replace NA values with mean
  
  ## PCA + scaling of phone usage variables (if there is just one variable, it is just scaling)
  phone_usage <- data %>% dplyr::select(all_of(phone_usage_vars))
  phone_usage <- princomp(phone_usage %>% scale(), cor = F)$scores[, 1]
  
  ## PCA + scaling of activity level variables (if there is just one variable, it is just scaling)
  activity_level <- data %>% dplyr::select(all_of(activity_level_vars)) 
  activity_level <- princomp(activity_level %>% scale(), cor = F)$scores[, 1]
  
  ## identify observations below lower thresholds for both phone usage and activity level
  low_phone_usage <- phone_usage <= quantile(phone_usage, thresholds$lower_bound_phone_usage)
  low_activity_level <- activity_level <= quantile(activity_level, thresholds$lower_bound_activity_level)
  low <- low_phone_usage & low_activity_level
  
  ## identify observations above upper thresholds for both phone usage and activity level
  high_phone_usage <- phone_usage >= quantile(phone_usage, thresholds$upper_bound_phone_usage)
  high_activity_level <- activity_level >= quantile(activity_level, thresholds$upper_bound_activity_level)
  high <- high_phone_usage & high_activity_level
  
  # data with only passive variable to be labeled, phone usage, activity level vectors and label after threshold filtering
  data <- data %>%
    dplyr::mutate(label_old = dplyr::case_when( low ~ "Missing",
                                  high ~ "Non Missing",
                                  TRUE ~ NA)) %>% ## label points if they are below or above thresholds (points in blue and red zones)
    dplyr::mutate_if(function(x){is.numeric(x) & length(unique(x)) != 1}, scale) ## scale the numeric columns for knn training
  
  
  
  if(delete.cor){ ## if we want to check for high correlations
    ## if there is a high correlation between phone usage and activity level, delete one of them
    if(cor(phone_usage, activity_level) > cor.thresh){
      data.training <- data %>% dplyr::select(all_of(phone_usage_vars)) %>% dplyr::filter(!is.na(label_old)) 
    } else {
      ## if columns are not highly correlated, only prepare training data for knn with only initially labeled observations
      data.training <- data %>% dplyr::filter(!is.na(label_old))
    }
  } else { ## if we do not check for high correlation
    ## prepare training data for knn with only initially labeled observations
    data.training <- data %>% dplyr::filter(!is.na(label_old)) 
  }
  
  ## prepare training data with prototype labels for KNN
  clusters <- data.training$label_old
  data.training <- data.training %>% dplyr::select(all_of(c(phone_usage_vars, activity_level_vars)))
  
  
  if(nrow(data.training) == 0){
    print("2SpamH failed. Choose different thresholds")
    return(NULL)
  }
  
  
  knn <- suppressWarnings(knn(
    train = data.training, ## train on already labeled points 
    test = dplyr::select(data,all_of(names(data.training))),
    cl = clusters,
    k = num.neighbor
  ))
  
  ## if old label (label after thresholding) was NA, assign it a new label from KNN
  data <- data %>%
    dplyr::mutate(label = ifelse(
      !is.na(label_old),
      label_old,
      as.character(knn))
    ) 
  
  og.data <- dplyr::mutate(og.data, label = data$label)
  
  if(plot.data){
    pass_var <- (og.data %>% dplyr::select(all_of(passive_variable))) %>% pull(1) %>% as.numeric()
    #return(pass_var)
    lb_pu <- quantile(phone_usage, thresholds$lower_bound_phone_usage)[[1]]
    ub_pu <- quantile(phone_usage, thresholds$upper_bound_phone_usage)[[1]]
    lb_al <- quantile(activity_level, thresholds$lower_bound_activity_level)[[1]]
    ub_al <- quantile(activity_level, thresholds$upper_bound_activity_level)[[1]]
    plot <- og.data %>%
      ggplot(aes(x = phone_usage, y = activity_level, size = pass_var, colour = label)) +
      labs(x = "Phone Usage", y = "Activity Level", size = gsub("_"," ",passive_variable), colour = "2SpamH Label") +
      geom_rect(aes(xmin = -Inf, xmax = lb_pu, ymin = -Inf, ymax = lb_al), fill = "red", alpha = 0.01, color = NA) +
      geom_rect(aes(xmin = ub_pu, xmax = Inf, ymin = ub_al, ymax = Inf), fill = "lightblue", alpha = 0.05, color = NA) +
      geom_point(alpha = 0.5) +
      scale_colour_manual(values = c("Non Missing" = "blue", "Missing" = "red4")) +
      theme_minimal() +
      theme(
        #legend.position = "none",
        axis.line = element_blank(),  # hide axis-only lines
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # add full box outline
      )
    print(plot)
  }
  
  return(og.data)
  
}


#' TwoSpamH_train
#'
#' @param data data frame to run 2SpamH on
#' @param passive_variable name of variable to be marked as missing or non-missing
#' @param phone_usage_vars vector of strings of phone usage variable names from data
#' @param activity_level_vars vector of strings of activity level variable names from data
#' @param num.neighbor neighbors for KNN algorithm
#' @param seed set seed
#' @param plot.data whether to plot data
#' @param conf if plot.data = TRUE, whether to plot the confidence of point's assignment
#'
#' @returns original dataset with column 'label' with "Missing"/"Non-missing" values
#' 
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile
#' @import ggplot2
#' @export
#' 
#' @description
#' A function to train a dataset by assigning 2SpamH labels to all observations using a Majority Vote method
#' 
#'
#' @examples
#' # Load example data
#' data("example_data")
#'
#' # Define variable names
#' passive_variable <- "step_count"
#' activity_level_vars <- "n_uploads"
#' phone_usage_vars <- c("screen_unlocks", "display_events")
#' all_vars <- c(passive_variable, activity_level_vars, phone_usage_vars)
#'
#' # Training function
#' TwoSpamH_train(
#'   data = example_data,
#'   passive_variable = passive_variable,
#'   phone_usage_vars = phone_usage_vars,
#'   activity_level_vars = activity_level_vars,
#'   plot.data = TRUE
#' )
#' 
TwoSpamH_train <- function(data, 
                           passive_variable,  
                           phone_usage_vars, 
                           activity_level_vars, 
                           num.neighbor = 5, 
                           seed = NULL,
                           plot.data = F,
                           conf = F
){
  
  set.seed(seed)
  
  ## check for correct inputs
  ## only one passive variable
  if(length(passive_variable) != 1 | !is.character(passive_variable)){
    stop('incorrect passive variable format')
  }
  ## num.neighbor for KNN algorithm must be integer greater than 1
  if (!(is.numeric(num.neighbor) && (num.neighbor%%1==0) && (num.neighbor > 1))) stop('num.neighbor is not a positive integer')
  ## input variables must be strings
  if(!all(sapply(list(passive_variable, phone_usage_vars, activity_level_vars), is.character))){
    stop('passive_variable, phone_usage_vars, activity_level_vars must be character vectors')
  }
  
  ## number of iterations across all thresholds
  iter = 0
  
  ## vector to count how many times an observation was classified as 'missing'
  vec <- rep(0, nrow(data))
  
  ## iterate across all combinations of thresholds
  for (i in seq(0.1, 0.9, by = 0.1)){ ## lb_pu
    for (j in seq(0.1, 0.9, by = 0.1)){ ## ub_pu
      for (k in seq(0.1, 0.9, by = 0.1)){ ## lb_al
        for (h in seq(0.1, 0.9, by = 0.1)){ ## ub_al
          
          ## the selection regions overlap if:
          ## lower bound phone usage > upper bound phone usage AND lower bound activity level > upper bound activity level
          ## OR lower bound phone usage == upper bound phone usage OR lower bound activity level == upper bound activity level
          if((i > j & k > h) | i == j | k == h){
            next
          }
          
          ## thresholds for a given loop combination
          t = data.frame( ## data frame with thresholds for activity level and phone usage
            lower_bound_phone_usage = i, ## default thresholds
            upper_bound_phone_usage = j, 
            lower_bound_activity_level = k,
            upper_bound_activity_level = h)
          
          ## run 2SpamH with the given threshold combination
          l <- TwoSpamH(
            data = data,
            passive_variable = passive_variable,
            phone_usage_vars = phone_usage_vars,
            activity_level_vars = activity_level_vars,
            num.neighbor = num.neighbor,
            thresholds = t)
          
          if(is.null(l)) next
          
          ## count iterations
          iter = iter + 1
          
          ## update the count of 'Missing' labels 
          vec <- vec + (l$label == "Missing")
          
        }}}}
  
  ## percentage of how many times observation classified as 'Missing' for all threshold combinations
  vec <- vec / iter
  
  ## if over half of the threshold combos classified a point as "Missing", it is set to "Missing"
  data$label <- ifelse(vec > 0.5, "Missing", "Non Missing")
  
  
  if(plot.data){
    pass_var <- (data %>% dplyr::select(all_of(passive_variable))) %>% pull(1) %>% as.numeric()
    phone_usage <- data %>% dplyr::select(all_of(phone_usage_vars))
    phone_usage <- princomp(phone_usage %>% scale(), cor = F)$scores[, 1]
    activity_level <- data %>% dplyr::select(all_of(activity_level_vars)) 
    activity_level <- princomp(activity_level %>% scale(), cor = F)$scores[, 1]
    if(!conf){
      plot <- data %>%
        ggplot(aes(x = phone_usage, y = activity_level, size = pass_var, colour = label)) +
        labs(x = "Phone Usage", y = "Activity Level", size = gsub("_"," ",passive_variable), colour = "2SpamH Label") +
        geom_point(alpha = 0.5) +
        scale_colour_manual(values = c("Non Missing" = "blue", "Missing" = "red4")) +
        theme_minimal() +
        theme(
          #legend.position = "none",
          axis.line = element_blank(),  # hide axis-only lines
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # add full box outline
        )
    } else {
      data$confidence <- abs(vec-0.5) /.5
      plot <- data %>%
        ggplot(aes(x = phone_usage, y = activity_level, size = pass_var, colour = label)) +
        labs(x = "Phone Usage", y = "Activity Level", size = gsub("_"," ",passive_variable), colour = "2SpamH Label") +
        geom_point(aes(alpha = confidence)) +
        scale_colour_manual(values = c("Non Missing" = "blue", "Missing" = "red4")) +
        theme_minimal() +
        theme(
          #legend.position = "none",
          axis.line = element_blank(),  # hide axis-only lines
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # add full box outline
        )
    }
    print(plot)
  }
  
  return(data)
}


#' Online_TwoSpamH
#'
#' @param new_data new observation to be labeled as "Missing" or "Non-missing" based on the training data
#' @param training_data training dataset after TwoSpamH_train() function
#' @param passive_variable name of variable to be marked as missing or non-missing
#' @param phone_usage_vars vector of strings of phone usage variable names from data
#' @param activity_level_vars vector of strings of activity level variable names from data
#' @param num.neighbor neighbors for KNN algorithm
#' @param seed set seed
#' @param plot.data whether to plot a data with new observation
#'
#' @returns new_data observation with "Missing"/"Non-missing" label assignment
#' 
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile
#' @import ggplot2
#' @export
#' 
#' @description
#' Label a new observation as "Missing" or "Non missing" using a training set
#' 
#' @examples
#' # Load example data
#' data("example_data")
#'
#' # Define variable names
#' passive_variable <- "step_count"
#' activity_level_vars <- "n_uploads"
#' phone_usage_vars <- c("screen_unlocks", "display_events")
#'
#' # Train the model
#' train <- TwoSpamH_train(
#'   data = example_data,
#'   passive_variable = passive_variable,
#'   phone_usage_vars = phone_usage_vars,
#'   activity_level_vars = activity_level_vars,
#'   plot.data = FALSE
#' )
#'
#' # Create a new observation
#' new_data <- data.frame(
#'   step_count = 3400,
#'   n_uploads = 90,
#'   screen_unlocks = 25,
#'   display_events = 130
#' )
#'
#' # Predict using the trained model
#' Online_TwoSpamH(
#'   new_data = new_data,
#'   training_data = train,
#'   passive_variable = passive_variable,
#'   phone_usage_vars = phone_usage_vars,
#'   activity_level_vars = activity_level_vars,
#'   plot.data = TRUE
#' )
Online_TwoSpamH <- function(new_data, 
                            training_data, 
                            passive_variable,  
                            phone_usage_vars, 
                            activity_level_vars, 
                            num.neighbor = 5, 
                            seed = NULL,
                            plot.data = F
){
  
  set.seed(seed)
  
  ## check for correct inputs
  ## only one passive variable
  if(length(passive_variable) != 1 | !is.character(passive_variable)){
    stop('incorrect passive variable format')
  }
  ## num.neighbor for KNN algorithm must be integer greater than 1
  if (!(is.numeric(num.neighbor) && (num.neighbor%%1==0) && (num.neighbor > 1))) stop('num.neighbor is not a positive integer')
  ## input variables must be strings
  if(!all(sapply(list(passive_variable, phone_usage_vars, activity_level_vars), is.character))){
    stop('passive_variable, phone_usage_vars, activity_level_vars must be character vectors')
  }
  ## new observation must have the same column names as training dataframe (except for label column)
  if(any(sort(colnames(new_data)) != sort(colnames(dplyr::select(training_data, -label)))) == T){
    stop('new observation has different column names than training dataframe')
  }
  
  ## PCA + scaling of phone usage and activity level variables in train dataset
  phone_usage_train <- training_data %>% dplyr::select(all_of(phone_usage_vars)) 
  phone_usage_new <- new_data %>% dplyr::select(all_of(phone_usage_vars))
  phone_usage_new <- princomp(rbind(phone_usage_train, phone_usage_new) %>% scale(), cor = F)$scores[, 1]
  phone_usage_new <- phone_usage_new[(nrow(phone_usage_train)+1):length(phone_usage_new)] ## just one value
  phone_usage_train <- princomp(phone_usage_train %>% scale(), cor = F)$scores[, 1]
  
  activity_level_train <- training_data %>% dplyr::select(all_of(activity_level_vars)) 
  activity_level_new <- new_data %>% dplyr::select(all_of(activity_level_vars))
  activity_level_new <- princomp(rbind(activity_level_train, activity_level_new) %>% scale(), cor = F)$scores[, 1]
  activity_level_new <- activity_level_new[(nrow(activity_level_train)+1):length(activity_level_new)] ## just one value
  activity_level_train <- princomp(activity_level_train %>% scale(), cor = F)$scores[, 1]
  
  train <- data.frame(phone_usage = phone_usage_train, activity_level = activity_level_train)
  test <- data.frame(phone_usage = phone_usage_new, activity_level = activity_level_new)
  
  test$label <- suppressWarnings(
    knn(
      train = train,
      test = test,
      cl = training_data$label,
      k = num.neighbor
    )
  ) %>% as.character()
  
  if(plot.data){
    pass_var <- c(((training_data %>% dplyr::select(all_of(passive_variable))) %>% pull(1) %>% as.numeric()), ((new_data %>% dplyr::select(all_of(passive_variable)))%>% pull(1) %>% as.numeric()))
    plot_data <- train %>%
      mutate(label = training_data$label) %>%
      rbind(test)
    plot <-  plot_data %>%
      ggplot(aes(x = phone_usage, y = activity_level, size = pass_var, colour = label)) +
      labs(x = "Phone Usage", y = "Activity Level", size = gsub("_"," ",passive_variable), colour = "2SpamH Label") +
      geom_point(alpha = 0.5) +
      scale_colour_manual(values = c("Non Missing" = "blue", "Missing" = "red4")) +
      geom_point(data = plot_data[(nrow(train)+1):nrow(plot_data), ],
                 shape = 21, colour = "black", size = 8, stroke = 1.2) +
      theme_minimal() +
      theme(
        #legend.position = "none",
        axis.line = element_blank(),  # hide axis-only lines
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # add full box outline
      )
    print(plot)
  }
  
  return(mutate(new_data, label = test$label))
}