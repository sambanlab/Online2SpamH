utils::globalVariables(c(
  "label", "label_old", "confidence",
  "phone_usage", "activity_level", "all_of"
))

#' TwoSpamH
#'
#' @param data A data frame containing the variable to be labelled and the variables representing device usage and sensory activity level.
#' @param passive_variable A passive variable name to be labeled as missing or non-missing.
#' @param phone_usage_vars A vector of strings of phone usage variable names from data.
#' @param activity_level_vars A vector of strings of activity level variable names from data.
#' @param thresholds A data frame with lower and upper thresholds for device usage and sensor activity level (look at examples for formatting).
#' @param num.neighbor A number of neighbors considered by each initially unlabeled data point in KNN algorithm.
#' @param check.cor Whether to remove one of the device usage or sensor activity vectors if they are highly correlated. If no, input should be NULL (default). If yes, input should be the correlation threshold for variables to be removed.
#' @param plot.data Whether to plot the data, default is FALSE.
#' @param seed The seed to be set, default is NULL.
#'
#' @return The original input data frame with an additional column 'label' representing observation's "Missing"/"Non missing" status.
#'
#' @description
#' A function to run 2SpamH algorithm that assigns all observations in data frame a "Missing" or "Non-missing" label using a pre-defined set of lower and upper thresholds.
#'
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile as.formula glm predict
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

  ## input variables must be strings representing variable names in data frame
  if(!all(sapply(list(passive_variable, phone_usage_vars, activity_level_vars), is.character))){
    stop('passive_variable, phone_usage_vars, activity_level_vars must be character vectors')
  }

  ## save the original data
  og.data <- data

  ## dealing with missing data among variables of interest
  all_vars <- c(phone_usage_vars, activity_level_vars, passive_variable)
  data <- data %>%
    dplyr::select(all_of(all_vars)) %>% ## choose only variables of interest
    dplyr::mutate_if(is.numeric, imputeTS::na_mean) ## replace NA values with mean

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

  # label high confidence points as missing or non-missing
  data <- data %>%
    dplyr::mutate(label_old = dplyr::case_when( low ~ "Missing",
                                  high ~ "Non Missing",
                                  TRUE ~ NA)) %>% ## if not high confidence (not in red or blue areas), leave as NA
    dplyr::mutate_if(function(x){is.numeric(x) & length(unique(x)) != 1}, scale) ## scale the numeric columns for knn training


  ## prepare data for self-supervised training with knn
  if(delete.cor){ ## if we check for high correlation between phone usage and activity level vectors
    if(cor(phone_usage, activity_level) > cor.thresh){ ## if there is a high correlation between phone usage and activity level vectors, use only one of them for knn
      data.training <- data %>% dplyr::select(all_of(phone_usage_vars)) %>% dplyr::filter(!is.na(label_old))
    } else { ## if columns are not highly correlated, prepare training data for knn with only initially labeled observations and both vectors
      data.training <- data %>% dplyr::filter(!is.na(label_old))
    }
  } else { ## if we do not check for high correlation
    ## prepare training data for knn with only initially labeled observations
    data.training <- data %>% dplyr::filter(!is.na(label_old))
  }

  ## prepare training data with prototype labels for KNN
  clusters <- data.training$label_old
  data.training <- data.training %>% dplyr::select(all_of(c(phone_usage_vars, activity_level_vars)))

  ## if no training data was produced (no points initially labeled with high confidence)
  if(nrow(data.training) == 0){
    print("2SpamH failed. Choose different thresholds")
    return(NULL)
  }

  ## use knn with high confidence prototypes as training set
  knn <- suppressWarnings(knn(
    train = data.training,
    test = dplyr::select(data,all_of(names(data.training))),
    cl = clusters,
    k = num.neighbor
  ))

  ## assign new labels to observations initially labeled as NA based on KNN training
  data <- data %>%
    dplyr::mutate(label = ifelse(
      !is.na(label_old),
      label_old,
      as.character(knn))
    )

  ## append a final label to original data
  og.data <- dplyr::mutate(og.data, label = data$label)

  ## if we want to display a plot
  if(plot.data){
    pass_var <- (og.data %>% dplyr::select(all_of(passive_variable))) %>% pull(1) %>% as.numeric()
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
        axis.line = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
      )
    print(plot)
  }

  ## return original data with new column 'label' with "Missing"/"Non-missing" label
  return(og.data)

}


#' TwoSpamH_train
#'
#' @param data A data frame containing the variable to be labelled and the variables representing device usage and sensory activity level.
#' @param passive_variable A passive variable name to be labeled as missing or non-missing.
#' @param phone_usage_vars A vector of strings of phone usage variable names from data.
#' @param activity_level_vars A vector of strings of activity level variable names from data.
#' @param method Which Online2SpamH method to use. Choose from "majority_vote" (default) or "optimal_thresholds".
#' @param num.neighbor A number of neighbors considered by each initially unlabeled data point in KNN algorithm.
#' @param seed The seed to be set, default is NULL.
#' @param plot.data Whether to plot the data, default is FALSE.
#' @param conf Whether to calculate confidence of label assignment, append it to original data frame, default is FALSE.
#'
#' @returns The original input data frame with an additional column 'label' representing observation's "Missing"/"Non missing" status.
#'
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile
#' @import ggplot2
#' @export
#'
#' @description
#' A function to train the model by assigning "Missing"/"Non missing" labels to all available observations. By default, it uses a Majority Vote method - check manuscript for details.
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
#'   method = "majority_vote",
#'   plot.data = TRUE
#' )
#'
TwoSpamH_train <- function(data,
                           passive_variable,
                           phone_usage_vars,
                           activity_level_vars,
                           method = "majority_vote",
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
  ## method must be "majority_vote" or "optimal_thresholds"
  if (method != "majority_vote" & method != "optimal_thresholds") {
    stop(
      '"method" parameter must be either "majority_vote" or "optimal_thresholds". refer to README and manuscript for details'
    )
  }

  ## using majority vote method
  if (method == "majority_vote") {

    ## track number of iterations across all thresholds
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

    ## if conf set to true, append a column with label confidence
    if(conf){
      data$confidence <- abs(vec-0.5) /.5
    }

    if(plot.data){
      ## transform variables for plotting
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

  ## using optimal thresholds method
  if (method == "optimal_thresholds") {

    ## define helper functions
    ## min-max normalization
    scaling <- function(d){
      return((d-min(d))/(max(d)-min(d)))
    }
    ## change the format from XX% to .XX
    convert_to_decimal <- function(vec) {
      return(as.numeric(sub("%", "", names(vec))) / 100)
    }

    ## prepare dataset with all variables
    all_vars <- c(phone_usage_vars, activity_level_vars, passive_variable)
    data <- data %>%
      dplyr::select(all_of(all_vars)) %>%
      mutate_if(is.numeric, na_mean) ## replace NA values with mean

    ## PCA + scaling of phone usage variables (if there is just one variable, it is just scaling)
    phone_usage <- data %>% dplyr::select(all_of(phone_usage_vars))
    phone_usage <- princomp(phone_usage %>% scale(), cor = F)$scores[, 1]
    ## min-max normalization to have all values above 0
    phone_usage <- scaling(phone_usage)

    ## PCA + scaling of activity level variables (if there is just one variable, it is just scaling)
    activity_level <- data %>% dplyr::select(all_of(activity_level_vars))
    activity_level <- princomp(activity_level %>% scale(), cor = F)$scores[, 1]
    ## min-max normalization to have all values above 0
    activity_level <- scaling(activity_level)

    for (i in seq_along(passive_variable)) {
      var_name <- paste0("passive_var", i)
      d <- data[[passive_variable[i]]] # no need to scale these
      assign(var_name, d)
    }

    ## variables ready
    ## separate bounds for Phone Usage and Activity Level
    ## use every 10th quantile to determine thresholds

    lb_pu <- quantile(phone_usage, probs = seq(0.1, 0.9, by = 0.1)) # lower bound phone usage
    ub_pu <- quantile(phone_usage, probs = seq(0.1, 0.9, by = 0.1)) # upper bound phone usage
    lb_al <- quantile(activity_level, probs = seq(0.1, 0.9, by = 0.1)) # lower bound activity level
    ub_al <- quantile(activity_level, probs = seq(0.1, 0.9, by = 0.1)) # upper bound activity level

    L <- data.frame(
      lower_bound_phone_usage = numeric(),
      upper_bound_phone_usage = numeric(),
      lower_bound_activity_level = numeric(),
      upper_bound_activity_level = numeric(),
      supervised_loss = numeric(),
      unsupervised_loss = numeric(),
      loss = numeric()
    )

    for (i in 1:length(lb_pu)) { ## lower bound phone usage
      for (j in 1:length(ub_pu)) { ## upper bound phone usage
        for (k in 1:length(lb_al)) { ## lower bound activity level
          for (h in 1:length(ub_al)) { ## upper bound activity level

            ## lb_pu = ub_pu, therefore we can compare indices only
            ## the selection regions overlap if:
            ## lower bound phone usage > upper bound phone usage AND lower bound activity level > upper bound activity level
            ## OR lower bound phone usage == upper bound phone usage OR lower bound activity level == upper bound activity level
            if ((i > j & k > h) | i == j | k == h) {
              next
            }

            n = nrow(data)
            prototype_label <- vector("numeric", n) ## vector for prototype labels

            for (m in 1:n) {
              if (phone_usage[m] < lb_pu[i] & activity_level[m] < lb_al[k]) {
                prototype_label[m] <- 0 # label as missing if both PU and AL below a lower threshold (point in red zone)

              } else if (phone_usage[m] > ub_pu[j] & activity_level[m] > ub_al[h]) {
                prototype_label[m] <- 1 # label as not missing if both PU and AL above an upper threshold (point in blue zone)

              } else{
                prototype_label[m] <- NA ## label as NA if there is no quantile-based prototype threshold
              }
            }

            ylab <- prototype_label[!is.na(prototype_label)] # only observations with assigned prototype labels

            ## ylab = 0 means the observation is missing
            ## probability of missing = quantile of lower bound pu * quantile of lower bound al (they are independent)
            prob_0 = convert_to_decimal(lb_pu[i]) * convert_to_decimal(lb_al[k])
            ## probability of non-missing = (1 - quantile of upper bound pu) * (1 - quantile of upper bound al)
            prob_1 = (1 - convert_to_decimal(ub_pu[j])) * (1 - convert_to_decimal(ub_al[h]))

            ## calculate loss using binary cross entropy
            ## this is the loss for assigning the prototype labels
            supervised_loss <- -sum(ylab * log(prob_1) + (1 - ylab) * log(prob_0))

            ## create a new table with the prototype label (1, 0 or NA) and corresponding passive variables

            t <- as.data.frame(do.call(cbind, c(list(
              prototype_label
            ))))
            t <- cbind(t,
                       dplyr::select(data, all_of(phone_usage_vars)),
                       dplyr::select(data, all_of(activity_level_vars)))
            colnames(t) <- c("prototype_label",
                             phone_usage_vars,
                             activity_level_vars)

            ## fit the model on prototype labels with passive vars as predictors
            ## start with logistic regression, more advanced later

            ## at least 5 observations in each group for logistic regression training
            if (sum(t$prototype_label == 1, na.rm = TRUE) > 5 &
                sum(t$prototype_label == 0, na.rm = TRUE) > 5) {
              ## train the model on observations with prototype labels
              filtered_t <- filter(t, !is.na(prototype_label))

              formula <- as.formula(paste("prototype_label ~", paste(
                c(phone_usage_vars, activity_level_vars),
                collapse = " + "
              )))


              model <- suppressWarnings(glm(formula, data = filtered_t, family = "binomial"))


              ## observations with no prototype label
              missing_prototype_labels <- filter(t, is.na(prototype_label))

              ## probabilities of response for the observations with no prototype label
              predicted_probs <- suppressWarnings(predict(model, missing_prototype_labels, type = "response"))

              ## binary entropy for binary classification with unknown ground truth
              unsupervised_loss <- function(p) {
                eps <- 1e-15 ## add small error to avoid log(0)-p * log(p + eps) - (1 - p) * log(1 - p + eps)
              }
              unsupervised_loss <- sum(unsupervised_loss(predicted_probs))

            } else {
              next ## if we can't train the self-supervised step, drop the thresholds
            }


            ## relative importance of the unsupervised loss
            hyperparameter = 0.1

            L <- rbind(
              L,
              data.frame(
                lower_bound_phone_usage = names(lb_pu[i]),
                upper_bound_phone_usage = names(ub_pu[j]),
                lower_bound_activity_level = names(lb_al[k]),
                upper_bound_activity_level = names(ub_al[h]),
                supervised_loss = supervised_loss,
                unsupervised_loss = unsupervised_loss,
                loss = supervised_loss + hyperparameter * unsupervised_loss
              )
            )

          }
        }
      }
    }

    ## find the thresholds that minimize the loss
    lower_bound_phone_usage <- L[which(L$loss == min(L$loss)), ]$lower_bound_phone_usage[1]
    upper_bound_phone_usage <- L[which(L$loss == min(L$loss)), ]$upper_bound_phone_usage[1]
    lower_bound_activity_level <- L[which(L$loss == min(L$loss)), ]$lower_bound_activity_level[1]
    upper_bound_activity_level <- L[which(L$loss == min(L$loss)), ]$upper_bound_activity_level[1]


    t <- data.frame(
      lower_bound_phone_usage = as.numeric(sub("%", "", lower_bound_phone_usage)) / 100,
      upper_bound_phone_usage = as.numeric(sub("%", "", upper_bound_phone_usage)) / 100,
      lower_bound_activity_level = as.numeric(sub("%", "", lower_bound_activity_level)) / 100,
      upper_bound_activity_level = as.numeric(sub("%", "", upper_bound_activity_level)) / 100
    )

    print("Optimal Thresholds:")
    print(t)

    return(
      TwoSpamH(
        data = data,
        passive_variable = passive_variable,
        phone_usage_vars = phone_usage_vars,
        activity_level_vars = activity_level_vars,
        num.neighbor = num.neighbor,
        thresholds = t,
        plot.data = plot.data))
  }
}


#' Online_TwoSpamH
#'
#' @param new_data A data frame with a single new observation to be labeled as "Missing" or "Non-missing" based on the data trained with 'TwoSpamH_train()' function.
#' @param training_data A data frame processed by a 'TwoSpamH_train()' function.
#' @param passive_variable A passive variable name to be labeled as missing or non-missing.
#' @param phone_usage_vars A vector of strings of phone usage variable names from data.
#' @param activity_level_vars A vector of strings of activity level variable names from data.
#' @param num.neighbor A number of neighbors considered by each initially unlabeled data point in KNN algorithm.
#' @param seed The seed to be set, default is NULL.
#' @param plot.data Whether to plot the data with labeled and highlighted new observation, default is FALSE.
#'
#' @returns A new_data data frame with "Missing"/"Non-missing" label assignment.
#'
#' @importFrom dplyr select mutate mutate_if %>% case_when filter pull
#' @importFrom imputeTS na_mean
#' @importFrom class knn
#' @importFrom stats cor princomp quantile
#' @import ggplot2
#' @export
#'
#' @description
#' A function to label a new observation as "Missing" or "Non missing" using a training set trained with 'TwoSpamH_train()' function.
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
  ## new_data can only have one observation (one row)
  if(nrow(new_data) != 1){
    stop("new_data must have only one row")
  }
  ## all variables of new_data must be included in training data
  if(length(colnames(new_data)) != length(colnames(training_data)) - 1){ ## if the number of variables differs (subtract 1 from training data for 'label' column which is not used)
    missing_cols <- setdiff(colnames(new_data), colnames(training_data))
    if (length(missing_cols) > 0) { ## if there are some columns in new_data not present in training_data
      stop(paste("Columns from new_data missing in training_data:", paste(missing_cols, collapse = ", ")))
    }
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

  ## train and test sets for knn algorithm
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
