#' @title ellsae
#' @description \code{ellsae} is a method for small area estimation used to 
#' impute a missing variable from a smaller survey dataset into a census. 
#' The imputation is based on a linear model and bootstrap samples. 
#' 
#' @param model a model that is specified for the relationship betwenn the 
#'   response varibale and the regressors. Model must be a linear model that can 
#'   be processed by \code{lm()} 
#' @param surveydata Smaller surveydata with additional response variable of 
#'   interest. Will be used to estimate the linear model 
#' @param censusdata The dataset in which a certain variable is supposed to be 
#'   imputed 
#' @param location_survey Name of location variable or vector for the survey 
#'   data which is used for error correction and the location means (if 
#'   \code{clustermeans} is specified) 
#' @param clustermeans Additional parameters for the regression based on 
#' location means calculated from the census data to account for the lack of 
#' information in a small survey 
#' @param location_census name of location variable (string) in the census data 
#'   which is used for error correction and location means. 
#'   If \code{clustermeans} 
#'   is specified, but \code{location_census} is missing 
#' @param n_boot Number of bootstrap samples used for the estimation, default is 
#'   \code{n_boot = 50} 
#' @param welfare.function Additionally a welfare function for the response can 
#'   be specified 
#' @param parallel indicates if compution is supposed to be done in parallel to 
#'   improve speed 
#' @param output indicator for which output is requested as a list.
#' @param save_yboot logical indicator if the bootraps of the response y are 
#' supposed to be saved as a CSV file under your current working direktory. 
#' The name is: ...
#' @param num_cores utilizes the given number of cores to speed up the 
#' estimation. The number of cores can be determined by 
#' parallel::detectCores() - 1 or bigstatsr::nb_cores() that returnes only
#' physical cores. 
#' @param quantiles vector of requested quantiles for the \code{summaryboot}
#' output as decimals between 0 and 1.
#' @param transf function to transform the response y in the model
#' @param transf_inv function for backtransformation of \code{transf}
#' @param seed to make research reproducible a seed can be set. Simple 
#' \code{set.seed()} in R wont work as functions run in \code{C++}.
#' @param output as "default" a list with ... is returned. Write "all" for all
#' possible outputs or specify outputs yourself in a vector.
#' @return The function takes the the typically smaller surveydata and uses the 
#' argument \code{model} to estimate a linear model of the type \code{lm()}. 
#' In case the argument \code{clustermeans} is specified means from the cluster 
#' data for the given variables are calculated and merged with the survey 
#' data by cluster locations. These new explanatory variables are also used for 
#' the estimation of the linear model. 
#' 
#' In the second step a C++ fuction takes over and calculates \code{n_boot} 
#' predicted Y´s by using the betas from the first step to draw from a 
#' multivariate normal distributionand draws indicidual and nested errors at 
#' random with replacement. If requested the Y´s are used to estimate the 
#' welfare function after which either the mean of the yhat or of the welfare 
#' fuctiom is returned.
#' 
#' The function returns a list with different objects. If \code{output} 
#' is left unspecified the estimated Y´s or welfare estimates \code{yhat}, 
#' the fit of the linear model \code{model_fit} and a summary of the bootstrap 
#' samples
#' \code{bootstrapCI} are returned.
#' 
#' If \code{output} is specified all the arguments given are returned. 
#' Next to the 
#' above the following inputs are possible, \code{surveydata} and 
#' \code{censusdata}
#' return the data frames used for the compution as some rows might be 
#' deleted due 
#' to NA handling and additional variables are created for \code{clustermeans}. 
#' 
#' Additionally the bootstrapped Y´s can be saved as a CSV if 
#' \code{save_yboot} is 
#' set equal \code{TRUE} and can be found under the current working directory as 
#' "Bootraps-of-Y.csv". 
#' @seealso Other small area estimation methods can also be found 
#' in the package \code{sae}. 
#' @keywords SAE, imputation 
#' @references 
#'   Elbers, C., Lanjouw, J. O. and Lanjouw, P. (2003). 
#'   \emph{Micro-Level Estimation of Poverty and Inequality}. 
#'   In: Econometrica 71.1, pp. 355-364, Jan 2003
#' 
#'   Guadarrama Sanz, M., Molina, I., and Rao, J.N.K.  (2016). 
#'   \emph{A comparison of small area estimation methods for poverty mapping}. 
#'   In: 17 (Mar. 2016), 41-66 and 156 and 158.
#' @examples mean(c(1,2,3,4))
#' @export  


ellsae <- function(model, surveydata, censusdata, location_survey,
                   n_boot = 50, seed, welfare.function, transfy, transfy_inv, 
                   output = "default", num_cores = 1, 
                   quantiles = c(0, 0.25, 0.5, 0.75, 1), clustermeans, 
                   location_census, save_boot = F){
  
  
  # --------------------------- preliminaries -------------------------------- #
  
  #   the following code
  #   - checks whether all parameters are specified, 
  #     if not tries to reformat them appropriately
  #   - definies some parameters to be used later on
  #       - n_obs_survey
  #       - n_obs_census)
  #   - computes means from the census for the regression of the survey dataset
  #     and adds them to the surveydataset to be included in the later 
  #     regression
  
  
  ##### check whether n_boot was specified
  if(missing(n_boot)){
    message(cat("As n_boot was not provided it was per default set to ", 
                n_boot, sep = ""))
  } 
  if(!(length(n_boot) == 1)){
    stop("n_boot has to be provided as single integer")
  } 
  if(!is.integer(n_boot)){
    n_boot <- try(as.integer(n_boot))
    if(class(n_boot) == "try-error"){
      stop("n_boot has to be provided as single integer")
    } else {
      warning("n_boot was not provided as integer and has 
               been coerced to integer by ELLsae")
    }
  }
    
  
  
  ##### check whether model is specified correctly and if not trys to correct 
  if(missing(model)){stop("A model has to be specified")}
  if(class(model) != "formula"){
    model <- try(as.formula(model), silent = T)
    if (class(model) == "try-error"){
      stop("model must either be provided as a formula or as a string.
           See ?formula for help")
    }
  }
  
  ##### check whether surveydata is specified correctly and trys to correct
  if(missing(surveydata)) stop("Input surveydata is missing")
  if(!is.data.table(censusdata)){
    surveydata <- try(as.data.table(surveydata), silent = T)
    if (any(class(surveydata) == "try-error")){ #"any" prevents warning
      stop("survey data should be provided as data.table or something similar
            (e.g. a data.frame or a matrix). ELLsae was not able to convert 
            your input into a data.table")
    }
  }
  n_obs_survey <- nrow(surveydata)
  if(!all( all.vars(model)[-1] %in%  names(surveydata))){
    stop("the model you provided specifies variables 
          that are not included in the surveydata")
  }
  
  ##### check whether censusdata is specified correctly and try to correct
  if(missing(censusdata)) stop("Data frame with the censusdata is missing")
  if(!is.data.table(censusdata)){ 
    censusdata <- try(as.data.table(censusdata))
    if (any(class(censusdata) == "try-error")){
      stop("census data should be provided as data.table or something similar
(e.g. a data.frame or a matrix). ELLsae was not able to convert
           your input into a data.table")
    }
  }
  if(!all( all.vars(model)[-1] %in%  names(censusdata))){
    stop("the model you provided specifies variables 
          that are not included in the censusdata")
  }
  n_obs_census <- nrow(censusdata)
 
  ##### check whether the locations are specified correctly and try to correct
  if(missing(location_survey)) {
    stop("you have to provide a string with the variable indicating the
         location in the survey data set")
  }
  
  if (!(length(location_survey) == 1 & is.character(location_survey))) {
    stop("you have to provide a string with the variable indicating the 
         location in the survey data set")
  }
  
  if (!location_survey %in% names(surveydata)){
    stop("String that was specified as variable name for the location 
         is not the name of one of the variables in the survey data set.")
  }
  
  if(any(is.na(surveydata[, ..location_survey]))){ 
    warning("There are missing values in the locations of your surveydata set. 
            Rows with missing values were omitted") 
    na.omit(surveydata, cols = c(location_survey))
  } 
  
  if(!missing(transfy)){
    if(missing(transfy_inv)){
      if(transfy == log){
        transfy_inv <- exp
      } else {
        stop("if you want to transform the response variable with a function 
             different from 'log', you have to provide an inverse function for
             backtransformation of the bootstrap sample")
      }
      }
    y <- all.vars(model)[1]
    suveydata[, c(y) := transfy(..y)]
      }
  
  
  #### check if input for clustermeans is valid and reformat
  if(!missing(clustermeans)){
    # if clustermeans is used we need a string with the census location
    if(missing(location_census)){
      if(location_survey %in% names(censusdata)){
        location_census <- location_survey
      } else {
        stop("if you want to use clustermeans, you also have to provide a 
              string indicating the name of the location variable in the 
              census dataset. If the variable names are identical, one string 
             for location_survey suffices.")
      }
    }
    
    # checks if all the locations in the survey are equal those in the census. 
    if(!all(unique(surveydata[, ..location_survey]) %in% 
            unique(censusdata[,..location_census]))){
      stop("All locations that appear in the survey data must also appear 
           in the census data")
    }
    if(any(is.na(censusdata[, ..location_census]))){ 
      stop("The locations in the census are not allowed to have missing values 
           if location means are supposed to be computed.")
    }
    #### extract variables for which the mean is to be calculated
    if(clustermeans == ".") {
      vars_for_mean_calculation <- all.vars(model)[-1]
    } else if(is.character(clustermeans) & length(clustermeans == 1)){ 
      # Fall: "a + b + c + d" oder "a, b, c, d"
      # replace " " by "" --> remove blanks
      vars_for_mean_calculation <- gsub(pattern = " ", replacement="" , 
                                        clustermeans)
      vars_for_mean_calculation <- unlist(strsplit(vars_for_mean_calculation, 
                                                   split="\\+"))
      vars_for_mean_calculation <- unlist(strsplit(vars_for_mean_calculation, 
                                                   split=","))
    } else if(is.character(clustermeans)){
      vars_for_mean_calculation <- clustermeans
    } else {
      stop("In order to include the means of variables included in the census 
           in the model fit on the surveydata, you have to give a
           a) string with the variables you want to include separated 
           by \"+\" or \",\" or
           b) a character vector with your variables
           c) a \"\'.\'\" as string, indicating that you want to include the 
           mean of all the variables in your model")
    }
    if(!all(clustermeans %in%  names(censusdata))){
      stop("your input for clustermeans includes variables that are not present 
           in the censusdata set.Means for those variables 
           cannot be calculated")
    }
    if(!all( clustermeans %in%  names(surveydata))){
      warning("your input for clustermeans includes variables that are not 
              present in the surveydata set.Means for variables will be added 
              to the model for variables not originally present in the survey")
    }
    
    # compute means from census, add them to surveydata and update model
    new_var_names <- paste(vars_for_mean_calculation, "_meanCensus", sep="")
    censusdata[, c(new_var_names) := (lapply(.SD, mean)), 
               by = c(location_census),
               .SDcols = c(vars_for_mean_calculation)]
    
    means_from_census <- unique(censusdata[, c(..new_var_names, 
                                               ..location_census)])
    # unique is done to facilitate merging
    
    # make location_census in means_from_census equal to location_survey so 
    # they can be merged later on
    names(means_from_census)[names(means_from_census) == 
                               location_census] <- location_survey
    surveydata <- merge(surveydata, means_from_census, 
                        by = paste(location_survey), all.x = TRUE)
    
    model.in.characters <- as.character(model)
    model_left_hand_side <- model.in.characters[2]
    model_right_hand_side <- paste(model.in.characters[3],
                                   paste(new_var_names, collapse = " + "),
                                   sep = " + ")
    model <- as.formula(paste(model_left_hand_side, 
                              model_right_hand_side, sep = " ~ "))
  }

  
  # -------------------------- inference survey ------------------------------ #
  # the following code
  # - fits a linear model as specified by the user on the survey data
  # - calculates location effects and residual error terms from the 
  #   regression residuals according to:
  #   regresson_residuals = location_effect 
  #                               + (regresson_residuals - location_effect)
  #   with
  #     - location effect i = average of all regression_residuals ij in 
  #       location i
  #     - residual ij = regresson_residual ij - location_effect of location i
  
  
  ##### fit a OLS model based on the survey data set
  model_fit <- lm(model, data = surveydata)
  
  # add regression residuals to the surveydata
  surveydata[,regr_res := residuals(model_fit)]
  # calculate random location effects
  surveydata[, location_effect := mean(regr_res), by = c(location_survey)]
  location_effect <- unique(surveydata[, location_effect])
  
  # calculate residuals
  surveydata[, residuals := regr_res - location_effect]
  
  
  
  # -------------------------- inference census ------------------------------ #

  # the following code:
  # - draws a bootstrap sample of the location effects
  # - draws a boostrap sample of all residuals
  # - draws a multiariate normal sample of the betas
  # - calculates predicted y = x'beta + random location effect + random 
  #   error term
  # - applies a welfare function to every predicted y, if the user has 
  #   provided one
  # - aggregates the predicted ys (or predicted welfare estimates)
  
  # obtain the Design matrix for the prediction
  t <- terms.formula(model)
  t <- delete.response(t)
  X_census <- model.matrix(t, censusdata)
  if(any(is.na(X_census))){
    warning("some explanatory variables in the census data set were missing. 
            Affected rows were removed")
    X_census <- na.omit(X_census)
    attributes(X_census)[-1] <- NULL
  }
  
  
  if(!missing(seed)){
    runif(1) # make sure R sets seed internally
    previousseed <- .Random.seed
    on.exit( { .Random.seed <<- previousseed } )
    set.seed(seed)
  } else {
    seed <- as.numeric(Sys.time())
  }
  
  betas <- t(MASS::mvrnorm(n = n_boot,
                           mu = coefficients(model_fit),
                           Sigma = vcov(summary(model_fit))))

  # in Dokumentation schreiben: num_cores <- parallel::detectCores() - 1
  
  
  
  bootstrap <- .InfCensCpp(n_bootstrap = n_boot, 
                           n_obs_censusdata = n_obs_census,
                           locationeffects = location_effect, 
                           residuals = residuals(model_fit),
                           X = X_census, beta_sample = betas, userseed = seed, 
                           ncores = num_cores)
  

  if(!missing(transfy)){
    bootstrap <- transfy_inv(bootstrap)
  }
  
  # Runs the welfare funcation over the boostrap response variable Y
  if(!missing(welfare.function)){
    bootstrap <- welfare.function(bootstrap)
  } 
  
  
  
  output_list <- list()
  if(output == "default" | output == "all" | "yboot" %in% output){
    output_list$yboot_est <- rowMeans(bootstrap)
  }
  if(output == "default" | output == "all" | "summary" %in% output | 
     "summary_boot" %in% output){
    summaryboot <- .summaryParC(bootstrap, quantiles = quantiles, 
                                nrow = n_obs_census, ncol = n_boot, 
                                ncores = num_cores)
    colnames(summaryboot) <- c("mean", "var", "sd", 
                               paste(quantiles*100, "%-Quant", sep = ""))
    output_list$summary_boot <- summaryboot
  }
  if(output == "default" | output == "all" | "model_fit" %in% output){
    output_list$model_fit <- model_fit
  }
  if(output == "all" | "bootsample" %in% output){
    output_list$bootsample <- bootstrap
  }
  if(output == "all" | "survey" %in% output){
    output_list$survey <- surveydata
  } 
  if(output == "all" | "census" %in% output){
    output_list$census <- censusdata
  } 
  if(save_boot == T){
    fwrite(as.data.table(bootstrap), paste("BootstrapSampleELLsae-", 
                                           Sys.Date(),  ".csv", sep = ""))
  }
  
  return(output_list)
}
  


