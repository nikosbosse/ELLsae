#' Function for small area estimation
#'
#' \code{ELLsae} is a method for small area estimation used to impute a missing
#' variable from a smaller survey dataset into a census. The imputation is based
#' on a linear model and bootstrap samples.
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
#'   \code{mResponse} is specified)
#' @param mResponse Additional parameters for the regression based on location
#'   means calculated from the census data to account for the lack of
#'   information in a small survey
#' @param location_census name of location variable (string) in the census data
#'   which is used for error correction and location means. If \code{mResponse}
#'   is specified, but \code{location_census} is missing
#' @param n_boot Number of bootstrap samples used for the estimation, default is
#'   \code{n_boot = 50}
#' @param welfare.function Additionally a welfare function for the response can
#'   be specified
#' @param parallel indicates if compution is supposed to be done in parallel to
#'   improve speed
#' @aliases sae, ELL, ellsae
#' @export yes
#' @return The function returns a vector of the imputed variable as well as ...
#' @references A
#' @seealso Other SAE methods can also be found in the package \code{\link[sae]{}}}.
#' @keywords SAE imputation
#' @examples no examples are currently specified


ELLsae <- function(model, surveydata, censusdata, location_survey,
                    mResponse, location_census, n_boot = 50, welfare.function, test,
                    parallel = F){
  
  
  # --------------------------------------------------------------------------------- #
  # ----------------------------- preliminaries ------------------------------------- #
  # --------------------------------------------------------------------------------- #
  
  #   the following code
  #   - checks whether all parameters are specified, if not tries to reformat them appropriately
  #   - definies some parameters to be used later on
  #       - n_obs_survey
  #       - n_obs_census)
  #   - computes means from the census for the regression of the survey dataset
  #     and adds them to the surveydataset to be included in the later regression
  
  
  ##### check whether n_boot was specified
  if(missing(n_boot)){message(cat("As n_boot was not provided it was per default set to ", n_boot, sep = ""))}
  
  ##### check whether model is specified correctly and try to correct misspecification
  if(missing(model)){stop("A model has to be specified")}
  if(class(model) != "formula"){
    model <- try(as.formula(model), silent = T)
    if (class(model) == "try-error"){
      stop("model must either be provided as a formula or as a string.
           See ?formula for help")
    }
  }
  
  ##### check whether the locations are specified correctly and try to correct
  if(missing(location_survey)) stop("you have to provide either 1) a vector of locations of length corresponding to the number of observations in the survey data or 2) a string with the name of a variable in the surveydata that provides the locations of individual observations")
  if (!(length(location_survey) == 1 & is.character(location_survey))) {
    stop("you have to provide a string with the variable indicating the location in the survey data set")
  }
  if (!any(location_survey == names(surveydata))){
    stop("String that was specified as variable name for the location is not the name of one of the variables in the survey data set.")
  }
  ### ACHTUNG ### wichtig das die locations vor dem survey-datensatz gescheckt werden und das
  # falls es ein separater vektor ist, 
  
  ##### check whether surveydata is specified correctly and try to correct
  if(missing(surveydata)) stop("Data frame with the surveydata is missing")
  # important for the model.frame calculation
  if(class(surveydata) != "data.frame"){
    surveydata <- try(as.data.frame(surveydata), silent = T)
    if (any(class(surveydata) == "try-error")){
      stop("Survey data should be provided as data.frame or matrix.
           ELLsae was not able to convert your input into a data.frame")
    }
  # reducing the data frame to only the model variables to save memory space and
  # also use this for NA-handling
  surveydata <- model.frame(formula = model,na.action = na.omit, data = surveydata)
  # now the reduced data.frame will be transformed into a data.table object
  if(class(surveydata) != "data.table"){
    surveydata <- try(as.data.table(surveydata), silent = T)
    if (any(class(surveydata) == "try-error")){
      stop("Survey data should be provided as data.table or something similar.
           ELLsae was not able to convert your input into a data.table")
    }
  }
  n_obs_survey <- nrow(surveydata)
  if(!all( all.vars(model)[-1] %in%  names(surveydata))){
    stop("the model you provided specifies variables that are not included in the surveydata")
  }
  
  ##### check whether censusdata is specified correctly and try to correct
  if(missing(censusdata)) stop("Data frame with the censusdata is missing")
  # # important for the model.frame calculation
  # if(class(censusdata) != "data.frame"){
  #   censusdata <- try(as.data.frame(censusdata), silent = T)
  #   if (any(class(censusydata) == "try-error")){
  #     stop("Census data should be provided as data.frame or matrix.
  #          ELLsae was not able to convert your input into a data.frame")
  #   }
  #   # reducing the data frame to only the model variables to save memory space and
  #   # also use this for NA-handling
  #   censusdata <- model.frame(formula = model,na.action = na.omit, data = censusdata)
  # }
  #   # now the reduced data.frame will be transformed into a data.table object
  if(class(censusdata) != "data.table"){ # alternativ if(!is.data.table(censusdata))?
    censusdata <- try(as.data.table(censusdata))
    if (any(class(censusdata) == "try-error")){
      stop("census data should be provided as data.table or something similar.
           ELLsae was not able to convert your input into a data.table")
    }
  }
  if(!all( all.vars(model)[-1] %in%  names(censusdata))){
    stop("the model you provided specifies variables that are not included in the censusdata")
  }
  n_obs_census <- nrow(censusdata)
  
  
  #### check if input for mResponse is valid and reformat
  if(!missing(mResponse)){
    # if mResponse is used we need a string with the census location
    if(missing(location_census)){
      if(any(location_survey == names(censusdata))){
        location_census <- location_survey
      } else {
        stop("if you want to use mResponse, you also have to provide a string indicating the name of the location variable in the census dataset.
             If the variable names are identical, one string for location_survey suffices.")
      }
    }
    # unique() %in% unique() 
    #### extract variables for which the mean is to be calculated
    if(mResponse == ".") {
      vars_for_mean_calculation <- all.vars(model)[-1]
    } else if(is.character(mResponse) & length(mResponse == 1)){ # Fall: "a + b + c + d" oder "a, b, c, d"
      # replace " " by "" --> remove blanks
      vars_for_mean_calculation <- gsub(pattern = " ", replacement="" , mResponse)
      vars_for_mean_calculation <- unlist(strsplit(vars_for_mean_calculation, split="\\+"))
      vars_for_mean_calculation <- unlist(strsplit(vars_for_mean_calculation, split=","))
    } else if(is.character(mResponse)){
      vars_for_mean_calculation <- mResponse
    } else {
      stop("In order to include the means of variables included in the census in the model fit on the surveydata, you have to give a
           a) string with the variables you want to include separated by \"+\" or \",\" or
           b) a character vector with your variables
           c) a \"\'.\'\" as string, indicating that you want to include the mean of all the variables in your model")
    }
    if(any(unique(surveydata[, location_survey]) != unique(censusdata[,location_census]))){
      stop("Locations from the survey data must be nested in the census data")
    }
    if(any(is.na(surveydata[, location_survey]))){
      stop("The locations are not allowed to have missing values")
    }
    if(!all( mResponse %in%  names(censusdata))){
      stop("Your input for mResponse includes variables that are not present in the censusdata set.
           Means for those variables cannot be calculated")
    }
    if(!all( mResponse %in%  names(surveydata))){
      warning("Your input for mResponse includes variables that are not present in the surveydata set.
              Means for variables will be added to the model for variables not originally present in the survey")
    }
    
    # compute means from census, add them to surveydata and update model
    new_var_names <- paste(vars_for_mean_calculation, "_meanCensus", sep="")
    censusdata[, c(new_var_names) := (lapply(.SD, mean)), by = c(location_census),
               .SDcols = c(vars_for_mean_calculation)]
    
    means_from_census <- unique(censusdata[, c(..new_var_names, ..location_census)])
    # unique is done to facilitate merging
    
    # make location_census in means_from_census equal to location_survey so they can be merged later on
    names(means_from_census)[names(means_from_census) == location_census] <- location_survey
    surveydata <- merge(surveydata, means_from_census, by = paste(location_survey), all.x = TRUE)
    
    # We save only save the necessary variables from the census data:
    # censusdata <- censusdata[all.vars(model)[-1]]
    
    model.in.characters <- as.character(model)
    model_left_hand_side <- model.in.characters[2]
    model_right_hand_side <- paste(model.in.characters[3],
                                   paste(new_var_names, collapse = " + "),
                                   sep = " + ")
    model <- as.formula(paste(model_left_hand_side, model_right_hand_side, sep = " ~ "))
  }
  
  
  # --------------------------------------------------------------------------------- #
  # ----------------------------- inference survey ---------------------------------- #
  # --------------------------------------------------------------------------------- #
  # the following code
  # - fits a linear model as specified by the user on the survey data
  # - calculates location effects and residual error terms from the regression residuals
  #   according to:
  #   regresson_residuals = location_effect + (regresson_residuals - location_effect)
  #   with
  #     - location effect i = average of all regression_residuals ij in location i
  #     - residual ij = regresson_residual ij - location_effect of location i
  
  
  ##### fit a OLS model based on the survey data set
  model_fit <- lm(model, data = surveydata)
  
  # add regression residuals to the surveydata
  surveydata[,regr_res := residuals(model_fit)]
  # calculate random location effects
  surveydata[, location_effect := mean(regr_res), by = c(location_survey)]  # apparently sum(x)/length(x) is faster..
  # calculate residuals
  surveydata[, residuals := regr_res - location_effect]
  
  
  
  # --------------------------------------------------------------------------------- #
  # ----------------------------- inference census ---------------------------------- #
  # --------------------------------------------------------------------------------- #
  
  # the following code:
  # - draws a bootstrap sample of the location effects
  # - draws a boostrap sample of all residuals
  # - draws a multiariate normal sample of the betas
  # - calculates predicted y = x'beta + random location effect + random error term
  # - applies a welfare function to every predicted y, if the user has provided one
  # - aggregates the predicted ys (or predicted welfare estimates)
  
  
  if(parallel == T){
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores)
    doParallel::registerDoParallel(cl)
    parallel::stopCluster(cl)
  }
  
  nsplit <- 5
  classLocation_boot_list <- vector("list", nsplit)
  location_effect <- unique(surveydata$location_effect)
  classLocation_boot_list <- foreach::foreach(i=1:nsplit, .packages=c("ELLsae", "Rcpp")) %do% ELLsae:::rddrawmatrixC(num_unique_elements = length(location_effect), # den Teil in C++ löschen
                                                                                                                     n_bootstrap = n_boot/nsplit, n_obs_censusdata = n_obs_census,
                                                                                                                     elements_to_draw_from = location_effect)
  
  
  
  # random_location_boot <- (rddrawmatrixC(num_unique_elements = length(unique(location_survey)),
  #                                                      n_bootstrap = n_boot, n_obs_censusdata = n_obs_census,
  #                                                      elements_to_draw_from = location_effect$location_effect))
  # # draw random resdiduals with a bootstrap sample
  
  bootstrap_res <- foreach::foreach(i=1:nsplit, .combine='cbind',  .packages=c("ELLsae", "Rcpp")) %do% ELLsae:::rddrawmatrixC(num_unique_elements = n_obs_survey,
                                                                                                                        n_bootstrap = n_boot/nsplit, n_obs_censusdata = n_obs_census,
                                                                                                                        elements_to_draw_from = residuals(model_fit))
  
  
  #
  # bootstrap_res <- (rddrawmatrixC(num_unique_elements = n_obs_survey,
  #                                             n_bootstrap = n_boot, n_obs_censusdata = n_obs_census,
  #                                             elements_to_draw_from = residuals(model_fit)))
  
  # n_split <- 5
  # subset_ind <- rep(seq.int(from = 1, to = n_split))
  
  
  beta_sample <- t(MASS::mvrnorm(n = n_boot,
                                 mu = coefficients(model_fit),
                                 Sigma = vcov(summary(model_fit))))
  t <- terms.formula(model)
  t <- delete.response(t)
  X <- model.matrix(t, censusdata)
  X <- na.omit(X)
  
  # X <- as.matrix(model.frame(t, censusdata))
  
  y_hat_mvnorm <- X %*% beta_sample
  
  
  
  # splivector <- ceiling(seq.int(from = 1, to = n_obs_census)/20000)
  # a <- split.data.frame((X), splivector)
  # unsplit((a), ceiling(seq_along(X)/20000))
  # nrow(a$`1`)
  
  
  # put y_hat_bootstrap, location effect and residual from bootstrap together
  y_bootstrap <- y_hat_mvnorm + random_location_boot + bootstrap_res
  # aggregate bootstrap welfare estimates to one estimate
  # if welfare function is provided: calculate Welfare measure based on y_bootstrap
  if(!missing(welfare.function)){
    welfare_bootstrap <- welfare.function(y_bootstrap)
    result <- rowmeanC(welfare_bootstrap)
  } else {
    result <- rowmeanC(y_bootstrap)
  }
  
  
  
  
  if(!missing(test)) {
    if(test == "meanforregression"){
      m <- list(censusmeans = means_from_census,
                surveydata = surveydata,
                newmodel = model)
      return(m)
    }
    if(test == "welfare_bootstrap"){
      return(welfare_bootstrap)
    }
    if(test == "yboot"){
      y_simple_pred <- predict(model_fit, newdata = censusdata)
      res <- list(ymvnorm = y_hat_mvnorm,
                  locboot = random_location_boot,
                  resboot = bootstrap_res,
                  ysimplepred = y_simple_pred)
    }
  }
  
  
  return(result)
}}
