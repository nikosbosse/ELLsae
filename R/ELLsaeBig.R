#' @title ELLsaeBig
#' @description Beschreibung der Funktion
#'
#' @param model a model that is specified for the relationship betwenn
#' the response varibale and the regressors. Model must be a linear model that can be processed by \code{lm()}
#' @param surveydata Smaller surveydata with additional response variable of interest.
#' Will be used to estimate the linear model
#' @param censusdata The dataset in which a certain variable is supposed to be imputed
#' @param location_survey Name of location variable or vector for the survey data which is used for
#' error correction and the location means (if \code{mResponse} is specified)
#' @param mResponse Additional parameters for the regression based on location means
#' calculated from the census data to account for the lack of information in a small survey
#' @param n_boot Number of bootstrap samples used for the estimation, default is 50
#' @param welfare.function Additionally a welfare function for the response can be specified
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples


ELLsaeBig <- function(model, surveydata, censusdata, location_survey,
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
  
  ##### check whether surveydata is specified correctly and try to correct
  if(missing(surveydata)) stop("Data frame with the surveydata is missing")
  if(class(surveydata) != "data.table"){
    surveydata <- try(as.data.table(surveydata), silent = T)
    if (any(class(surveydata) == "try-error")){
      stop("survey data should be provided as data.table or something similar.
           ELLsae was not able to convert your input into a data.table")
    }
    }
  n_obs_survey <- nrow(surveydata)
  if(!all( all.vars(model)[-1] %in%  names(surveydata))){
    stop("the model you provided specifies variables that are not included in the surveydata")
  }
  
  ##### check whether censusdata is specified correctly and try to correct
  if(missing(censusdata)) stop("Data frame with the censusdata is missing")
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
  
  ##### check whether the locations are specified correctly and try to correct
  if(missing(location_survey)) stop("you have to provide either 1) a vector of locations of length corresponding to the number of observations in the survey data or 2) a string with the name of a variable in the surveydata that provides the locations of individual observations")
  if (!(length(location_survey) == 1 & is.character(location_survey))) {
    stop("you have to provide a string with the variable indicating the location in the survey data set")
  }
  if (!any(location_survey == names(surveydata))){
    stop("String that was specified as variable name for the location is not the name of one of the variables in the survey data set.")
  }
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
    if(!all( mResponse %in%  names(censusdata))){
      stop("your input for mResponse includes variables that are not present in the censusdata set.
           Means for those variables cannot be calculated")
    }
    if(!all( mResponse %in%  names(surveydata))){
      warning("your input for mResponse includes variables that are not present in the surveydata set.
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
  location_effect <- unique(surveydata$location_effect)
  # location_effect <- unique(surveydata[, location_effect])
  
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
  
  # obtain the Design matrix for the prediction
  t <- terms.formula(model)
  t <- delete.response(t)
  X_census <- model.matrix(t, censusdata)
  
  betas <- t(MASS::mvrnorm(n = n_boot,
                           mu = coefficients(model_fit),
                           Sigma = vcov(summary(model_fit))))
  
    y_bootstrap <- bigstatsr::FBM(nrow = n_obs_census, ncol = n_boot, type = "double")
  
  
  if(parallel == T){
    # bigstatsr::big_apply(y_bootstrap,
    #                      a.FUN = function(y_bootstrap, ind, fun, n_obs, loc, res, X_census, betas) {
    #   y_bootstrap[, ind] <- fun(n_bootstrap = length(ind),
    #                             n_obs_censusdata = n_obs,
    #                             locationeffects = loc,
    #                             residuals = res,
    #                             X = X_census, beta_sample = betas)
    #   NULL
    # }, a.combine = 'c', ncores = bigstatsr::nb_cores(), fun = ELLsae::inferenceCensusC,
    # n_obs = n_obs_census, loc = location_effect, res = residuals(model_fit),
    # X_census = X_census, betas = betas)
  } else {
      maxsize <- 300000000 # 300000000 #arbitrarily define a upper limit per submatrix, here: 300mb
      numentries_max <- maxsize / 8 # assume that one entry in a matrix / data.frame takes up 8 bytes
      num_boot_max <- floor(numentries_max / n_obs_census)
      if(n_boot > num_boot_max){
        rest <- n_boot %% num_boot_max
        if(rest == 0){rest <- num_boot_max}
        number_of_chunks <- ceiling(n_boot / num_boot_max)
        vector_of_num_boots <- c(rep(num_boot_max, number_of_chunks - 1), rest)
      } else {
        number_of_chunks <- 1
        vector_of_num_boots <- n_boot
      }
    ind <- c(0, cumsum(vector_of_num_boots))
    if(!missing(welfare.function)){
      tmp <- foreach(i=1:number_of_chunks, .combine = "c") %do% {
        y_bootstrap[,(ind[i] + 1):ind[i +1] ] <- welfare.fun(ELLsae:::inferenceCensusC(n_bootstrap = vector_of_num_boots[i], 
                                                                           n_obs_censusdata = n_obs_census, locationeffects = location_effect, 
                                                                           residuals = residuals(model_fit), X = X_census, beta_sample = betas))
        NULL
      }
    } else {
      tmp <- foreach(i=1:number_of_chunks, .combine = "c") %do% {
        y_bootstrap[,(ind[i] + 1):ind[i +1] ] <- ELLsae:::inferenceCensusC(n_bootstrap = vector_of_num_boots[i], 
                                                                           n_obs_censusdata = n_obs_census, locationeffects = location_effect, 
                                                                           residuals = residuals(model_fit), X = X_census, beta_sample = betas)
        NULL
      }
    }
  }
  

  

  result <- ELLsae::rowmeansBigC(y_bootstrap)

  
  if(!missing(test)) {
    if(test == "meanforregression"){
      m <- list(censusmeans = means_from_census,
                surveydata = surveydata,
                newmodel = model)
      return(m)
    }
    if(test == "welfare_bootstrap"){
      welfare_bootstrap <- welfare.function(y_bootstrap)
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
}
