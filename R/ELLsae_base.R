#'@title ellsae
#'@description The function \code{ellsae} implements the "ELL-method" method for
#'  small area estimation by Elbers, C., Lanjouw, J. O. and Lanjouw, P (2003)
#'  used to impute a missing variable from a smaller survey dataset into a
#'  census. The imputation is based on a linear model and bootstrap samples
#'
#'@param model a model that describes the relationship betwenn the response and
#'  the explanatory variables. Input must be a linear model that can be
#'  processed by \code{lm()}
#'@param surveydata data set with the response variable of interest included.
#'  Will be used to estimate the linear model
#'@param censusdata dataset where the variable of interest is missing and shall
#'  be imputed
#'@param location_survey string with the name of the variable in the survey data
#'  set that contains information about the cluster (= location) of an
#'  observation
#'@param n_boot integer with size of bootstrap sample
#'@param seed integer, seed can be set to obtain reproducible results
#'@param welfare.function function that transforms the bootstraped variable of
#'interested to obtain some welfare estimate
#'@param transfy function to transform the response y in the model
#'@param transfy_inv inverse function of \code{transfy} for backtransformation
#'@param output character string or character vector. Either "default", "all",
#'  or a vector with one or more of the following elements: c("summary",
#'  "yboot", "model_fit", "bootsample", "survey", "census")
#'@param cores either a string, "auto", or an integer value indicating the
#'  number of cores to be used for the estimation.
#'@param quantiles vector of requested quantiles for the \code{summaryboot}
#'  output as decimals between 0 and 1.
#'@param clustermeans character vector with names of variables present in both
#'  data sets. The mean of those variables in the census will be computed by
#'  location and added to the survey data set before estimation of the linear
#'  model. This may enhance precision of your estimates
#'@param location_census string with the name of the variable in the survey data
#'  set that contains information about the cluster (= location) of an
#'  observation. Only needed if \code{clustermeans} shall be computed.
#'@param save_boot logical value. TRUE saves the bootstrap sample as
#'  BootstrapSampleELLsae-DATE.csv in your current working direktory.
#'

#'
#'@details The function takes the the surveydata and uses the argument
#'\code{model} to estimate a linear model of the type \code{lm()}. In case the
#'argument \code{clustermeans} is specified, means from the census data for the
#'given variables are calculated and merged with the survey data by cluster
#'locations. These new explanatory variables are also used for the estimation of
#'the linear model. Rows with NA's are omitted from the computation.
#'
#'The user may choose to transform the response variable using
#'a function, \code{transfy} previously to estimating the model.
#'This function will be directly applied to the vector of the response
#'variable, i.e. \code{transfy(response)}. In principle this could also be
#'achieved by altering the specified model, but using \code{transfy} and
#'\code{transfy_inv} is the recommended usage. 
#'
#'From the regression, location
#'effects are calculated as the mean by location of the regression residuals.
#'Individual random error terms are then obtained as the difference between the
#'regression residuals and the location effects. The bootstrapped response
#'variables are generated using three sources of randomness. The betas obtained
#'from \code{lm()} are replaced by draws from a multivariate normal
#'distribution. In addition random location effects and residuals are drawn with
#'replacement. Internally the sample is a matrix, \code{bootstrap}, with
#'the rows corresponding to bootstrap samples for one individual observation in
#'the census data set. 
#'
#'If \code{transfy_inv} was specified, the bootstrap sample
#'is transformed back. This function will be directly applied to the matrix
#'of bootstrap samples, i.e. \code{transfy_inv(bootstrap)}. 
#'
#'If a welfare
#'function was specified it will be used to transform the bootstrap sample. It
#'will be diretly applied to the matrix of bootstrap samples, i.e.
#'\code{welfare.function(bootstrap)}.
#'
#'\code{cores} specifies the number of cores to use for the calculation. As
#'parallelization is done in C++ and incurs little overhead this should in most
#'cases be left to "auto".
#'
#'To obtain reproducicble results, seed must be specified. Simply running
#'\code{set.seed()} in R does not work. Providing a seed will not permanently
#'alter the seed in R.
#'
#'@return \code{ellsae} returns a list. By default, this list included a matrix
#'with basic summary statistics as specified in \code{quantiles}, a vector with
#'the means of the bootstrap samples for every observation, and the
#'\code{lm}-object obtained from the linear model estimation. In addition, the
#'user can request the full matrix of bootstrap samples, and an updated
#'data.table of the survey and census data set with residuals and location
#'effects and clustermeans added.
#'
#'
#'@seealso Other small area estimation methods can also be found in the package
#'  \code{sae}.
#'@keywords SAE, imputation
#'@references Elbers, C., Lanjouw, J. O. and Lanjouw, P. (2003).
#'\emph{Micro-Level Estimation of Poverty and Inequality}. In: Econometrica
#'71.1, pp. 355-364, Jan 2003
#'
#'Guadarrama Sanz, M., Molina, I., and Rao, J.N.K.  (2016). \emph{A comparison
#'of small area estimation methods for poverty mapping}. In: 17 (Mar. 2016),
#'41-66 and 156 and 158.
#'@examples
#'# Generate a sample survey and census data from the provided brazil data set
#'brazil <-  ELLsae::brazil
#'helper <- sample(x = 1:nrow(brazil), size = nrow(brazil)/5, replace = F)
#'helper <- sort(helper)
#'survey <- brazil[helper,]
#'census <- brazil[-helper,]
#'model.example <- hh_inc ~ geo2_br + age + sex + computer + trash
#'
#' ELLsae::ellsae(model = model.example,
#'                surveydata = survey,
#'                censusdata = census,
#'                location_survey = "geo2_br",
#'                n_boot = 250L,
#'                seed = 1234,
#'                transfy = log,
#'                transfy_inv = exp,
#'                output = "all",
#'                cores = "auto",
#'                quantiles = c(0, 0.25, 0.5, 0.75, 1),
#'                clustermeans = "age",
#'                location_census = "geo2_br",
#'                save_boot = F)
#'@export


ellsae <- function(model,
                   surveydata,
                   censusdata,
                   location_survey,
                   n_boot = 250L,
                   seed,
                   welfare.function,
                   transfy,
                   transfy_inv,
                   output = "default",
                   cores = "auto",
                   quantiles = c(0, 0.25, 0.5, 0.75, 1),
                   clustermeans,
                   location_census,
                   save_boot = F) {
  
  
  # --------------------------- preliminaries -------------------------------- #
  
  #   the following code
  #   - checks whether all parameters are specified,
  #     if not tries to reformat them appropriately
  #   - definies some parameters to be used later on, e.g. n_obs_survey, 
  #     n_obs_census)
  #   - transfoms response variable 
  #   - computes means from the census for the regression of the survey dataset
  #     and adds them to the surveydataset to be included in the later
  #     regression
  
  
  ##### check whether model is specified correctly and if not try to correct
  if (missing(model)) {
    stop("A model has to be specified")
  }
  if (class(model) != "formula") {
    model <- try(as.formula(model), silent = T)
    if (class(model) == "try-error") {
      stop("model must either be provided as a formula or as a string.
           See ?formula for help")
    }
  }
  # saving the model parameters to avoid recalculation later on
  response <- all.vars(model)[1]
  explanatories <- all.vars(model)[-1]
  
  ##### check whether surveydata is specified correctly and try to correct
  if (missing(surveydata))
    stop("Input surveydata is missing")
  if (!is.data.table(surveydata)) {
    surveydata <- try(as.data.table(surveydata), silent = T)
    if (!is.data.table(surveydata)) {
      stop(
        "survey data should be provided as data.table or something similar
        (e.g. a data.frame or a matrix). ELLsae was not able to convert
        your input into a data.table"
      )
    }
  }
  # all the variables of the model have to be in the data as well
  if (!all(explanatories %in%  names(surveydata))) {
    stop("the model you provided specifies variables
         that are not included in the surveydata")
  }
  
  ##### check whether censusdata is specified correctly and try to correct
  if (missing(censusdata))
    stop("Data frame with the censusdata is missing")
  if (!is.data.table(censusdata)) {
    censusdata <- try(as.data.table(censusdata), silent = T)
    if (!is.data.table(censusdata)) {
      stop(
        "census data should be provided as data.table or something similar
        (e.g. a data.frame or a matrix). ELLsae was not able to convert
        your input into a data.table"
      )
    }
  }
  # all the variables of the model have to be in the data as well
  if (!all(explanatories %in%  names(censusdata))) {
    stop("the model you provided specifies variables
         that are not included in the censusdata")
  }
  
  ##### check whether the locations are specified correctly and try to correct
  if (missing(location_survey)) {
    stop(
      "you have to provide a string with the variable indicating the
      location in the survey data set"
    )
  }
  # check whether locaion_survey is specified correctly
  if (!(length(location_survey) == 1 &
        is.character(location_survey))) {
    stop(
      "you have to provide a string with the variable indicating the
      location in the survey data set"
    )
  }
  # localtion_survey has to be avariable of the survey
  if (!location_survey %in% names(surveydata)) {
    stop(
      "String that was specified as variable name for the location
      is not the name of one of the variables in the survey data set."
    )
  }
  
  ##### check whether n_boot was specified
  if (length(n_boot) != 1) {
    stop("n_boot has to be provided as single integer")
  }
  # n_boot has to be an integer that can be passed to C++ - function
  if (!is.integer(n_boot)) {
    n_boot <- try(as.integer(n_boot), silent = T)
    if (!is.integer(n_boot)) {
      stop("n_boot has to be provided as single integer")
    }
  }
  
  # if a seed is specified, save the internal seed and restore it later
  if (!missing(seed)) {
    runif(1) # make sure R seed is set internally
    previousseed <- .Random.seed
    on.exit({
      .Random.seed <<- previousseed
    })
    set.seed(seed)
  } else {
    seed <- as.numeric(Sys.time()) # seed needed for C++
  }
  
  ##### check whether seed is now correctly specified
  if (length(seed) != 1) {
    stop("If you want to set a seed it has to be provided as single integer")
  }
  if (!is.integer(seed)) {
    seed <- try(as.integer(seed), silent = T)
    if (!is.integer(seed)) {
      stop("If you want to set a seed it has to be provided as single integer")
    }
  }
  
  # checks whether the user wants a transformation of the response
  if (!missing(transfy)) {
    if (missing(transfy_inv)) {
      message("you have transformed y, but not provided a funtion transfy_inv
              for backtransformation")
    } 
    #surveydata[, ..response := transfy(..response)]
    set(surveydata, j = response, value = transfy(surveydata[[response]]))
  }
  
  
  #### check whether output was correctly specified
  if (!is.character(output)) {
    stop(
      "your input for character should eiher be 'all', 'default', or a
      vector with the outputs you want, e.g. c('summary_boot','yboot_est', ...)"
    )
  }
  
  ##### check whether cores was correctly specified
  if (length(cores) != 1) {
    stop("cores has to be either 'auto' or a single integer")
  }
  if (cores == "auto"){
    cores <- 999L
  }
  if (!is.integer(cores)) {
    cores <- try(as.integer(cores), silent = T) 
    if (!is.integer(cores)) {
      stop("cores has to be either 'auto' or a single integer")
    }
  }
  
  # Trys to correct the quantiles if specified incorrectly
  quantiles <- try(as.numeric(quantiles))
  #must be done to pass to C++
  if (class(quantiles) == "try-error") {
    stop("quantiles must be provided as an ascending vector of numbers
         between 0 and 1")
  }
  if (any(is.na(quantiles))) {
    stop("quantiles must be provided as an ascending vector of numbers
         between 0 and 1")
  }
  if (any(quantiles < 0) | any(quantiles > 1)) {
    quantiles <- quantiles[-which(quantiles < 0 | quantiles > 1)]
    warning("quantiles < 0 and >1 are automatically omitted")
  }
  if (length(quantiles) == 0) {
    quantiles <- c(0.5) 
    # can't pass vector of length 0 to C++ - code
  }
  quantiles <- sort(quantiles) # sort to be safe for C++ - code
  
  
  
  # check for NA and remove 
  if (any(is.na(surveydata[, c(..response, ..explanatories)]))) {
    surveydata <- surveydata[complete.cases(surveydata[, c(..response, ..explanatories)]), ]
    warning("your surveydata had missing values. Affected rows were removed.
            Maybe they were introduced through transfy?" )
  }
  if (any(is.na(censusdata[, c(..explanatories)]))) {
    censusdata <- censusdata[complete.cases(censusdata[, c(..explanatories)]), ]
    warning("your censusdata had missing values. Affected rows were removed.")
  }

  n_obs_survey <- nrow(surveydata)
  n_obs_census <- nrow(censusdata)
  
  
  ######## clustermeans #######
  
  #### check if input for clustermeans is valid and reformat
  if (!missing(clustermeans)) {
    # if clustermeans is used we need a string with the census location
    if (missing(location_census)) {
      if (location_survey %in% names(censusdata)) {
        location_census <- location_survey
      } else {
        stop(
          "if you want to use clustermeans, you also have to provide a
          string indicating the name of the location variable in the
          census dataset. If the variable names are identical, one string
          for location_survey suffices."
        )
      }
    }
    
    # checks if all the locations in the survey are equal those in the census.
    if (!all(unique(surveydata[, ..location_survey]) %in%
             unique(censusdata[, ..location_census]))) {
      stop(
        "All locations that appear in the survey data must also appear
        in the census data, there might also be missing values"
      )
    }
    if (any(is.na(censusdata[, ..location_census]))) {
      stop(
        "The locations in the census are not allowed to have missing values
        if location means are supposed to be computed."
      )
    }
    #### extract variables for which the mean is to be calculated
    if (length(clustermeans) == 1 && clustermeans == ".") {
      vars_for_mean_calculation <- all.vars(model)[-1]
    } else if (is.character(clustermeans) &
               length(clustermeans == 1)) {
      # handle cases "a + b + c + d" or "a, b, c, d"
      # replace " " by "" --> remove blanks
      vars_for_mean_calculation <-
        gsub(pattern = " ",
             replacement = "" ,
             clustermeans)
      vars_for_mean_calculation <-
        unlist(strsplit(vars_for_mean_calculation,
                        split = "\\+"))
      vars_for_mean_calculation <-
        unlist(strsplit(vars_for_mean_calculation,
                        split = ","))
    } else if (is.character(clustermeans)) {
      vars_for_mean_calculation <- clustermeans
    } else {
      stop(
        "In order to include the means of variables included in the census
        in the model fit on the surveydata, you have to give a
        a) string with the variables you want to include separated
        by \"+\" or \",\", e.g. 'age, sex' or
        b) a character vector with your variables, e.g. c('age', 'sex')
        c) a \"\'.\'\" as string, indicating that you want to include the
        mean of all the variables in your model"
      )
    }
    if (!all(clustermeans %in%  names(censusdata))) {
      stop(
        "your input for clustermeans includes variables that are not present
        in the censusdata set. Means for those variables cannot be
        calculated"
      )
    }
    if (!all(clustermeans %in%  names(surveydata))) {
      warning(
        "your input for clustermeans includes variables that are not
        present in the surveydata set. Means for variables will be added
        to the model for variables not originally present in the survey"
      )
    }
    
    # compute means from census, add them to surveydata and update model
    new_var_names <- paste(vars_for_mean_calculation, "_meanCensus", sep = "")
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
    surveydata <- merge(
      surveydata,
      means_from_census,
      by = paste(location_survey),
      all.x = TRUE
    )
    
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
  #                         + (regresson_residuals - location_effect)
  #   with
  #     - location effect i = average of all regression_residuals ij in
  #       location i
  #     - residual ij = regresson_residual ij - location_effect of location i
  
  
  ##### fit a linear model based on the survey data set
  model_fit <- lm(model, data = surveydata)
  
  # add regression residuals to the surveydata
  surveydata[, regr_res := residuals(model_fit)]
  # calculate random location effects
  surveydata[, location_effect := mean(regr_res), by = c(location_survey)]
  location_effect <- unique(surveydata[, location_effect])
  
  # calculate residuals
  surveydata[, residuals := regr_res - location_effect]
  
  
  
  # -------------------------- inference census ------------------------------ #
  
  # the following code:
  # - draws a bootstrap sample of the location effects
  # - draws a boostrap sample of all residuals
  # - draws a multivariate normal sample of the betas
  # - calculates predicted y = x'beta + random location effect + random
  #   error term
  # - makes backtransformation of bootstrap sample
  # - applies a welfare function to every predicted y, if the user has
  #   provided one
  # - aggregates the predicted ys (or predicted welfare estimates)
  # - creates the output
  
  # obtain the design matrix for the prediction
  t <- terms.formula(model)
  t <- delete.response(t)
  X_census <- model.matrix(t, censusdata)
  
  betas <- t(MASS::mvrnorm(
    n = n_boot,
    mu = coefficients(model_fit),
    Sigma = vcov(summary(model_fit))
  ))
  
  bootstrap <- .InfCensCpp(
    n_bootstrap = n_boot,
    n_obs_censusdata = n_obs_census,
    locationeffects = location_effect,
    residuals = residuals(model_fit),
    X = X_census,
    beta_sample = betas,
    userseed = seed,
    ncores = cores
  )
  
  # back transfromation of previeously transformed responses
  if (!missing(transfy_inv)) {
    bootstrap <- transfy_inv(bootstrap)
  }
  
  # Runs the welfare funcation over the boostrap response variable Y
  if (!missing(welfare.function)) {
    bootstrap <- welfare.function(bootstrap)
  }

  # generation of the possible output
  output_list <- list()
  
  if (any(output == "default" |
          output == "all" | 
          output == "summary"  |
          output == "summary_boot")) {
    summaryboot <- .summaryParC(bootstrap,
                                quantiles = quantiles,
                                nrow = n_obs_census,
                                ncol = n_boot, 
                                ncores = cores)
    colnames(summaryboot) <- c("mean",
                               "var",
                               "sd",
                               paste(quantiles * 100, "%-Quant", sep = ""))
    output_list$summary_boot <- summaryboot
  }
  
  if (any(output == "default" |
          output == "all" | 
          output == "yboot"  |
          output == "yboot_est")) {
    if (any(output == "default" |
            output == "all" | 
            output == "summary"  |
            output == "summary_boot")) {
      output_list$yboot_est <- summaryboot[,1] # use means if available
    } else {
      output_list$yboot_est <- rowMeans(bootstrap) # calculate if not
    }
  }
  
  if (any(output == "default" |
          output == "all" | 
          output == "model_fit"))  {
    output_list$model_fit <- model_fit
  }
  
  if (any(output == "all" | output == "bootsample")) {
    output_list$bootsample <- bootstrap
  }
  
  if (any(output == "all" | output == "survey")) {
    output_list$survey <- surveydata
  }
  
  if (any(output == "all" | output == "census")) {
    output_list$census <- censusdata
  }
  
  if (save_boot == T) {
    fwrite(
      as.data.table(bootstrap),
      paste("BootstrapSampleELLsae-",
            Sys.Date(),  ".csv", sep = "")
    )
  }
  
  return(output_list)
}
