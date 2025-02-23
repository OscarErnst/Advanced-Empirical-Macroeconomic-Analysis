#---------------------------------------------------
# Function: estimateLP
# Purpose: Estimate Local Projections (LP) impulse responses
# Inputs:
#   - data: Time series data (T x N matrix, where T = time periods, N = variables)
#   - h: Forecast horizon (how many steps ahead to estimate)
#   - p: Number of lags included as regressors
#   - c_case: Determines whether to include constant/trend (0 = none, 1 = constant, 2 = constant + trend)
#   - exdata: Additional exogenous variables (optional)
#   - alpha: Confidence level for confidence intervals (default 90%)
#   - NWSE: Whether to use Newey-West standard errors (default = TRUE)
# Output:
#   - A list containing estimated impulse responses and other relevant matrices
#---------------------------------------------------

estimateLP <- function(data, h, p, c_case, exdata, alpha=90, NWSE=T){
  
  #---------------------------------------------------
  # Step 1: Initialize Parameters & Storage Containers
  #---------------------------------------------------
  
  t <- nrow(data)   # Number of time periods
  n <- ncol(data)   # Number of variables in the system
  
  # Arrays to store impulse response estimates and confidence intervals
  Gamma <- array(NA, dim=c(n,n,h))      # Impulse response estimates
  Gammalo <- array(NA, dim=c(n,n,h))    # Lower bound of confidence intervals
  Gammaup <- array(NA, dim=c(n,n,h))    # Upper bound of confidence intervals
  
  # Compute z-score for confidence intervals based on alpha level
  zscore <- qnorm(1-(1-alpha/100)/2)   # Default 90% → ~1.645
  
  #---------------------------------------------------
  # Step 2: Prepare Dependent (Y) and Regressor (X) Matrices
  #---------------------------------------------------
  
  # Convert input data to a matrix format
  Y <- unname(as.matrix(data))  # Ensures no row/column names interfere with calculations
  
  # Create X matrix: Generate lagged values of Y
  X <- Y
  for (pp in 1:p){
    X <- cbind(X, rbind(matrix(NA, nrow=pp, ncol=n), Y[1:(nrow(data)-pp),]))  # Shift data for lags
  }
  
  #---------------------------------------------------
  # Step 3: Add Constant and/or Trend (if specified by c_case)
  #---------------------------------------------------
  
  if (c_case == 0){   # No constant or trend
    c <- NULL
  } else if (c_case == 1){  # Include only constant
    c <- matrix(1, nrow=nrow(X), ncol=1)
  } else if (c_case == 2){  # Include constant and trend
    c <- cbind(matrix(1, nrow=nrow(X), ncol=1), matrix(1:nrow(X), nrow=nrow(X), ncol=1))
  } else {  # Error handling for incorrect input
    print('c_case variable needs to be set to 0, 1, or 2.')
  }
  
  #---------------------------------------------------
  # Step 4: Add Exogenous Variables (Optional)
  #---------------------------------------------------
  
  Xex <- NULL  # Initialize exogenous variable matrix
  if (!is.null(exdata)){  # If additional exogenous variables are provided
    for (pp in 1:p){  # Lag exogenous variables as well
      Xex <- cbind(Xex, rbind(matrix(NA, nrow=pp, ncol=ncol(Xex)), exdata[1:(nrow(data)-pp),]))
    }
    Xex <- Xex[(p+1):nrow(Xex), ]  # Remove initial NA rows
  }
  
  # Combine regressors: Constant/Trend + Lagged Variables + Exogenous Variables
  X <- cbind(c, X, Xex)  
  
  #---------------------------------------------------
  # Step 5: Local Projection Estimation Loop
  #---------------------------------------------------
  
  print('Progress...')  # Notify user that estimation has started
  counter <- 1  # Initialize progress counter
  pb <- txtProgressBar(min = 0, max = n*h, initial = 0, style=3)  # Progress bar
  
  for (nn in 1:n){  # Loop over each variable (shock variable)
    for (hh in 0:(h-1)){  # Loop over each forecast horizon
      
      #---------------------------------------------------
      # Step 5.1: Define Dependent Variable (LHS)
      #---------------------------------------------------
      
      # Shift Y to get s-step ahead dependent variable
      lhs = as.matrix(c(Y[(hh+1):nrow(Y), nn], rep(NA, hh)), ncol=1)
      
      #---------------------------------------------------
      # Step 5.2: Drop Missing Values and Prepare Regression Data
      #---------------------------------------------------
      
      regdata <- data.frame(cbind(lhs, X))  # Combine dependent and independent variables
      nona <- rowSums(is.na(regdata)) == 0  # Find rows without missing values
      regdata <- regdata[nona, ]  # Keep only rows with complete observations
      
      #---------------------------------------------------
      # Step 5.3: Estimate Regression (OLS)
      #---------------------------------------------------
      
      reg <- lm(X1 ~ 0 + ., data=regdata)  # Estimate OLS without intercept
      
      #---------------------------------------------------
      # Step 5.4: Compute Standard Errors (Newey-West Optional)
      #---------------------------------------------------
      
      if (NWSE){  # If using HAC (Newey-West) standard errors
        vcovNW <- unname(NeweyWest(reg))  # Compute robust variance-covariance matrix
        se <- sqrt(diag(vcovNW))  # Extract standard errors
      } else {  # Otherwise, use standard OLS standard errors
        se <- summary(reg)$coefficients[,2]
      }
      
      #---------------------------------------------------
      # Step 5.5: Store Results (Impulse Response & Confidence Intervals)
      #---------------------------------------------------
      
      # Store estimated impulse response
      Gamma[nn,,hh+1] <- reg$coefficients[(c_case+1):(c_case+n)]
      
      # Compute lower and upper confidence intervals
      Gammalo[nn,,hh+1] <- Gamma[nn,,hh+1] - zscore*se[(c_case+1):(c_case+n)]
      Gammaup[nn,,hh+1] <- Gamma[nn,,hh+1] + zscore*se[(c_case+1):(c_case+n)]
      
      # Update progress bar
      setTxtProgressBar(pb, counter)
      counter <- counter + 1  # Increment counter
      
    }
  }
  
  #---------------------------------------------------
  # Step 6: Return Output as List
  #---------------------------------------------------
  
  return(LP = list('data' = data,      # Original data
                   'exdata' = exdata,  # Exogenous data
                   'X' = X,            # Regressor matrix
                   'Xex' = Xex,        # Exogenous regressor matrix
                   'c_case' = c_case,  # Constant/trend specification
                   'p' = p,            # Number of lags used
                   't' = t,            # Number of observations
                   'n' = n,            # Number of variables
                   'Gamma' = Gamma,    # Estimated impulse response matrix
                   'Gammalo' = Gammalo,# Lower bound of confidence intervals
                   'Gammaup' = Gammaup,# Upper bound of confidence intervals
                   'h' = h))           # Forecast horizon
  
}
