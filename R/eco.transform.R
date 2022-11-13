#' Transform Economic Data
#' @description Quickly transform an entire economic dataset with a variety of transformations
#' @param df Dataframe containing economic data that will be transformed.
#' @param transformation The type of transformation to be applied.
#' @param lags The number of lags to apply
#' @return df dataframe with transformations applied
#' @examples eco.transform(df = data, transformation = 'pct_chg', lags = 1)

eco.transform <-function(df, transformation, lags){

  '%notin%' <-Negate('%in%')
  names(df) <- tolower(names(df))

  # Perform sanity checks
  if(transformation %notin% c('pct_chg', 'difference','log', 'log_diff')){
    warning('Invalid transformation option selected')
    return(print('Invalid Transformation selected'))
  }

  # Set defaults
  lags <-ifelse(missing(lags),1,lags)
  index <-ifelse(missing(index.date),0,index.date)

# Get all numeric columns in dataframe
  numeric.cols <-names(dplyr::select_if(df,is.numeric))

# Non Annualized Growth Rates
  if(transformation == 'pct_change'){

    df[numeric.cols] <-plyr::colwise(function(x){x / lag(x, lags) - 1})(df[numeric.cols])
    return(df)

# Change
  }else if(transformation == 'difference'){

    df[numeric.cols] <-plyr::colwise(function(x){x - lag(x, lags)})(df[numeric.cols])

    return(df)

# Log Transformation
}else if(transformation == 'log'){

  df[numeric.cols] <-log(df[numeric.cols])

  return(df)

# Monthly Annualized % Change
  } else if(transformation == 'log difference'){

  df[numeric.cols] <-log(df[numeric.cols])
  df[numeric.cols] <-plyr::colwise(function(x){x - lag(x, lags)})(df[numeric.cols])

  return(df)

  } else if(transformation == 'sqrt'){
    df[numeric.cols] <-plyr::colwise(function(x){sqrt(x)})(df[numeric.cols])
    return(df)
  }
}
