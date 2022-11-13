#' Download clean Fred or BLS Economic data
#'
#' @description fredr and blsAPI wrapper to download and format economic data.
#' @param source Determines API to use: fred or bls. This must be defined.
#' @param keys bls or fred keys. Fred keys can be found on the Fred website. BLS keys have been cleaned to be human readable ie labor_force
#' @param shape Choose 'long' for long format or 'wide' for wide format. Default is wide.
#' @param periodicity Choose data periodicity. Either 'monthly', 'quarterly', 'weekly', or 'daily'. Default is monthly.
#' @param show.recession Choose y to include recession indicator or 'n' to exclude. Default is 'y'.
#' @param time.series Choose 'y' to convert dataframe into a time series (xts) object
#' @return df Cleaned dataframe is returned
#' @import fredr blsAPI dplyr ggplot2 utils reshape2
#' @importFrom plyr colwise
#' @importFrom purrr map_dfr
#' @importFrom xts xts
#' @importFrom stats reshape pnorm
#' @importFrom scales date_breaks date_format
#' @examples
#' \dontrun{eco.download(keys=c('UNRATE','PAYEMS'),  show.recession = 'y', time.series = 'y', periodicity='monthly')}

eco.download <-
  function(source,
           keys,
           shape,
           periodicity,
           show.recession,
           time.series) {
    # Set default values
    keys <- toupper(keys)
    source <- ifelse(missing(source), 'fred', tolower(source))
    periodicity <-
      ifelse(missing(periodicity), 'monthly', tolower(periodicity))
    shape <- ifelse(missing(shape), 'wide', tolower(shape))
    show.recession <-
      ifelse(missing(show.recession), 'y', tolower(show.recession))
    time.series <-
      ifelse(missing(time.series), 'n', tolower(time.series))
    
    # Ensure function inputs have proper classes
    if (inherits(c(keys, shape, show.recession, time.series, periodicity),
                 'character') == FALSE) {
      warning('Entered improper class')
      return(print('Function inputs must be in character format'))
    }
    
    # Determine recession periodicity
    recession.indicators <- list('monthly' = 'USRECM',
                                 'quarterly' = 'USRECQ',
                                 'daily' = 'USRECD')
    
    if (show.recession == 'y') {
      recession.name <-
        unlist(recession.indicators[periodicity], use.names = F)
      keys <- append(keys, recession.name)
    } else{
      keys <- keys
    }
    
    if (source == 'fred') {
      # Download FRED Data
      df <- data.frame(purrr::map_dfr(keys, fredr::fredr))
      df <-
        df[order(df$series_id, as.Date(df$date, format = "%d/%m/%Y")),]
      
    } else if (source == 'bls') {
      bls_data_codes <- list(
        labor_force = 'LNS11000000',
        employment = 'LNS12000000',
        unemployment = 'LNS13000000',
        unemployment_rate = 'LNS14000000',
        nonfarm_employment = 'CES0000000001',
        avg_weekly_hrs = 'CES0500000002',
        avg_weekly_hrs_nonsup = 'CES0500000007',
        avg_hrly_earnings = 'CES0500000003',
        avg_hrly_earnings_nonsup = 'CES0500000008',
        nonfarm_productivity = 'PRS85006092',
        nonfarm_labor_costs = 'PRS85006112',
        nonfarm_hrly_compensation = 'PRS85006152',
        nonfarm_multi_productivity = ' MPU4910012',
        cpi_urban_80s = 'CUUR0000SA0',
        cpi_urban_67 = 'CUUR0000AA0',
        core_cpi_u = 'CUUR0000SA0L1E',
        core_cpi_w = 'CWUR0000SA0L1E',
        ppi_final_sa = 'WPSFD4',
        ppi_final_ua = 'WPUFD4',
        ppi_final_core = 'WPUFD49104',
        ppi_final_core_less_trade = 'WPUFD49116',
        ppi_finshed_goods_82 = 'WPUFD49207',
        imports_all_commodities = 'EIUIR',
        exports_all_commodities = 'EIUIQ',
        employment_cost_index = 'CIU1010000000000A',
        eci_private = 'CIU2010000000000A',
        eci_private_wages = 'CIU2020000000000A'
      )
      # Download dataset
      keys <- unlist(bls_data_codes[keys], use.names = F)
      df <-
        blsAPI::blsAPI(list('seriesid' = c(keys)) , return_data_frame = T)
      df$date <-
        as.Date(lubridate::ydm(paste(df$year, 01, df$periodName)))
      df <- df[c('date', 'seriesID', 'value')]
      colnames(df) <- c('date', 'series_id', 'value')
      
    }
    # Shape dataset
    if (shape == 'wide') {
      key_col_names <- paste0('value.', keys)
      df <-
        stats::reshape(df,
                       idvar = 'date',
                       timevar = 'series_id',
                       direction = 'wide')
      rownames(df) <- df$date
      df <- df[, c('date', key_col_names)]
      
      if (show.recession == 'y') {
        names(df)[names(df) == paste0('value.', recession.name)] <-
          'recession'
      }
      
      # Formats data as xts if desired
      if (time.series ==  'y') {
        df$series_id <- NULL
        df <- xts::xts(x = df, order.by = df$date)
        df$date <- NULL
        storage.mode(df) <- 'numeric'
      }
    } else if (shape == 'long') {
      df
    }
    # Add additional date columns
    df$quarter <-
      paste(quarters(as.Date(df$date)), format(df$date, '%y'), sep = ' ')
    df$year <- format(df$date, '%y')
    return(df)
  }
