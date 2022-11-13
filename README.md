# ecoMoon: Quickly download, transform, and visualize economic data
Package to download, transform, and visualize economic data. Fred and BLS data downloads are support thanks to the fredr and blsAPI packages, which the eco.download function is a wrapper for. 

## Installation
This package is currently only available from GitHub.

devtools::install_github("soloemoon/ecoMoon")

# Functions Overview

## Download Economic Data

Economic data can be downloaded using the eco.download() function. This function is a fredr and blsAPI wrapper that has added features such as built in data reshape, time series output, and download quickcodes (for BLS). Quarters and years are automatically added to all datasets for quick aggregation. Currently the function downloads all available data. Recession indicators, wide shape, and monthly periodicity are the defaults. Recession indicator periodicity is automatically aligned to the data periodicity.                     

Thanks to the creators of the fredr and blsAPI packages, which this function uses.

```
test_data <-eco.download(keys = c('UNRATE', 'PAYEMS'), show.recession = 'y', periodicty = 'monthly', shape='wide', show.recession='y')

colnames(test_data) <-c('unemployment_rate', 'payrolls')
```
 Dictionary:
  * keys - FRED identifiers or BLS short code identifiers
  * shape -  specifies if the dataframe should be outputted in the long (keys ins rows) or the wide (keys in columns) format. The default is wide.
  * show.recession - determines whether to include or exclude recession indicators (only available for monthly, daily, or quarterly data). Weekly transformation coming soon. Default is to include recession indicator.
  * time.series - determines whether the output of the data will be a dataframe or xts object. Default is 'n' meaning data is outputted as dataframe.
  * periodicity - dictates over what time period the recession indicator is for. Default is monthly. Options include monthly, quarterly, and daily. Weekly periodicity coming soon. It is suggested that only data reflecting the same periodicity be downloaded in a single call.

## Transform Economic Data

Transformations can be applied to an entire economic dataset using the eco.transform function. Transformations are applied only to numeric columns. A best practice would be to only use the eco.transform function in coordination with data pulled using the eco.download function. 

```
# Download data to be transformed

data <-eco.download(keys = c('UNRATE', 'PAYEMS'), key.rename = c('UE','Payrolls'), shape = 'w', show.recession = 'y',time.series = 'n', periodicity = 'monthly') 

# Transform data set
data.trans <-eco.transform(df = data, transformation = '% change', lags = 1)
```
Dictionary:
 
 * df - dataframe or xts containing economic data
  * transformation - type of transformation to be applied to the dataset.
      Transformations include:
        1. % change
        2. difference
        3. log
        4. annualize monthly
        5. annualize quarterly
      (more coming soon)

## Plotting Tools

Clean pre-formatted plots have been included. If the data set being plotted has recession bars present, the plots automatically display recession bars. Plots are based on GGPlot2 and GGTheme.

```
data <-eco.download(keys = c('UNRATE', 'PAYEMS'), key.rename = c('UE','Payrolls'), shape = 'w', show.recession = 'y',time.series = 'n', periodicity = 'monthly') 

eco.plot(plot_type = 'line', df = data, x='date', y = 'Payrolls', y.title = 'Chg', title = 'NFP Change', caption = 'Source: Bureau of Labor Statistics', date.break = '5 year')
```
Included plot types are:
   * single line - plot a single economic data series (line chart)
   * dual line - plot two economic series (linechart)
   * tri line - plot three economic series (line chart)
   * bar - plot economic series as a bar
   * bar line - plot two economic series: one as  a line and the other as a bar.
   
Each plot includes options to adjust the color, titles, caption, subtitle, legend position, date format, line size, and text size.


Thanks to all the developers of packages such as dplyr, purrr, fredr, etc whose hard work made this package possible! More detailed credit will be added.

Features coming soon: 
1. More data sources
2. Additional transformations (logit, indexing, etc)
3. Pre-built batch dataset downloads (initially from BEA and BLS)
