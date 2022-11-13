#' Plot Economic Data
#' @description Quickly plot economic data with recession bars
#' @param plot_type Type of plot to create. Options are line, dual line, tri line, bar, and bar line
#' @param df Dataframe containing economic data
#' @param x x axis
#' @param y1 first y axis
#' @param y2 Optional 2nd y axis
#' @param y3 Optional 3rd y axis
#' @param labels.list List containing chart labs
#' @param theme_list Optional List containing theme options
#' @param stat Optional bar parameter
#' @param bar.position Optional bar parameter
#' @param fill.value Optional bar parameter
#' @param ycolor1 Optional first y color
#' @param ycolor2 Optional second y color
#' @param ycolor 3 Optional third y color
#' @param date.break # Optional date break ie '1 month'
#' @param date.format Optional date format ie '%y'
#' @return df dataframe with transformations applied
#' @examples eco.plot(plot_type = 'line', df, x='date', y1= value.UNRATE)


eco.plot <-function(plot_type, df,x,y1,y2,y3,labels.list, theme.list, stat, bar.position, fill.value, ycolor1, ycolor2, ycolor3, date.break, date.format){
  # Set Default Values
  plot_type <-ifelse(missing(plot_type),'line',tolower(plot_type))
  fill.value <-ifelse(missing(fill.value),'NULL', fill.value)

  # Define color palette in future
  ycolor1 <- ifelse(missing(ycolor1),"#447099",ycolor1)
  ycolor2 <-ifelse(missing(ycolor2),"#EE6331",ycolor2)
  ycolor3 <-ifelse(missing(ycolor3),"#404041",ycolor3)

  date.break <- ifelse(missing(date.break),'1 year', date.break)
  date.format <- ifelse(missing(date.format),'%y', date.format)

  # Define Theme
  theme_eco_moon <-function(){
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.position = 'top',
      legend.title = element_blank(),
      legend.text = element_text(size = 15),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.caption = element_text(size = 10),
      plot.title = element_text(size = 20, hjust = .5)
    )
  }

  theme.list <-ifelse(missing(theme.list), theme_eco_moon, theme.list)

  # Define Labels
  label_eco_moon <-list(
    title = ''
  )

  labels.list <-ifelse(missing(labels.list),label_eco_moon, labels.list)

  # Create plot plot
  base_plot <-ggplot()+
    theme_list +
    labs(labels.list)

# Define Graph Function List
    line_list = function(df, xaxis, yaxis){
      geom_list <-list(geom = 'line', data = df,aes = list(x = {{xaxis}}, y = {{yaxis}}))
      return(geom_list)
    }


    if(fill.value == 'NULL'){
      bar_list = function(df, xaxis, yaxis, stat, bar.position, fill.value=NULL){
        # Define default bar parameters
        bar.position <- ifelse(missing(bar.position),'dodge', bar.position) # GGPLOT 2 Options ie(dodge, stack,fill)
        stat <-ifelse(missing(stat), 'identity', stat)
        geom_list <-list(geom = 'bar',position =bar.position ,data = df,aes = list(x = {{xaxis}}, y = {{yaxis}}))
        return(geom_list)

    }
      }else{

    bar_list = function(df, xaxis, yaxis, stat, bar.position){
      # Define default bar parameters
      bar.position <- ifelse(missing(bar.position),'dodge', bar.position) # GGPLOT 2 Options ie(dodge, stack,fill)
      stat <-ifelse(missing(stat), 'identity', stat)

      geom_list <-list(geom = 'bar',position = bar.position ,data = df,aes = list(x = {{xaxis}}, y = {{yaxis}}, fill = {{fill.value}}))
      return(geom_list)
      }
    }

# Create economic graphs based on the plot type
  if(plot_type == 'line'){
    eco_plot <-base_plot +
      geom_from_list(line_list(df, x, y1)) +
      scale_color_manual(labels=c(y1),values=c(ycolor1))

  } else if(plot_type == 'dual line'){
    eco_plot <-base_plot +
      geom_from_list(line_list(df, x, y1)) +
      geom_from_list(line_list(df, x, y2)) +
      scale_color_manual(values = c(ycolor1, ycolor2),labels = c(y1,y2))

  } else if(plot_type == 'tri line'){
    eco_plot <-base_plot +
      geom_from_list(line_list(df, x, y1)) +
      geom_from_list(line_list(df, x, y2)) +
      geom_from_list(line_list(df, x, y3)) +
      scale_color_manual(values = c(ycolor1, ycolor2,ycolor3),labels = c(y1,y2,y3))

  } else if(plot_type == 'bar'){
    eco_plot <- base_plot +
      geom_from_list(bar_list(df, x, y1, fill.value, bar.position)) +
      scale_fill_manual(values=c(ycolor1, ycolor2, ycolor3,"#419599","#72994E"))

  } else if(plot_type == 'bar line'){
    eco_plot <-base_plot +
      geom_from_list(bar_list(df, x, y1, fill.value, bar.position))
      geom_from_list(line_list(df, x, y2)) +
      scale_color_manual(labels=c(y2),values=c(ycolor2))
  }

  if(inherits(x, 'Date')==TRUE){
    eco_plot +
      scale_x_date(date_breaks = date.break,labels = date_format(date.format))
  }

  # Adds recession bars to chart
  if("recession" %in% colnames(df)){

    if(head(df[['recession']],1)== 1){
      start <-df$date[head(df[['recession']],1)]
    } else {
      start <-df$date[which(diff(df[['recession']])==1)]
    }

    end <-df$date[which(diff(df[['recession']])==-1)]

    if (length(end)>length(start)){
      end <-end[-1]
      recession.df <-data.frame(start = start, end = end)
      #recession.df <- subset(recession.df, start>=min(df[['recession']]))
    } else {
      recession.df <-data.frame(start = start, end = end)
      #recession.df <- subset(recession.df, start>=min(df[['recession']]))
    }

    eco_plot <-eco_plot +
      geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=.3,color='grey80')
    return(eco_plot)

  } else {
    return(eco_plot)
  }

}
