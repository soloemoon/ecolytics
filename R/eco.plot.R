#' Plot Economic Data
#'
#' @description Quickly plot economic data with recession bars
#' @param plot.type Type of plot to create. Options are line, dual line, tri line, bar, and bar line
#' @param df Dataframe containing economic data
#' @param x x axis
#' @param y1 first y axis
#' @param y2 Optional 2nd y axis
#' @param y3 Optional 3rd y axis
#' @param labels.list List containing chart labs
#' @param stat Optional bar parameter
#' @param bar.position Optional bar parameter
#' @param fill.value Optional bar parameter
#' @param ycolor1 Optional first y color
#' @param ycolor2 Optional second y color
#' @param ycolor3 Optional third y color
#' @param date.break # Optional date break ie '1 month'
#' @param date.format Optional date format
#' @examples
#' \dontrun{ eco.plot(plot.type = 'line', df, x='date', y1= 'value.UNRATE')}
#' @export

eco.plot <-
  function(plot.type,
           df,
           x,
           y1,
           y2,
           y3,
           labels.list,
           stat.value,
           bar.position,
           fill.value,
           ycolor1,
           ycolor2,
           ycolor3,
           date.break,
           date.format) {
    # Set Default Values
    plot.type <- ifelse(missing(plot.type), 'line', tolower(plot.type))
    fill.value <- ifelse(missing(fill.value), 'NULL', fill.value)
    bar.position <-ifelse(missing(bar.position), 'dodge', bar.position) # GGPLOT 2 Options ie(dodge, stack,fill)
    stat.value <- ifelse(missing(stat.value), 'identity', stat.value)
    
    # Define color palette in future
    ycolor1 <- ifelse(missing(ycolor1), "#447099", ycolor1)
    ycolor2 <- ifelse(missing(ycolor2), "#EE6331", ycolor2)
    ycolor3 <- ifelse(missing(ycolor3), "#404041", ycolor3)
    
    date.break <- ifelse(missing(date.break), '1 year', date.break)
    date.format <- ifelse(missing(date.format), '%y', date.format)
    
    # Create plot plot
    base_plot <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = 'top',
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 13),
        axis.text.x = ggplot2::element_text(size = 12),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        plot.caption = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(size = 17, hjust = .5),
        plot.subtitle = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank() 
      ) +
      do.call(labs, labels.list)
    
    # Define Graph Function List
    line_list = function(df, xaxis, yaxis, clr) {
      geom_list <-geom_line(aes(df[[xaxis]], y=df[[yaxis]], color = clr))
      return(geom_list)
    }
    
    if (fill.value == 'NULL') {
     
      bar_list = function(df, xaxis, yaxis,  stat, bar.position, fill.value = NULL) {
          geom_list <-geom_bar(aes(x=df[[xaxis]], y=df[[yaxis]]), position=bar.position, stat=stat.value)
        return(geom_list)
      }
    } else {
      bar_list = function(df, xaxis, yaxis, stat, bar.position) {
        geom_list <-geom_bar(aes(x=df[[axis]], y=df[[yaxis]], fill = df[[fill.value]]), position = bar.position, stat=stat.value)
        return(geom_list)
      }
    }
    
    # Create economic graphs based on the plot type
    if (plot.type == 'line') {
      eco_plot <- base_plot +
        line_list(df, x, y1, ycolor1) +
        scale_color_manual(labels= y1, values= ycolor1)

    } else if (plot.type == 'dual line') {
      eco_plot <- base_plot +
        line_list(df, x, y1, ycolor1) +
        line_list(df, x, y2, ycolor2) +
        scale_color_manual(labels= c(y1, y2), values = c(ycolor1, ycolor2))
      
    } else if (plot.type == 'tri line') {
      eco_plot <- base_plot +
        line_list(df, x, y1, ycolor1) +
        line_list(df, x, y2, ycolor2) +
        line_list(df, x, y3, ycolor3) +
        scale_color_manual(labels = c(y1, y2, y3), values = c(ycolor1, ycolor2, ycolor3))
      
    } else if (plot.type == 'bar') {
      eco_plot <- base_plot +
        bar_list(df, x, y1, fill.value, bar.position, stat.value) +
        ggplot2::scale_fill_manual(values = c(ycolor1, ycolor2, ycolor3, "#419599", "#72994E"))
      
    } else if (plot.type == 'bar line') {
      eco_plot <- base_plot +
        bar_list(df, x, y1, fill.value, bar.position, stat.value) +
        line_list(df, x, y2, ycolor2)
    }
    
    if (inherits(x, 'Date') == TRUE) {
      eco_plot +
        ggplot2::scale_x_date(date_breaks = date.break, labels = date_format(date.format))
    }
    
    # Adds recession bars to chart
    if ("recession" %in% colnames(df)) {
      if (head(df[['recession']], 1) == 1) {
        start <- df$date[head(df[['recession']], 1)]
      } else {
        start <- df$date[which(diff(df[['recession']]) == 1)]
      }
      
      end <- df$date[which(diff(df[['recession']]) == -1)]
      
      if (length(end) > length(start)) {
        end <- end[-1]
        recession.df <- data.frame(start = start, end = end)
        #recession.df <- subset(recession.df, start>=min(df[['recession']]))
      } else {
        recession.df <- data.frame(start = start, end = end)
        #recession.df <- subset(recession.df, start>=min(df[['recession']]))
      }
      
      eco_plot <- eco_plot +
        ggplot2::geom_rect(
          data = recession.df,
          aes(
            xmin = start,
            xmax = end,
            ymin = -Inf,
            ymax = +Inf
          ),
          alpha = .3,
          color = 'grey80'
        )
      return(eco_plot)
      
    } else {
      return(eco_plot)
    }
  }
