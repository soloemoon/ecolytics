#' Recession Probability Based on Treasury Spread
#' @description Compute and visualize the most recent recession probability
#' @param start.date Date to begin analysis
#' @return graph
#' @examples
#' recession.probability('2000-01-01')


recession.probability <-function(start.date){

  start.date <-ifelse(missing(start.date),'2000-01-01',start.date)

    RP <-eco.download(keys = 'T10Y3MM') %>%
          na.omit() %>%
          filter(date > as.Date(start.date)) %>%
          mutate(recession_probability = round(pnorm(-0.5333 - 0.6330 * value.T10Y3MM) * 100, 2))

    if(sum(RP[['recession']]) !=0){

      start <- if(head(RP[['recession']],1)== 1){
        RP$date[head(RP[['recession']],1)]
      }else{
        RP$date[which(diff(RP[['recession']])==1)]
      }

      end <-RP$date[which(diff(RP[['recession']])==-1)]

      if (length(end)>length(start)){
        end <-end[-1]
        recession.df <-data.frame(start = start, end = end)
        recession.df <- subset(recession.df, start>=min(RP[['recession']]))
      } else {
        recession.df <-data.frame(start = start, end = end)
        recession.df <- subset(recession.df, start>=min(RP[['recession']]))
      }
     }

      recession_chart <-ggplot()+
        geom_line(aes(x=RP$date, y=RP$recession_probability,colour = '#014d64'),size=.7)+
        labs(subtitle='Based on 3 Month Treasury Spread', y='%',x='',title='Recession Probability',caption='Source: Treasury Model')+
        scale_x_date(date_breaks = '1 year',labels = date_format('%y'))+
        scale_color_manual(values = c('#014d64'), labels = c('12M Recession Probability')) +
        geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=.3,color='grey80')+
        theme_economist_white(gray_bg = FALSE)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title = element_text(size=15),
              plot.caption = element_text(size=13),
              legend.text = element_text(size = 15))


      return(recession_chart)
}
