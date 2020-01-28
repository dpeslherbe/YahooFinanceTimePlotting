library(quantmod)
library(ggplot2)
library(dplyr)


YahooFinanceTimePlot <- function(Stockname, Stocksearch, startdate, enddate, interestvariable1,
                         interestvariable2, undertitle=NULL, xlegend=NULL, ylegend=NULL){
getSymbols(Stockname, src = 'yahoo', from = '1900-01-01', env = .GlobalEnv)
Financialinfo <- function(Stocksearch, startdate, enddate, interestvariable1, interestvariable2){
  dataf <<- data.frame(date = index(Stocksearch), coredata(Stocksearch))
  colnames(dataf) <<- c('date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  if(missing(startdate)){
    if(missing(enddate)){
    }
    else{
      dataf <<- subset.data.frame(dataf, date <= enddate)
    }
  }
  else{
    if(missing(enddate)){
      dataf <<- subset.data.frame(dataf, date >= startdate)
    }
    else{
      dataf <<- subset.data.frame(dataf, date >= startdate & date <= enddate)
    }
  }
  graph <<- ggplot()+
    geom_line(data = dataf, aes(x = date, y = dataf[, interestvariable1]),
              color = "#FC4E07", size =3)
  if(missing(interestvariable2)){
    return(graph)
  }
  else{
    graph <<- graph +
      geom_line(data = dataf, aes(x = date, y = dataf[, interestvariable2]*(max(dataf[, interestvariable1])/max(dataf[, interestvariable2]))),
                color = "#00AFBB", size =3)+
      scale_y_continuous(sec.axis = sec_axis(~.*max(dataf[, interestvariable2])/max(dataf[, interestvariable1])))
  }
  return(graph)
}

Customizegraph <- function(Stockname, undertitle=NULL, xlegend=NULL, ylegend=NULL){
  graph <<- graph +
    labs(title = c(Stockname), subtitle = c(undertitle), x = c(xlegend), y = c(ylegend))
  return(graph)
}
Financialinfo(Stocksearch, startdate, enddate, interestvariable1, interestvariable2)
Customizegraph(Stockname, undertitle, xlegend, ylegend)
}
