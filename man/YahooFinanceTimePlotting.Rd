\name{YahooFinanceTimePlot}
\alias{YahooFinanceTimePlot}
\title{
YahooFinanceTimePlot{values}
}
\description{
%%  ~~ Create a plot through ggplot() of values of a specified Stock between certain dates (if specified),
%%      or stock lifetime ~~
}
\usage{
YahooFinanceTimePlot(Stockname, Stocksearch, startdate, enddate, interestvariable1,
                     interestvariable2, undertitle = NULL, xlegend = NULL, ylegend = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Stockname \Name of stock desired to plot (Written as 'Stock')~~
%%     ~~Stocksearch \Name of stock desired to plot (Written as Stock)~~
%%     ~~startdate \Start date for stock data and plot - optional (Written as 'YYYY-MM-DD')~~
%%     ~~enddate \End date for stock data and plot -optional (Written as 'YYYY-MM-DD')~~
%%     ~~interestvariable1 \Variable of interest to be plotted (Written as 'Variable')~~
%%     ~~interestvariable2 \Second variable of interest to be plotted -optional (Written as 'Variable')~~
%%     ~~undertitle \Subtitle for plot -optional (Written as 'undertitle')~~
%%     ~~xlegend \Axis title for x -optional (Written as 'xlegend')~~
%%     ~~ylegend \Axis title for y -optional (Written as 'ylegend')~~
}
}
\details{
%%  ~~ Uses packages quantmod, ggplot, & dplyr ~~
%%  ~~ List of possible variables: 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted' ~~
}
\value{
%%  \item{Stocksearch }{Xts observation of all available Stock data}
%%  \item{dataf }{Data frame of desired dates -if specified}
%%  \item{graph }{Time plot of desired variables on desired dates -if specified}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~Inspired by Data retrieval from quantmod by Jeffrey A. Rian.
%%  ~Code written by D Peslherbe
}
\note{
%%  ~~Please ignore Deprecation warning, fix will be included in later update~~
}

\seealso{
%% package quantmod
}
\examples{
%%##Get GOOG (Alphabet Inc. Class C) Stock Closing & Volume Values from Jan 1st 2020 to Jan 26th 2020
%%YahooFinanceTimePlot(Stockname = 'GOOG', Stocksearch =  GOOG, startdate = '2020-01-01', enddate = '2020-01-26',
%%             interestvariable1 = 'Close', interestvariable2 = 'Volume',
%%             undertitle = 'Yahoo Data from Jan 1st to Jan 26th 2020',
%%             xlegend = 'Date', ylegend = 'Stock Closing Value (Orange) & Volume (Blue)')
%%
%%##Get AAPL (Apple Inc.) Stock High & Volume Values from Jan 1st 2009 to December 31st 2019
%%YahooFinanceTimePlot(Stockname = 'AAPL', Stocksearch = AAPL, startdate = '2009-01-01', enddate = '2019-12-31',
%%             interestvariable1 = 'High', interestvariable2 = 'Volume',
%%             undertitle = NULL, xlegend = 'Date', ylegend = 'Stock High Value (Orange) & Volume (Blue)')
%%
%%##Get MSFT (Microsoft Corporation) Stock Opening & Closing Values for all time data available
%%YahooFinanceTimePlot('MSFT', MSFT, interestvariable1 = 'Open', interestvariable2 = 'Close',
%%             undertitle = 'Opening (Orange) & Closing (Blue) Stock Values for MSFT Stock',
%%             xlegend = NULL, ylegend = NULL)
%%
%%##Get BMO (Bank of Montreal) Stock Adjusted Values for all time data available until December 31st 2015
%%YahooFinanceTimePlot('BMO', BMO, enddate = '2015-12-31', interestvariable1 = 'Adjusted',
%%             undertitle = 'Adjusted Stock Value until Dec 31st 2015',
%%             xlegend = 'Date', ylegend = 'Adjusted Stock Value')
}
