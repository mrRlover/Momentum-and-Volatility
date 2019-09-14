#' A loosely-constrained ggplot version of PerformanceAnalytics performance summary charts.
#' This edition uses facet grid and subsetting to simplify layout. 
#' Returns the plot object for further manipulation of legends, layout, guides, etc.
#' Requires `dplyr`, 'tidyr`, `magrittr`, `scales` and `ggplot2`.
#'
#' @example 
#' p <- ggpChartsPerformanceSummary(returns.xts,"My Returns")
#' - to position legend use
#' p <- p + theme(legend.position="bottom")
#' 
#' - to eliminate series guide use
#' p <- p + guides(color=FALSE,fill=FALSE)
#' 
#' - to use direct labels to identify series on cumulative return plot without guides
#' require(directlabels)
#' direct.label(p+guides(color=FALSE,fill=FALSE))
#' 
#' @seealso PerformanceAnalytics::charts.PerformanceSummary
#' @param r.xts an XTS object with period returns for each column; 
#' function will compute cumulative return and drawdowns for each item
#' @param ptitle a plot title string, default empty string
#' @param geometric whether cumulative returns should use geometric method
#' @return ggplot object for further manipulation or display
#' @author mrb
#' 
gg.PerformanceSummary <- function(r.xts, ptitle="", geometric=TRUE) {
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(tidyr))
  suppressPackageStartupMessages(require(magrittr))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  
  # cumulative return  
  c.xts <- if (geometric) {
    cumprod(1+r.xts)
  } else {
    1 + cumsum(r.xts)
  }

  
  # drawdowns
  d.xts <- do.call(cbind, lapply(1:ncol(c.xts), function(j) {
    cx <- cummax(c.xts[, j])
    dd <- c.xts[,j] / cx
  }))
  
  # tagged dataframes to facilitate facet grid subsetting
  
  port_dates <- r.xts %>% index() %>% as.POSIXct()
  
  options(warn = -1)
  
  pc <- data.frame(Date=port_dates, Plot="Cumulative Returns", c.xts)
  pr <- data.frame(Date=port_dates, Plot="Period Returns", r.xts)
  pd <- data.frame(Date=port_dates, Plot="Drawdowns", d.xts)
  pf <- bind_rows(pc,pr,pd) %>% gather(Series, Value, 3:ncol(pr)) %>% na.omit
  
  options(warn = 1)
  
  # facet plot
  p <- ggplot(pf,aes(x=Date, y=Value, color=Series, fill=Series)) +
    geom_line(data=subset(pf, Plot=="Cumulative Returns"), size = 1.2, show.legend = T) +
    geom_bar(data=subset(pf, Plot=="Period Returns"),stat="identity",position="dodge", show.legend = F) +
    geom_smooth(data=subset(pf, Plot=="Period Returns"),method="lm",show.legend = F) +
    geom_line(data=subset(pf,  Plot=="Drawdowns"), size = 1.2,show.legend = F) +
    facet_grid(Plot~., scales="free_y", space="fixed") +
    ggtitle(ptitle) + xlab(NULL) + ylab(NULL)+
    scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
    
    guides(colour = guide_legend(override.aes = list(size=1))) +
    
    theme(axis.text.x = element_text(angle = 60, hjust = 1)
          , legend.title = element_blank()
          , legend.position = c(0,1)
          , legend.justification = c(0,1)
          , legend.background = element_rect(colour = 'grey')
          , legend.key = element_rect(fill = "white", colour = "white")
          , strip.background = element_rect(fill = "grey")
          , panel.background = element_rect(fill = "white", colour = "white")
          , panel.grid.major = element_line(colour = "grey", size = 0.5) 
          , panel.grid.minor = element_line(colour = NA, size = 0.0))
  return(p)
}
