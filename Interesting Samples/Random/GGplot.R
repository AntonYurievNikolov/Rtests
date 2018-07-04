#GGPLOT
#shape = 21 with col ,fill
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,color=continent,size=pop)) + geom_point()+scale_x_log10()+
  facet_wrap(~year)+expand_limits(y = 0)
#lineplot
ggplot(year_continent, aes(x = year, y = meanLifeExp, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

#custom colour
blues <- brewer.pal(9, "Blues")
blue_range <- colorRampPalette(blues)
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11))


#time series
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data  = recess,
            aes(xmin = begin, xmax = end, ymax = +Inf, ymin = -Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()


qplot(
  cyl, wt,
  data = mtcars,
  fill = am,
  geom = "dotplot",
  binaxis = "y",
  stackdir = "center"
)


#Stat summary with errorbar
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean,
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
               position = posn.d, fun.args = list(mult = 1), width = 0.1)


#Heat maps mosaic
GGmosaic<- function(data, X, FILL) {
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # Plot
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xposn, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = yposn, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}