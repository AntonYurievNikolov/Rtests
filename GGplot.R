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