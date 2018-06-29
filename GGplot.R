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