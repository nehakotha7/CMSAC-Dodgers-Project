library(tidyverse)
library(Lahman) 
yearly_batting <- Batting |>
  filter(lgID %in% c("AL", "NL")) |>
  group_by(yearID) |>
  summarize(total_h = sum(H, na.rm = TRUE),
            total_hr = sum(HR, na.rm = TRUE),
            total_so = sum(SO, na.rm = TRUE),
            total_bb = sum(BB, na.rm = TRUE),
            total_ab = sum(AB, na.rm = TRUE)) |>
  mutate(batting_avg = total_h / total_ab)

ggplot(data=yearly_batting)

yearly_batting |> 
  ggplot() +
  geom_point(aes(x=yearID, y=total_hr))+ # putting aes inside ggplot() globalizes it
  geom_line(aes(x=yearID, y=total_hr))

yearly_batting |> 
  ggplot(aes(x=yearID, y=total_hr))+
  geom_point(aes(color=total_so, size=total_bb))+
  geom_line(color='green', linetype='dashed', size=.4)+
  scale_color_gradient(low='midnightblue', high='gold')+
  scale_size_continuous(breaks = seq(0,20000,2500))+
  labs(x='Year',
       y='Home runs',
       color='Strikeout',
       size='Walks',
       title='The rise of three true outcomes in baseball',
       subtitle='Bad Boy was here',
       caption='Data via Lahman') +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust=.5, face='bold') # The range is -1, 1
  )


  #scale_y_continuous(breaks = c(0,6000))+ # specifies low & upp bound of break
  # scale_y_continuous(breaks=seq(0,6000,1000)) this has the step
  #scale_x_continuous(limits = c(1990,2005)) +
  #scale_x_reverse()+
  #stat_smooth() #gives smooth regression line through graph
  #geom_smooth(method = 'lm', se=F)

# Previous graphing not simplistic enough
  # Pivot longer makes it tidy and able to graph separate plots
yearly_batting_long <- yearly_batting |> 
  select(yearID,
         HRs=total_hr,
         Strikeouts=total_so,
         Walks=total_bb) |> 
  pivot_longer(c(HRs, Strikeouts, Walks),
               values_to = 'val',
               names_to = 'stat')

yearly_batting_long |> 
  ggplot(aes(x=yearID,y=val))+
  geom_point(color='darkblue',alpha=.7)+
  geom_line()+
  facet_wrap(~stat, nrow = 3, scales='free_y')+
  labs(
    x='Year',
    y='Total of Statistic',
    title='The rise of three true outcomes in baseball',
    caption='Data courtesy of Lahman'
  ) +
  theme_bw()+
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    plot.title = element_text(hjust=.5, face='bold')
  )

babynames |> 
  filter(name=='Gabriel', sex=='M') |> 
  ggplot(aes(year, n))+
  geom_point()+
  geom_vline(xintercept = 2004,
             color='red',
             linetype='dashed')
