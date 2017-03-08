# Compare teams got into playoff vs. didn’t make into playoff:
  # Plot of Stats vs. Season:
    # Do they take more shots > 8 ft.?
    # Do they make more shots > 8 ft.?
    # What is their avg. shot dist.?
    # What is their avg. made shot dist.?

library(dplyr)
library(ggplot2)
library(reshape2)
shooting_data = read.csv('../data/shot_distance_stats.csv')
shooting_data['pf_or_not'] = 'No'

# Teams got into playoff
for (season in levels(shooting_data$Season)) {
  playoff_teams = 
    subset(shooting_data, Season.Type == 'POFF' & Season == season)
  
  shooting_data[
    shooting_data$Team %in% playoff_teams$Team & 
      shooting_data$Season.Type == 'REG' & 
      shooting_data$Season == season, ]['pf_or_not'] = 'Yes'
}
reg_pf_teams = shooting_data[shooting_data$pf_or_not == 'Yes', ]
reg_non_teams = shooting_data[shooting_data$pf_or_not == 'No' & 
                                shooting_data$Season.Type == 'REG', ]

reg_teams_compare = data.frame(reg_pf_teams %>% 
                                 group_by(Season) %>% 
                                 summarise(mean_8ft=mean(Less.than.8ft..usage..)), 
                               reg_non_teams %>% 
                                 group_by(Season) %>% 
                                 summarise(mean_8ft=mean(Less.than.8ft..usage..))
                               )
head(reg_teams_compare)
  
plot(reg_teams_compare$mean_8ft, type='line', ylim=c(39.0, 44.0), 
     col='red', xlab='Season', ylab='< 8 ft shot %')
lines(reg_teams_compare$mean_8ft.1, col='green')









shinyServer(function(input, output) {
  output$plot <- renderPlot({
    plot(c(2005, 2016), c(37.0, 44.0), type='n', ylim=c(39.0, 44.0), 
         xlab='Season', ylab='< 8 ft shot %', main='Shot Distance % (< 8 ft)')
    lines(seq(2005, 2016), reg_teams_compare$mean_8ft, col='red', lwd=2.5)
    lines(seq(2005, 2016), reg_teams_compare$mean_8ft.1, col='green', lwd=2.5)
    legend('bottomright', 
           c('Win Teams', 'Lose Teams'), 
           lty=c(1, 1), 
           lwd=c(2, 2), 
           col=c('red', 'green'))
    
    
    # teams_15_16 = shooting_data[
    #   shooting_data$Season.Type == 'REG' & 
    #     shooting_data$Season == '2015-2016', ]
    # ggplot(data=teams_15_16, aes(pf_or_not, mean(Less.than.8ft..usage..))) + geom_point()
    # ggplot(data=teams_15_16, aes(pf_or_not, X8.16.feet.usage..)) + geom_point()
    # ggplot(data=teams_15_16, aes(pf_or_not, X16.24.feet.usage..)) + geom_point()
    # ggplot(data=teams_15_16, aes(pf_or_not, Avg..Shot.Dis..ft..)) + geom_point()
    # ggplot(data=teams_15_16, aes(pf_or_not, Avg..Made.Shot.Dis..ft..)) + geom_point()
    # ggplot(data=teams_15_16, aes(pf_or_not, Avg..Missed.Shot.Dis..ft..)) + geom_point()
  })

})

