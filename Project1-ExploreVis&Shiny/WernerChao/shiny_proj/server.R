# Compare teams got into playoff vs. didn’t make into playoff:
  # Plot of Stats vs. Season:
    # Seasonal shot distance % for winning team vs. losing team
  # Win% vs. Shot Distance


library(dplyr)
library(ggplot2)
library(reshape2)
shooting_data = read.csv('data/shot_distance_stats.csv')
shooting_data['pf_or_not'] = 'No'


# win_data = read.csv('data/win_stats.csv')
# shooting_data['W'] = 0
# shooting_data['L'] = 0
# shooting_data['WIN.'] = 0
# 
# # Join win/loss from win_data to shooting_data
# for (season in levels(win_data$Season)) {
#   for (team in levels(win_data$TEAM)) {
#     shooting_data[shooting_data$Season == season & shooting_data$Team == team, ]['W'] = 
#       win_data[win_data$Season == season & win_data$TEAM == team, ]['W']
#     
#     shooting_data[shooting_data$Season == season & shooting_data$Team == team, ]['L'] = 
#       win_data[win_data$Season == season & win_data$TEAM == team, ]['L']
#     
#     shooting_data[shooting_data$Season == season & shooting_data$Team == team, ]['WIN.'] = 
#       win_data[win_data$Season == season & win_data$TEAM == team, ]['WIN.']
#   }
# }
# 
# # Remove playoff rows for now.
# shooting_data <- shooting_data[!(shooting_data$Season.Type=="POFF"), ]
# ggplot(shooting_data[shooting_data$Season == '2015-2016' 
#                      | shooting_data$Season == '2014-2015'
#                      | shooting_data$Season == '2013-2014', ], aes(WIN., Less.than.8ft..usage..)) + geom_point()
# 
# ggplot(shooting_data[shooting_data$Season == '2015-2016', ], aes(X8.16.feet.usage.., WIN.)) + geom_point()
# ggplot(shooting_data[shooting_data$Season == '2015-2016', ], aes(WIN., X16.24.feet.usage..)) + geom_point()
# 
# ggplot(shooting_data[shooting_data$Season == '2015-2016' 
#                      | shooting_data$Season == '2014-2015'
#                      | shooting_data$Season == '2013-2014', ], aes(WIN., X24..feet.usage..)) + geom_point()
# 
# ggplot(shooting_data, aes(WIN., Avg..Shot.Dis..ft..)) + geom_point()
# ggplot(shooting_data, aes(WIN., Avg..Made.Shot.Dis..ft..)) + geom_point()



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
                                 summarise(pf_mean_0ft=mean(Less.than.8ft..usage..), 
                                           pf_mean_8ft=mean(X8.16.feet.usage..), 
                                           pf_mean_16ft=mean(X16.24.feet.usage..),
                                           pf_mean_24ft=mean(X24..feet.usage..)), 
                               reg_non_teams %>% 
                                 group_by(Season) %>% 
                                 summarise(non_mean_0ft=mean(Less.than.8ft..usage..), 
                                           non_mean_8ft=mean(X8.16.feet.usage..), 
                                           non_mean_16ft=mean(X16.24.feet.usage..),
                                           non_mean_24ft=mean(X24..feet.usage..))
                               )



shinyServer(function(input, output) {
  output$plot <- renderPlot({
    data = switch(input$distance,
                  '< 8ft' = data.frame(reg_teams_compare$pf_mean_0ft, reg_teams_compare$non_mean_0ft), 
                  '8-16ft' = data.frame(reg_teams_compare$pf_mean_8ft, reg_teams_compare$non_mean_8ft),
                  '16-24ft' = data.frame(reg_teams_compare$pf_mean_16ft, reg_teams_compare$non_mean_16ft),
                  '> 24ft' = data.frame(reg_teams_compare$pf_mean_24ft, reg_teams_compare$non_mean_24ft)
                  )

    plot(c(2005, 2016), c(min(data[, 1])-1.5, max(data[, 1])+1.5), type='n', 
         xlab='Season', ylab=paste(input$distance, ' shot %'), main=paste('Shot % (Distance ', input$distance, ')'))
    lines(seq(2005, 2016), data[, 1], col='red', lwd=2.5)
    lines(seq(2005, 2016), data[, 2], col='green', lwd=2.5)
    legend('bottomright', 
           c('Win Teams', 'Lose Teams'), 
           lty=c(1, 1), 
           lwd=c(2, 2), 
           col=c('red', 'green'))
    
  })

})

