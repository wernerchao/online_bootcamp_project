# Compare teams got into playoff vs. didn’t make into playoff:
  # Plot of Stats vs. Season:
    # Do they take more shots > 8 ft.?
    # Do they make more shots > 8 ft.?
    # What is their avg. shot dist.?
    # What is their avg. made shot dist.?

library(dplyr)
library(ggplot2)
shooting_data = read.csv('../data/shot_distance_stats.csv')
shooting_data['pf_or_not'] = 'No'

# Teams got into playoff
playoff_teams = 
  subset(shooting_data, Season.Type == 'POFF' & Season == '2015-2016')

shooting_data[
  shooting_data$Team %in% playoff_teams$Team & 
    shooting_data$Season.Type == 'REG' & 
    shooting_data$Season == '2015-2016', ]['pf_or_not'] = 'Yes'

teams_15_16 = shooting_data[
    shooting_data$Season.Type == 'REG' & 
    shooting_data$Season == '2015-2016', ]
colnames(teams_15_16)


shinyServer(function(input, output) {
  output$plot <- renderPlot({
    ggplot(data=teams_15_16, aes(pf_or_not, Less.than.8ft..usage..)) + geom_point()
    ggplot(data=teams_15_16, aes(pf_or_not, X8.16.feet.usage..)) + geom_point()
    ggplot(data=teams_15_16, aes(pf_or_not, X16.24.feet.usage..)) + geom_point()
    ggplot(data=teams_15_16, aes(pf_or_not, Avg..Shot.Dis..ft..)) + geom_point()
    ggplot(data=teams_15_16, aes(pf_or_not, Avg..Made.Shot.Dis..ft..)) + geom_point()
    ggplot(data=teams_15_16, aes(pf_or_not, Avg..Missed.Shot.Dis..ft..)) + geom_point()
  })

})

