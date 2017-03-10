library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)

# 1) Shooting distance stat.
shot_dist_data = read.csv('data/shot_distance_stats.csv')
rating_data = read.csv('data/off_rating_stat.csv')
rating_data['WIN.'] = rating_data['W'] / (rating_data['W'] + rating_data['L'])
rating_data['pf_or_not'] = 'No'

## 1.1) Teams got into playoff
for (season in levels(rating_data$Season)) {
  playoff_teams = 
    subset(shot_dist_data, Season.Type == 'POFF' & Season == season)
  
  rating_data[
    rating_data$Team %in% playoff_teams$Team & 
      rating_data$Season == season, ]['pf_or_not'] = 'Yes'
}

reg_pf_teams = rating_data[rating_data$pf_or_not == 'Yes', ]
reg_non_teams = rating_data[rating_data$pf_or_not == 'No' & 
                              rating_data$Season.Type == 'REG', ]

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

## 1.2) Multivariate Linear Regression for Offensive Rating
model_x = data.frame(rating_data$Less.than.8ft..usage..,
                     rating_data$X8.16.feet.usage..,
                     rating_data$X16.24.feet.usage..,
                     rating_data$X24..feet.usage..)
fit = lm(rating_data$OFFRTG~., data=model_x)



# # 2) More Advanced Plots
# ## 2.3) Plot Shot Comb Ratio vs Offensive Rating
# ggplot(rating_data, aes((X24..feet.usage../Less.than.8ft..usage..), OFFRTG)) + 
#   geom_point() + 
#   geom_smooth(method = "auto", se = TRUE)
# 
# ggplot(rating_data, aes(X24..feet.usage../X8.16.feet.usage.., OFFRTG)) + 
#   geom_point() + 
#   geom_smooth(method = "auto", se = TRUE)
# 
# ggplot(rating_data, aes(X24..feet.usage../X16.24.feet.usage.., OFFRTG)) + 
#   geom_point() + 
#   geom_smooth(method = "auto", se = TRUE)


server <- function(input, output, session) {
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
               c('Playoff Teams', 'Non-playoff Teams'),
               lty=c(1, 1),
               lwd=c(2, 2),
               col=c('red', 'green'))

      })
  output$plot_2 <- renderPlot({
    data = switch(input$distance,
                  '< 8ft' = data.frame(rating_data$Less.than.8ft..usage..),
                  '8-16ft' = data.frame(rating_data$X8.16.feet.usage..),
                  '16-24ft' = data.frame(rating_data$X16.24.feet.usage..),
                  '> 24ft' = data.frame(rating_data$X24..feet.usage..)
    )
    ggplot(rating_data, aes(data, OFFRTG)) +
      geom_point() +
      xlab(input$distance) +
      ggtitle(paste('Offensive Rating vs.', input$distance, ' Shot Usage %')) +
      theme(plot.title = element_text(size=16, lineheight=.8, face="bold", hjust=0.5)) +
      geom_smooth(method = 'glm', se = TRUE)

  })

  output$plot_3 <- renderPlot({
    data = switch(input$distance,
                  '< 8ft' = data.frame(rating_data$Less.than.8ft..usage..),
                  '8-16ft' = data.frame(rating_data$X8.16.feet.usage..),
                  '16-24ft' = data.frame(rating_data$X16.24.feet.usage..),
                  '> 24ft' = data.frame(rating_data$X24..feet.usage..)
    )
    ggplot(rating_data, aes(data, WIN.)) +
      geom_point() +
      xlab(input$distance) +
      ggtitle(paste('Win % vs.', input$distance, ' Shot Usage %')) +
      theme(plot.title = element_text(size=16, lineheight=.8, face="bold", hjust=0.5)) +
      geom_smooth(method = 'glm', se = TRUE)

  })
  observe({
    updateSliderInput(session, "slider1", min=0, max=50, value = 50 - (input$slider2))
  })
  output$slider2 <- renderUI({
    sliderInput("slider2", label="8-16ft Shot Usage (%): ", min=0, max=50, value=50 - (input$slider1))
  })
  observe({
    updateSliderInput(session, "slider3", min=0, max=50, value = 50 - (input$slider4))
  })
  output$slider4 <- renderUI({
    sliderInput("slider4", label="> 24ft Shot Usage (%): ", min=0, max=50, value=50 - (input$slider3))
  })
  output$pred_rtg <- renderText({
    new = data.frame(rating_data.Less.than.8ft..usage.. = input$slider1,
                     rating_data.X8.16.feet.usage.. = input$slider2,
                     rating_data.X16.24.feet.usage.. = input$slider3,
                     rating_data.X24..feet.usage.. = input$slider4)
    paste('Predicted Offensive Rating: ', predict(fit, new))
  })
  ndata <- eventReactive(input$predict, {
    data.frame(rating_data.Less.than.8ft..usage.. = input$slider1,
               rating_data.X8.16.feet.usage.. = input$slider2,
               rating_data.X16.24.feet.usage.. = input$slider3,
               rating_data.X24..feet.usage.. = input$slider4)
  })
  
  ntext <- eventReactive(input$predict, {
    paste('< 8ft shot usage: ', input$slider1,  '%', '\n', 
          '8-16ft shot usage: ', 50-input$slider1, '%', '\n',
          '16-24ft shot usage: ', input$slider3,  '%', '\n',
          '> 24ft shot usage: ', 50-input$slider3, '%')
  })
  output$nText <- renderText({
    ntext()
  })
}





# # Make a counter here.
# values <- reactiveValues(i = 1)
# observe({
#   input$predict
#   isolate({
#     values$i <- rbind(values$i + 1)
#   })
# })
# 
# # output$ <- renderText({
# #   paste0("i = ", values$i)
# # })
# output$pred_plot <- renderPlot({
#   new = data.frame(rating_data.Less.than.8ft..usage.. = input$slider1,
#                    rating_data.X8.16.feet.usage.. = input$slider2,
#                    rating_data.X16.24.feet.usage.. = input$slider3,
#                    rating_data.X24..feet.usage.. = input$slider4)
#   plot(x=values$i, y=predict(fit, new))
# })





# ### 3D Plots.
# fit_1 = lm(data = rating_data, 
#            OFFRTG ~ X24..feet.usage.. + Less.than.8ft..usage..)
# 
# #Setup Axis
# graph_reso <- 0.05
# axis_x <- seq(min(rating_data$Less.than.8ft..usage..), max(rating_data$Less.than.8ft..usage..), by = graph_reso)
# axis_y <- seq(min(rating_data$X24..feet.usage..), max(rating_data$X24..feet.usage..), by = graph_reso)
# 
# #Sample points
# lm_surface <- expand.grid(Less.than.8ft..usage.. = axis_x, 
#                           X24..feet.usage.. = axis_y, 
#                           KEEP.OUT.ATTRS = F)
# lm_surface$OFFRTG <- predict.lm(fit_1, newdata=lm_surface, se=TRUE)
# lm_surface <- acast(lm_surface, Less.than.8ft..usage.. ~ X24..feet.usage.., value.var = "OFFRTG")
# 
# p <- plot_ly(rating_data, 
#              x = ~Less.than.8ft..usage.., 
#              y = ~X24..feet.usage.., 
#              z = ~OFFRTG, 
#              type = 'scatter3d', 
#              mode = 'markers', 
#              marker = list(color = hcolors)) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = '<8ft'),
#                       yaxis = list(title = '>24ft'),
#                       zaxis = list(title = 'Off Rtg.')))
# 
# p %>% add_trace(z = lm_surface, 
#                 type = "surface")
# 
# p_2 <- plot_ly(z = lm_surface, 
#                x = axis_x, 
#                y = axis_y) %>% add_surface()
# p_2
