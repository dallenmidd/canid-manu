library(readr)
library(dplyr)
library(ggplot2)

### NH Analysis
{
  NHspace <- read_csv("data_spatial/nh_spatial.csv", skip = 2)
  
  NHgfspace <- lm(lyme ~ gf, data = NHspace)
  summary(NHgfspace)
  
  NHgfspace.plot<-NHgfspace%>%
    ggplot(aes(x=gf, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Gray Fox CPUE")+
    ylab("") +
    theme_classic(base_size = 14)
  NHgfspace.plot
  
  NHrfspace <- lm(lyme ~ rf, data = NHspace)
  summary(NHrfspace)
  
  NHrfspace.plot<-NHrfspace%>%
    ggplot(aes(x=rf, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Red Fox CPUE")+
    ylab("") +
    theme_classic(base_size = 14) 
   NHrfspace.plot
  
  NHcoyspace <- lm(lyme ~ coy, data = NHspace)
  summary(NHcoyspace)
  
  NHcoyspace.plot<-NHcoyspace%>%
    ggplot(aes(x=coy, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Coyote CPUE")+
    ylab("Lyme disease cases
         (per 100,000)")+
    theme_classic(base_size = 14)
  NHcoyspace.plot
  

  
}

### NY analysis
{
  
  NYspace <- read_csv("data_spatial/ny_spatial.csv", skip = 2)
  
  
  NYgfspace <- lm(lyme ~ gf, data = NYspace)
  summary(NYgfspace)
  
  NYgfspace.plot<-NYgfspace%>%
    ggplot(aes(x=gf, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Gray fox sightings/1000 hours")+
    ylab("") +
    theme_classic(base_size = 14)
  NYgfspace.plot
  
  NYrfspace <- lm(lyme ~ rf, data = NYspace)
  summary(NYrfspace)
  
  NYrfspace.plot<-NYrfspace%>%
    ggplot(aes(x=rf, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Red fox sightings/1000 hours")+
    ylab("") +
    theme_classic(base_size = 14)
  NYrfspace.plot
  
  NYcoyspace <- lm(lyme ~ coy, data = NYspace)
  summary(NYcoyspace)
  
  NYcoyspace.plot<-NYcoyspace%>%
    ggplot(aes(x=coy, y=lyme))+
    geom_point()+
    geom_smooth(method=lm, se = F, color = 'black')+
    xlab("Coyote sightings/1000 hours")+
    ylab("Lyme disease cases
         (per 100,000)")+
    theme_classic(base_size = 14)
  NYcoyspace.plot
}

{
  library(cowplot)
  spatial_result_figure <- plot_grid(NHcoyspace.plot,NHrfspace.plot,NHgfspace.plot,
            NYcoyspace.plot,NYrfspace.plot,NYgfspace.plot,
            ncol = 3,
            labels = c('A','','','B','',''))
  
  ggsave('figures/figure2.pdf', spatial_result_figure, width = 10, height = 6, units = 'in')
  ggsave('figures/figure2.png', spatial_result_figure, width = 10, height = 6, units = 'in')
}

