library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(cowplot)

# load state temporal data
DEdata <- read_csv('data/de_data.csv')
MDdata <- read_csv('data/md_data.csv')
MNdata <- read_csv('data/mn_data.csv')
NHdata <- read_csv('data/nh_data.csv')
NYdata <- read_csv('data/ny_data.csv')
PAdata <- read_csv('data/pa_data.csv')
VAdata <- read_csv('data/va_data.csv')
VTdata <- read_csv('data/vt_data.csv')
WIdata <- read_csv('data/wi_data.csv')

## run temporal regressions with covariates
DEmodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = DEdata)
step(DEmodel, direction = 'backward')
fDEmodel <- lm(formula = state_lyme ~ temp, data = DEdata)
summary(fDEmodel)

MDmodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = MDdata)
step(MDmodel, direction = 'backward')
fMDmodel <- lm(formula = state_lyme ~ rf, data = DEdata)
summary(fMDmodel)

MNmodel <- lm(lyme ~ gf + rf + coy + temp + rain + year, data = MNdata)
step(MNmodel, direction = 'backward')
fMNmodel <- lm(formula = lyme ~ gf + coy + temp + year, data = MNdata)
summary(fMNmodel)  
       
NHmodel <- lm(state_lyme ~ gf + rf +  coy + temp + rain + year, data = NHdata)
step(NHmodel, direction = 'backward')
fNHmodel <- lm(formula = state_lyme ~ rf +  coy + year, data = NHdata)
summary(fNHmodel)
          
NYmodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = NYdata)
step(NYmodel, direction = 'backward')
fNYmodel <- lm(formula = state_lyme ~ year, data = NYdata)
summary(fNYmodel)

PAmodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = PAdata)
step(PAmodel, direction = 'backward')
fPAmodel <- lm(formula = state_lyme ~ gf + rf + coy + rain + year, data = PAdata)
summary(fPAmodel)

VAmodel <- lm(lyme ~ gf + rf + coy + temp + year, data = VAdata)
step(VAmodel, direction = 'backward')
fVAmodel <- lm(formula = lyme ~ gf + coy + year, data = VAdata)
summary(fVAmodel)

VTmodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = VTdata)
step(VTmodel, direction = 'backward')
fVTmodel <- lm(formula = state_lyme ~ rf + year, data = VTdata)
summary(fVTmodel)

WImodel <- lm(state_lyme ~ gf + rf + coy + temp + rain + year, data = WIdata)
step(WImodel, direction = 'backward')
fWImodel <- lm(formula = state_lyme ~ rf + year, data = WIdata)
summary(fWImodel)

## run temporal regressions without covariates
summary(lm(state_lyme ~ coy, data = DEdata))
summary(lm(state_lyme ~ coy, data = MDdata))
summary(lm(lyme ~ coy, data = MNdata))
summary(lm(state_lyme ~ coy, data = NHdata))
summary(lm(state_lyme ~ coy, data = NYdata))
summary(lm(state_lyme ~ coy, data = PAdata))
summary(lm(lyme ~ coy, data = VAdata))
summary(lm(state_lyme ~ coy, data = VTdata))
summary(lm(state_lyme ~ coy, data = WIdata))

summary(lm(state_lyme ~ gf, data = DEdata))
summary(lm(state_lyme ~ gf, data = MDdata))
summary(lm(lyme ~ gf, data = MNdata))
summary(lm(state_lyme ~ gf, data = NHdata))
summary(lm(state_lyme ~ gf, data = NYdata))
summary(lm(state_lyme ~ gf, data = PAdata))
summary(lm(lyme ~ gf, data = VAdata))
summary(lm(state_lyme ~ gf, data = VTdata))
summary(lm(state_lyme ~ gf, data = WIdata))

summary(lm(state_lyme ~ rf, data = DEdata))
summary(lm(state_lyme ~ rf, data = MDdata))
summary(lm(lyme ~ rf, data = MNdata))
summary(lm(state_lyme ~ rf, data = NHdata))
summary(lm(state_lyme ~ rf, data = NYdata))
summary(lm(state_lyme ~ rf, data = PAdata))
summary(lm(lyme ~ rf, data = VAdata))
summary(lm(state_lyme ~ rf, data = VTdata))
summary(lm(state_lyme ~ rf, data = WIdata))


## make temporal regression figure for appendix
DEcoy <- DEdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("Deleware Lyme disease cases")+
  theme_classic(base_size = 18)

DEgf <- DEdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())

DErf <- DEdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())

MDcoy <- MDdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("Maryland Lyme disease cases")+
  theme_classic(base_size = 18)

MDgf <- MDdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())

MDrf <- MDdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())

MNcoy <- MNdata |>
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("Minnesota Lyme disease cases")+
  theme_classic(base_size = 18)

MNgf <- MNdata |>
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())

MNrf <- MNdata |>
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())




plot_grid(DEcoy, DEgf, DErf, 
          MDcoy, MDgf, MDrf, 
          MNcoy, MNgf, MNrf,ncol = 3)



## NY state data
NYdata <- read.csv('RNYdata.csv')

#NY gray fox regression
NYgfmodel <- lm(state.lyme ~ gf, data = NYdata)
summary(NYgfmodel)

#NY gray fox plot
NYgfplot<-NYdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox
       (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
NYgfplot

#NY red fox regression
NYrfmodel <- lm(state.lyme ~ rf, data = NYdata)
summary(NYrfmodel)

#NY red fox plot
NYrfplot<-NYdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox
       (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
NYrfplot

#NY coyote regression
NYcoymodel <- lm(state.lyme ~ c, data = NYdata)
summary(NYcoymodel)

#NY coyote plot
NYcoyplot<-NYdata%>%
  ggplot(aes(x=c, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote
       (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 18)
NYcoyplot

NYcoyplot <- NYdata %>%
  ggplot(aes(x = c, y = state.lyme)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Coyote (Fur Area * Totals and Sightings per 1000 hours)") + # Adjusted x-axis label
  theme_classic(base_size = 18) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot <- plot_grid(NYgfplot, NYrfplot, NYcoyplot, align='vh', vjust=1, scale = 1)
ggdraw(add_sub(plot, "Label", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

NYgfplot + NYrfplot + NYcoyplot + plot_layout(guides="collect")

##VA state data ###### Something is wrong here
VAdata <- read.csv('RVAdata.csv')

#VA gray fox regression###### Something is wrong here
VAgfmodel <- lm(lyme ~ gf, data = VAdata)
summary(VAgfmodel)

#VA gray fox plot
VAgfplot<-VAdata%>%
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
VAgfplot

#VA red fox regression
VArfmodel <- lm(lyme ~ rf, data = VAdata)
summary(VArfmodel)

#VA red fox plot
VArfplot<-VAdata%>%
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
VArfplot

#VA coyote regression
VAcoymodel <- lm(lyme ~ coy, data = VAdata)
summary(VAcoymodel)

#VA coyote plot
VAcoyplot<-VAdata%>%
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 18)
VAcoyplot

##MN state data 
MNdata <- read.csv('RMNdata.csv')
MNdata <- MNdata%>%
  mutate(year=str_remove(year, ','))

#MN gray fox regression 
MNgfmodel <- lm(lyme ~ gf, data = MNdata)
summary(MNgfmodel)

#MN gray fox plot
MNgfplot<-MNdata%>%
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Minnesota Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
MNgfplot

#MN red fox regression
MNrfmodel <- lm(lyme ~ rf, data = MNdata)
summary(MNrfmodel)

#MN red fox plot *SIGNIFICANT*
MNrfplot<-MNdata%>%
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Minnesota Lyme Cases")+
  annotate("text", x = max(MNdata$rf), y = max(MNdata$lyme), label = "*p < 0.05", hjust = 1, size = 6)+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
MNrfplot

#MN coyote regression
MNcoymodel <- lm(lyme ~ coy, data = MNdata)
summary(MNcoymodel)

#MN coyote plot
MNcoyplot<-MNdata%>%
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Minnesota Lyme Cases")+
  theme_classic(base_size = 18)
MNcoyplot

##NH state data
NHdata <- read.csv('RNHdata.csv')

#NH gray fox regression *SIGNIFICANT*
NHgfmodel <- lm(state.lyme ~ gf, data = NHdata)
summary(NHgfmodel)

#NH gray fox plot
NHgfplot<-NHdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  annotate("text", x = 1.7, y = 2000, label = "*p < 0.05", size = 6)+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
NHgfplot

#NH red fox regression
NHrfmodel <- lm(state.lyme ~ rf, data = NHdata)
summary(NHrfmodel)

#NH red fox plot *SIGNIFICANT*
NHrfplot<-NHdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  annotate("text", x = max(NHdata$rf), y = max(NHdata$state.lyme), label = "*p < 0.001", hjust = 1, size = 6)+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
NHrfplot

#NH coyote regression 
NHcoymodel <- lm(state.lyme ~ c, data = NHdata)
summary(NHcoymodel)
#year significant

#NH coyote plot
NHcoyplot<-NHdata%>%
  ggplot(aes(x=c, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
NHcoyplot

##VT state data
VTdata <- read.csv('RVTdata.csv')

#VT gray fox regression
VTgfmodel <- lm(state.lyme ~ gf, data = VTdata)
summary(VTgfmodel)

#VT gray fox plot
VTgfplot<-VTdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
VTgfplot

#VT red fox regression *SIGNIFICANT*
VTrfmodel <- lm(state.lyme ~ rf, data = VTdata)
summary(VTrfmodel)

#VT red fox plot 
VTrfplot<-VTdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate("text", x = 5.5, y = 1000, label = "*p < 0.05", size = 6)
VTrfplot

#VT coyote regression
VTcoymodel <- lm(state.lyme ~ coy, data = VTdata)
summary(VTcoymodel)

#VT coyote plot
VTcoyplot<-VTdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
VTcoyplot

##WI state data
WIdata <- read.csv('RWIdata.csv')

#WI gray fox regression
WIgfmodel <- lm(state.lyme ~ gf.cpue, data = WIdata)
summary(WIgfmodel)

#WI gray fox plot
WIgfplot<-WIdata%>%
  ggplot(aes(x=gf.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank())
WIgfplot

#WI red fox regression *SIGNIFICANT*
WIrfmodel <- lm(state.lyme ~ rf.cpue, data = WIdata)
summary(WIrfmodel)

#WI red fox plot 
WIrfplot<-WIdata%>%
  ggplot(aes(x=rf.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 18)+
  annotate("text", x = 16, y = 4000, label = "*p < 0.05", size = 6)+
  theme(axis.title.y = element_blank())
WIrfplot

#WI coyote regression
WIcoymodel <- lm(state.lyme ~ coyote.cpue, data = WIdata)
summary(WIcoymodel)
#year significant

#WI coyote plot
WIcoyplot<-WIdata%>%
  ggplot(aes(x=coyote.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 18)
WIcoyplot

##PA state data
PAdata <- read.csv('RPAdata.csv')

#PA gray fox regression
PAgfmodel <- lm(state.lyme ~ gf, data = PAdata)
summary(PAgfmodel)

#PA gray fox plot
PAgfplot<-PAdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
PAgfplot

#PA red fox regression 
PArfmodel <- lm(state.lyme ~ rf, data = PAdata)
summary(PArfmodel)

#PA red fox plot 
PArfplot<-PAdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
PArfplot

#PA coyote regression
PAcoymodel <- lm(state.lyme ~ coy, data = PAdata)
summary(PAcoymodel)

#PA coyote plot
PAcoyplot<-PAdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
PAcoyplot

##DE state data
DEdata <- read.csv('RDEdata.csv')

#DE gray fox regression
DEgfmodel <- lm(state.lyme ~ gf, data = DEdata)
summary(DEgfmodel)

#DE gray fox plot
DEgfplot<-DEdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
DEgfplot

#DE red fox regression 
DErfmodel <- lm(state.lyme ~ rf, data = DEdata)
summary(DErfmodel)

#DE red fox plot 
DErfplot<-DEdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
DErfplot

#DE coyote regression
DEcoymodel <- lm(state.lyme ~ coy, data = DEdata)
summary(DEcoymodel)

#DE coyote plot
DEcoyplot<-DEdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
DEcoyplot

##MD state data
MDdata <- read.csv('RMDdata.csv')

#MD gray fox regression
MDgfmodel <- lm(state.lyme ~ gf, data = MDdata)
summary(MDgfmodel)

#MD gray fox plot
MDgfplot<-MDdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Maryland Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
MDgfplot

#MD red fox regression 
MDrfmodel <- lm(state.lyme ~ rf, data = MDdata)
summary(MDrfmodel)

#MD red fox plot 
MDrfplot<-MDdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Maryland Lyme Cases")+
  annotate("text", x = 120, y = max(MDdata$state.lyme), label = "~p = 0.06507", size = 6)+
  theme_classic(base_size = 18)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
MDrfplot

#MD coyote regression
MDcoymodel <- lm(state.lyme ~ coy, data = MDdata)
summary(MDcoymodel)

#MD coyote plot
MDcoyplot<-MDdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Maryland Lyme Cases")+
  theme_classic(base_size = 18)+
  theme(axis.title.x = element_blank())
MDcoyplot

plots_list <- list(
  c(DEcoyplot, DEgfplot, DErfplot),
  c(MDcoyplot, MDgfplot, MDrfplot),
  c(MNcoyplot, MNgfplot, MNrfplot),
  c(NHcoyplot, NHgfplot, NHrfplot),
  c(PAcoyplot, PAgfplot, PArfplot),
  c(VTcoyplot, VTgfplot, VTrfplot),
  c(WIcoyplot, WIgfplot, WIrfplot),
  c(NYcoyplot, NYgfplot, NYrfplot),
  c(VAcoyplot, VAgfplot, VArfplot)
)

# Create the grid layout
grid_layout <- lapply(1:9, function(i) {
  plots_list[((i - 1) * 3 + 1):(i * 3)]
})

combined_plots <- wrap_plots(grid_layout, ncol = 3)

# Display the combined plots
combined_plots


##############################################Covariates included
## NY state data
NYdata <- read.csv('RNYdata.csv')

#NY gray fox regression
eNYgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = NYdata)
summary(eNYgfmodel)
#year significant

#NY gray fox plot
NYgfplot<-NYdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 14)
NYgfplot

#NY red fox regression
eNYrfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = NYdata)
summary(eNYrfmodel)
#year significant

#NY red fox plot
NYrfplot<-NYdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 14)
NYrfplot

#NY coyote regression
eNYcoymodel <- lm(state.lyme ~ c + year + rain + temp, data = NYdata)
summary(eNYcoymodel)
#year significant

#NY coyote plot
NYcoyplot<-NYdata%>%
  ggplot(aes(x=c, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (Fur Area * Totals and Sighting/1000 hours)")+
  ylab("New York Lyme Cases")+
  theme_classic(base_size = 14)
NYcoyplot

##VA state data
VAdata <- read.csv('RVAdata.csv')

#VA gray fox regression
eVAgfmodel <- lm(lyme ~ gf + year + rain + temp, data = VAdata)
summary(eVAgfmodel)
#year, rain, and temp significant

#VA gray fox plot
VAgfplot<-VAdata%>%
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 14)
VAgfplot

#VA red fox regression
eVArfmodel <- lm(lyme ~ rf + year + rain + temp, data = VAdata)
summary(eVArfmodel)

#VA red fox plot
VArfplot<-VAdata%>%
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 14)
VArfplot

#VA coyote regression 
eVAcoymodel <- lm(lyme ~ coy + year + rain + temp, data = VAdata)
summary(eVAcoymodel)

#VA coyote plot
VAcoyplot<-VAdata%>%
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (individuals hunted)")+
  ylab("Virginia Lyme Cases")+
  theme_classic(base_size = 14)
VAcoyplot

##MN state data 
MNdata <- read.csv('RMNdata.csv')

#MN gray fox regression
eMNgfmodel <- lm(lyme ~ gf + year + rain + temp, data = MNdata)
summary(eMNgfmodel)

#MN gray fox plot
MNgfplot<-MNdata%>%
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Minnesota Lyme Cases")+
  theme_classic(base_size = 14)
MNgfplot

#MN red fox regression 
eMNrfmodel <- lm(lyme ~ rf + year + rain + temp, data = MNdata)
summary(eMNrfmodel)

#MN red fox plot *SIGNIFICANT*
MNrfplot<-MNdata%>%
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Minnesota Lyme Cases")+
  theme_classic(base_size = 14)
MNrfplot

#MN coyote regression 
eMNcoymodel <- lm(lyme ~ coy + year + rain + temp, data = MNdata)
summary(eMNcoymodel)

#MN coyote plot
MNcoyplot<-MNdata%>%
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Minnesota Lyme Cases")+
  theme_classic(base_size = 14)
MNcoyplot

##NH state data
NHdata <- read.csv('RNHdata.csv')

#NH gray fox regression
eNHgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = NHdata)
summary(eNHgfmodel)
#year significant

#NH gray fox plot
NHgfplot<-NHdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  theme_classic(base_size = 14)
NHgfplot

#NH red fox regression
eNHrfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = NHdata)
summary(eNHrfmodel)
#year significant

#NH red fox plot *SIGNIFICANT*
NHrfplot<-NHdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  theme_classic(base_size = 14)
NHrfplot

#NH coyote regression 
eNHcoymodel <- lm(state.lyme ~ c + year + rain + temp, data = NHdata)
summary(eNHcoymodel)
#year significant

#NH coyote plot
NHcoyplot<-NHdata%>%
  ggplot(aes(x=c, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("New Hampshire Lyme Cases")+
  theme_classic(base_size = 14)
NHcoyplot

##VT state data
VTdata <- read.csv('RVTdata.csv')

#VT gray fox regression
eVTgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = VTdata)
summary(eVTgfmodel)
#year significant

#VT gray fox plot
VTgfplot<-VTdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 14)
VTgfplot

#VT red fox regression *SIGNIFICANT*
eVTrfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = VTdata)
summary(eVTrfmodel)
#year significant

#VT red fox plot 
VTrfplot<-VTdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 14)
VTrfplot

#VT coyote regression
eVTcoymodel <- lm(state.lyme ~ coy + year + rain + temp, data = VTdata)
summary(eVTcoymodel)
#year significant

#VT coyote plot
VTcoyplot<-VTdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Vermont Lyme Cases")+
  theme_classic(base_size = 14)
VTcoyplot

##WI state data
WIdata <- read.csv('RWIdata.csv')

#WI gray fox regression
eWIgfmodel <- lm(state.lyme ~ gf.cpue + year + rain + temp, data = WIdata)
summary(eWIgfmodel)
#year significant

#WI gray fox plot
WIgfplot<-WIdata%>%
  ggplot(aes(x=gf.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 14)
WIgfplot

#WI red fox regression *SIGNIFICANT*
eWIrfmodel <- lm(state.lyme ~ rf.cpue + year + rain + temp, data = WIdata)
summary(eWIrfmodel)
#year significant

#WI red fox plot 
WIrfplot<-WIdata%>%
  ggplot(aes(x=rf.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 14)
WIrfplot

#WI coyote regression
eWIcoymodel <- lm(state.lyme ~ coyote.cpue + year + rain + temp, data = WIdata)
summary(eWIcoymodel)
#year significant

#WI coyote plot
WIcoyplot<-WIdata%>%
  ggplot(aes(x=coyote.cpue, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Wisconsin Lyme Cases")+
  theme_classic(base_size = 14)
WIcoyplot

##PA state data
PAdata <- read.csv('RPAdata.csv')

#PA gray fox regression
ePAgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = PAdata)
summary(ePAgfmodel)
#year, gf, and rain are significant

#PA gray fox plot
PAgfplot<-PAdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 14)
PAgfplot

#PA red fox regression 
ePArfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = PAdata)
summary(ePArfmodel)
#year significant

#PA red fox plot 
PArfplot<-PAdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 14)
PArfplot

#PA coyote regression
ePAcoymodel <- lm(state.lyme ~ coy + year + rain + temp, data = PAdata)
summary(ePAcoymodel)
#year p=0.0628

#PA coyote plot
PAcoyplot<-PAdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Pennsylvania Lyme Cases")+
  theme_classic(base_size = 14)
PAcoyplot

##DE state data
DEdata <- read.csv('RDEdata.csv')

#DE gray fox regression
eDEgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = DEdata)
summary(eDEgfmodel)

#DE gray fox plot
DEgfplot<-DEdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 14)
DEgfplot

#DE red fox regression 
eDErfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = DEdata)
summary(eDErfmodel)

#DE red fox plot 
DErfplot<-DEdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 14)
DErfplot

#DE coyote regression
eDEcoymodel <- lm(state.lyme ~ coy + year + rain + temp, data = DEdata)
summary(eDEcoymodel)

#DE coyote plot
DEcoyplot<-DEdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Deleware Lyme Cases")+
  theme_classic(base_size = 14)
DEcoyplot

##MD state data
MDdata <- read.csv('RMDdata.csv')

#MD gray fox regression
eMDgfmodel <- lm(state.lyme ~ gf + year + rain + temp, data = MDdata)
summary(eMDgfmodel)

#MD gray fox plot
MDgfplot<-MDdata%>%
  ggplot(aes(x=gf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Gray Fox (cpue)")+
  ylab("Maryland Lyme Cases")+
  theme_classic(base_size = 14)
MDgfplot

#MD red fox regression 
eMDrfmodel <- lm(state.lyme ~ rf + year + rain + temp, data = MDdata)
summary(eMDrfmodel)

#MD red fox plot 
MDrfplot<-MDdata%>%
  ggplot(aes(x=rf, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Red Fox (cpue)")+
  ylab("Maryland Lyme Cases")+
  theme_classic(base_size = 14)
MDrfplot

#MD coyote regression
eMDcoymodel <- lm(state.lyme ~ coy + year + rain + temp, data = MDdata)
summary(eMDcoymodel)

#MD coyote plot
MDcoyplot<-MDdata%>%
  ggplot(aes(x=coy, y=state.lyme))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("Coyote (cpue)")+
  ylab("Maryland Lyme Cases")+
  theme_classic(base_size = 14)
MDcoyplot
########