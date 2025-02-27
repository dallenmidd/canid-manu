library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(cowplot)

# load state temporal data
DEdata <- read_csv('data_temporal/de_data.csv')
MDdata <- read_csv('data_temporal/md_data.csv')
MNdata <- read_csv('data_temporal/mn_data.csv')
NHdata <- read_csv('data_temporal/nh_data.csv')
NYdata <- read_csv('data_temporal/ny_data.csv')
PAdata <- read_csv('data_temporal/pa_data.csv')
VAdata <- read_csv('data_temporal/va_data.csv')
VTdata <- read_csv('data_temporal/vt_data.csv')
WIdata <- read_csv('data_temporal/wi_data.csv')

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
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("Delaware cases")+
  theme_classic(base_size = 12)

DEgf <- DEdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

DErf <- DEdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

MDcoy <- MDdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("Maryland cases")+
  theme_classic(base_size = 12)

MDgf <- MDdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

MDrf <- MDdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

MNcoy <- MNdata |>
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("Minnesota cases")+
  theme_classic(base_size = 12)

MNgf <- MNdata |>
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

MNrf <- MNdata |>
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

NHcoy <- NHdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Coyote CPUE")+
  ylab("New Hampshire cases")+
  theme_classic(base_size = 12)

NHgf <- NHdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Gray fox CPUE")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

NHrf <- NHdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Red fox CPUE")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

PAcoy <- PAdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("Pennsylvania cases")+
  theme_classic(base_size = 12)

PAgf <- PAdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

PArf <- PAdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

VTcoy <- VTdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("Vermont cases")+
  theme_classic(base_size = 12)

VTgf <- VTdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

VTrf <- VTdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

WIcoy <- WIdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Coyote CPUE")+
  ylab("Wisconsin cases")+
  theme_classic(base_size = 12)

WIgf <- WIdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Gray fox CPUE")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())

WIrf <- WIdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Red fox CPUE")+
  ylab("")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())



figS1a <- plot_grid(DEcoy, DEgf, DErf, 
          MDcoy, MDgf, MDrf, 
          MNcoy, MNgf, MNrf,
          NHcoy, NHgf, NHrf,
          ncol = 3,
          labels = c('A',rep('',11)))

figS1b <- plot_grid(PAcoy, PAgf, PArf,
          VTcoy, VTgf, VTrf,
          WIcoy, WIgf, WIrf,
          ncol = 3,
          labels = c('B',rep('',8)))


NYcoy <- NYdata |>
  ggplot(aes(x=coy, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Coyotes (per 1000 hrs)")+
  ylab("New York cases")+
  theme_classic(base_size = 12)
NYgf <- NYdata |>
  ggplot(aes(x=gf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Gray foxes (per 1000 hrs)")+
  theme_classic(base_size = 12) +
  theme(axis.title.y = element_blank())
NYrf <- NYdata |>
  ggplot(aes(x=rf, y=state_lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Red foxes (per 1000 hrs)")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())


VAcoy <- VAdata |>
  ggplot(aes(x=coy, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Coyotes (inds. hunted)")+
  ylab("Virginiaa cases")+
  theme_classic(base_size = 12) +
  scale_x_continuous(breaks = c(15000,22500,30000))
VAgf <- VAdata |>
  ggplot(aes(x=gf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Gray foxes (inds. hunted)")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(breaks = c(10000,17500,25000))
VArf <- VAdata |>
  ggplot(aes(x=rf, y=lyme))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = 'black')+
  xlab("Red foxes (inds. hunted)")+
  theme_classic(base_size = 12)+
  theme(axis.title.y = element_blank())



figS1c <- plot_grid(NYcoy, NYgf, NYrf,
          VAcoy, VAgf, VArf,
          ncol = 3,
          labels = c('C',rep('',5)))

ggsave('figures/figureS1a.pdf', figS1a, width = 7, height = 9, units = 'in')
ggsave('figures/figureS1b.pdf', figS1b, width = 7, height = 7, units = 'in')
ggsave('figures/figureS1c.pdf', figS1c, width = 7, height = 5, units = 'in')

