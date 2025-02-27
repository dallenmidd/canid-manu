library(deSolve)
library(dplyr)
library(ggplot2)

pars <- c(u = 0.2,
          b = 80000,
          rfp = 0,
          gfp = 0,
          coyp = 0,
          Tmt1 = 0.921,
          Tmt2 = 0.465,
          Tmt3 = 0.55,
          Tmt4 = 0.147,
          Tmt5 = 0.013,
          Tmt6 = 0.026,
          r1 = 16.65,
          r2 = 18,
          r3 = 7.5,
          r4 = 8,
          r5 = 4,
          r6 = 16,
          K1 = 10000,
          K2 = 24000,
          K3 = 5000,
          K4 = 5000,
          K5 = 68,
          K6 = 250,
          v = 500000,
          p_rf_wfm = 0.01825,
          p_gf_wfm = 0.04108,
          p_coy_wfm = 0.06388,
          p_rf_sh = 0.00366,
          p_gf_sh = 0.00102,
          p_coy_sh = 0.06352,
          p_rf_ch = 0,
          p_gf_ch = 0.00185,
          p_coy_ch = 0,
          p_rf_sq = 0.008148,
          p_gf_sq = 0.00333,
          p_coy_sq = 0.06086,
          p_rf_rac = 0.053,
          p_gf_rac = 0.00675,
          p_coy_rac = 0.0493,
          p_rf_op = 0.041,
          p_gf_op = 0.0031,
          p_coy_op = 0.726
)

pars_og <- pars

full_model <- function(pars, times) {
  # initial state 
  state <- c(Nm = 10000, Nsh = 5000, Nc = 5000, Nsq = 810, Nr = 20, No = 10, Sm = 10000, Ssh = 5000, Sc = 5000, Ssq = 810, Sr = 20, So = 10, Im = 0, Ish = 0, Ic = 0, Isq = 0, Ir = 0, Io = 0, St = 1000, It = 1000, Jt = 0)
  # derivative
  deriv <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_Nm = r1*Nm*(1-Nm/K1) -Nm*(p_rf_wfm*rfp +p_gf_wfm*gfp +p_coy_wfm*coyp)
      d_Nsh = r2*Nsh*(1-Nsh/K2) -Nsh*(p_rf_sh*rfp +p_gf_sh*gfp +p_coy_sh*coyp)
      d_Nc = r3*Nc*(1-Nc/K3) -Nc*(p_rf_ch*rfp + p_gf_ch*gfp + p_coy_ch*coyp)
      d_Nsq = r4*Nsq*(1-Nsq/K4) -Nsq*(p_rf_sq*rfp +p_gf_sq*gfp +p_coy_sq*coyp)
      d_Nr = r5*Nr*(1-Nr/K5) -Nr*(p_rf_rac*rfp +p_gf_rac*gfp +p_coy_rac*coyp)
      d_No = r6*No*(1-No/K6) -No*(p_rf_op*rfp +p_gf_op*gfp +p_coy_op*coyp)
      d_Sm = r1*Nm*(1-Nm/K1) -Tmt1*It*Sm/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Sm/Nm)*Nm*(p_rf_wfm*rfp +p_gf_wfm*gfp +p_coy_wfm*coyp)
      d_Ssh = r2*Nsh*(1-Nsh/K2) -Tmt2*It*Ssh/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Ssh/Nsh)*Nsh*(p_rf_sh*rfp +p_gf_sh*gfp +p_coy_sh*coyp)
      d_Sc = r3*Nc*(1-Nc/K3) -Tmt3*It*Sc/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Sc/Nc)*Nc*(p_rf_ch*rfp + p_gf_ch*gfp + p_coy_ch*coyp)
      d_Ssq = r4*Nsq*(1-Nsq/K4) -Tmt4*It*Ssq/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Ssq/Nsq)*Nsq*(p_rf_sq*rfp +p_gf_sq*gfp +p_coy_sq*coyp)
      d_Sr = r5*Nr*(1-Nr/K5) -Tmt5*It*Sr/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Sr/Nr)*Nr*(p_rf_rac*rfp +p_gf_rac*gfp +p_coy_rac*coyp)
      d_So = r6*No*(1-No/K6) -Tmt6*It*So/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(So/No)*No*(p_rf_op*rfp +p_gf_op*gfp +p_coy_op*coyp)
      d_Im = Tmt1*It*Sm/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Im/Nm)*Nm*(p_rf_wfm*rfp +p_gf_wfm*gfp +p_coy_wfm*coyp)
      d_Ish = Tmt2*It*Ssh/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Ish/Nsh)*Nsh*(p_rf_sh*rfp +p_gf_sh*gfp +p_coy_sh*coyp)
      d_Ic = Tmt3*It*Sc/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Ic/Nc)*Nc*(p_rf_ch*rfp + p_gf_ch*gfp + p_coy_ch*coyp)
      d_Isq = Tmt4*It*Ssq/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Isq/Nsq)*Nsq*(p_rf_sq*rfp +p_gf_sq*gfp +p_coy_sq*coyp)
      d_Ir = Tmt5*It*Sr/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Ir/Nr)*Nr*(p_rf_rac*rfp +p_gf_rac*gfp +p_coy_rac*coyp)
      d_Io = Tmt6*It*So/(b+Nm+Nsh+Nc+Nsq+Nr+No) -(Io/No)*No*(p_rf_op*rfp +p_gf_op*gfp +p_coy_op*coyp)
      d_St = v - St*(Nm+Nsh+Nc+Nsq+Nr+No)/(b+Nm+Nsh+Nc+Nsq+Nr+No) - u*St

      d_It = St*(Tmt1*Im+Tmt2*Ish+Tmt3*Ic+Tmt4*Isq+Tmt5*Ir+Tmt6*Io)/(b+Nm+Nsh+Nc+Nsq+Nr+No) - (Nm+Nsh+Nc+Nsq+Nr+No)/(b+Nm+Nsh+Nc+Nsq+Nr+No)*It - u*It
      d_Jt = (Sm+Ssh+Sc+Ssq+Sr+So)+(((1-Tmt1)*Im)+((1-Tmt2)*Ish)+((1-Tmt3)*Ic)+((1-Tmt4)*Isq)+((1-Tmt5)*Ir)+((1-Tmt6)*Io))/(b+Nm+Nsh+Nc+Nsq+Nr+No)*St-(Nm+Nsh+Nc+Nsq+Nr+No)/(b+Nm+Nsh+Nc+Nsq+Nr+No)*Jt-u*Jt
    
      return(list(c(Nm = d_Nm, Nsh = d_Nsh, Nc = d_Nc, Nsq = d_Nsq, Nr = d_Nr, No = d_No, Sm = d_Sm, Ssh = d_Ssh, Sc = d_Sc, Ssq = d_Ssq, Sr = d_Sr, So = d_So, Im = d_Im, Ish = d_Ish, Ic = d_Ic, Isq = d_Isq, Ir = d_Ir, Io = d_Io, St = d_St, It = d_It, Jt = d_Jt)))
    })
  }
  # solve
  ode(y = state, times = times, func = deriv, parms = pars)
}

# run the model and check results
full_results <- full_model(pars = pars, times = 1:100)
full_results <- as_tibble(full_results)

## plot mice
full_results %>%
  ggplot(aes(time,Sm)) + 
  geom_line() +
  geom_line(aes(time,Im), col = 'red')+
  ylab("Mouse density (individuals per km^2)")+
  xlab("Time (years)")

## plot shrew
full_results %>%
  ggplot(aes(time,Ssh)) + 
  geom_line() +
  geom_line(aes(time,Ish), col = 'red')

## plot chipmunk
full_results %>%
  ggplot(aes(time,Sc)) + 
  geom_line() +
  geom_line(aes(time,Ic), col = 'red')

## plot squirrel
full_results %>%
  ggplot(aes(time,Ssq)) + 
  geom_line() +
  geom_line(aes(time,Isq), col = 'red')

## plot raccoon
full_results %>%
  ggplot(aes(time,Sr)) + 
  geom_line() +
  geom_line(aes(time,Ir), col = 'red')

## plot opossum
full_results %>%
  ggplot(aes(time,So)) + 
  geom_line() +
  geom_line(aes(time,Io), col = 'red')

## plot tick
full_results %>%
  ggplot(aes(time,St)) + 
  geom_line() +
  geom_line(aes(time,It), col = 'red')+
  ylab("Nymph density (individuals per km^2)")+
  xlab("Time (years)")+
  theme_classic(base_size = 14)
## NIP
full_results$It[100] / (full_results$It[100]+full_results$St[100])

#### run model with different predator densities
is.atomic(rf_results)
is.atomic(pars)
is.atomic(full_results)

full_results <- as.data.frame(full_results)

rf_results <- data.frame(rf=seq(from = 0, to = 25, length = 100), DIN = numeric(100), NIP = numeric (100))

pars <- pars_og

for (i in 1:100) {
  pars['rfp'] <- rf_results$rf[i]
  full_results <- full_model(pars = pars, times = 1:100)
  full_results <- as_tibble(full_results)
  rf_results$DIN[i] <- full_results$It[100]
  rf_results$NIP[i] <- full_results$It[100] / (full_results$It[100]+full_results$St[100])
}

xlab <- expression(paste("Red fox density (per km"^2, ')'))
ylab <- expression(paste("DIN (per km"^2, ')'))
NIP_range <- c(0.075,0.26)
DIN_range <- c(75000,310000)


rf_DIN <- rf_results%>%
  ggplot(aes(x=rf, y=DIN))+
  geom_line()+
  xlab(xlab)+
  ylab('')+
  ylim(DIN_range)+
  theme_classic(base_size = 14)
rf_DIN

rf_NIP <- rf_results%>%
  ggplot(aes(x=rf, y=NIP))+
  geom_line()+
  xlab(xlab)+
  ylab("")+
  ylim(NIP_range)+
  theme_classic(base_size = 14)
rf_NIP


gf_results <- data.frame(gf=seq(from = 0, to = 25, length = 100), DIN = numeric(100), NIP = numeric (100))

pars <- pars_og


for (i in 1:100) {
  pars['gfp'] <- gf_results$gf[i]
  full_results <- full_model(pars = pars, times = 1:100)
  full_results <- as_tibble(full_results)
  gf_results$DIN[i] <- full_results$It[100]
  gf_results$NIP[i] <- full_results$It[100] / (full_results$It[100]+full_results$St[100])
}

xlab <- expression(paste("Gray fox density (per km"^2, ')'))
ylab <- expression(paste("DIN (per km"^2, ')'))


gf_DIN <- gf_results%>%
  ggplot(aes(x=gf, y=DIN))+
  geom_line()+
  xlab(xlab)+
  ylab('')+
  ylim(DIN_range)+
  theme_classic(base_size = 14)
gf_DIN

gf_NIP <- gf_results%>%
  ggplot(aes(x=gf, y=NIP))+
  geom_line()+
  xlab(xlab)+
  ylab("")+
  ylim(NIP_range)+
  theme_classic(base_size = 14)
gf_NIP


coy_results <- data.frame(coy=seq(from = 0, to = 25, length = 100), DIN = numeric(100), NIP = numeric (100))

pars <- pars_og


for (i in 1:100) {
  pars['coyp'] <- coy_results$coy[i]
  full_results <- full_model(pars = pars, times = 1:100)
  full_results <- as_tibble(full_results)
  coy_results$DIN[i] <- full_results$It[100]
  coy_results$NIP[i] <- full_results$It[100] / (full_results$It[100]+full_results$St[100])
}

xlab <- expression(paste("Coyote density (per km"^2, ')'))
ylab <- expression(paste("DIN (per km"^2, ')'))


coy_DIN <- coy_results%>%
  ggplot(aes(x=coy, y=DIN))+
  geom_line()+
  xlab(xlab)+
  ylab(ylab)+
  ylim(DIN_range)+
  theme_classic(base_size = 14)
coy_DIN

coy_NIP <- coy_results%>%
  ggplot(aes(x=coy, y=NIP))+
  geom_line()+
  xlab(xlab)+
  ylab('NIP')+
  ylim(NIP_range)+
  theme_classic(base_size = 14)
coy_NIP



library(cowplot)
model_result_fig <- plot_grid(coy_NIP, rf_NIP, gf_NIP,
                     coy_DIN, rf_DIN, gf_DIN,
                     ncol = 3)
  
ggsave('figures/figure3.pdf', model_result_fig, width = 10, height = 6, units = 'in')
ggsave('figures/figure3.png', model_result_fig, width = 10, height = 6, units = 'in')
