## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(viridis)
library(deSolve)
library(scales)
library(lubridate)
library(viridis)
library(readr)
library(readxl)
library(ggrepel)
library(cowplot)
library(viridis)
library(brms)
library(kableExtra)


## ----echo=FALSE, fig.height=2, message=FALSE, warning=FALSE, paged.print=FALSE---------------------------------
ggplot() +
  annotate("text", x = 0.5, y = 0.7,
                   label = "(y_cumulative[i]) %~% italic(N)(mu[i], sigma)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.52,
           label = "italic(mu[i]) == frac(italic(max)*date[i]^omega, theta^omega + date[i]^omega)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.35,
           label = "italic(max) %~% italic(beta)(2.76, 549)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.27,
           label = "italic(theta) %~% italic(Gamma)(4, 0.04)", parse = T, family = "serif") +
   annotate("text", x = 0.5, y = 0.19,
           label = "italic(omega) %~% italic(N)(4, 0.4)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.11,
           label = "sigma %~% italic(HalfCauchy)(0,1)", parse = T, family = "serif") +
  coord_cartesian(ylim = c(0.11, .8)) +
  labs(title = "Weibull non-linear regression:") +
  theme_void() +
  theme(text = element_text(family = "serif"))


## ----include=FALSE, cache = T----------------------------------------------------------------------------------
sd_pop <- 903027 #from here: https://worldpopulationreview.com/states/south-dakota-population/

sd_covid_data <- read.csv(here::here("data/data_kelo.csv")) %>%
  clean_names() %>%
  mutate(date_num = as.numeric(mdy(date)) - 18329,
         date = mdy(date),
         source = "SD_DOH via Keloland news",
         incidence = positive_cases - lag(positive_cases),
         cum_hosp = parse_number(as.character(hospitalized)),
         incidence1 = incidence + 1,
         cum_prop_hosp = cum_hosp/sd_pop)  ##adds 1 to all incidence to allow for zeros in log-linear regression


all_hosp <- sd_covid_data %>% select(date, hospitalized_currently) %>% arrange(date)



#hosp_beds
sd_hospital_beds <- read_excel(here::here("data/sd_hospital_beds.xlsx")) %>% 
  clean_names()

all_beds <- sum(sd_hospital_beds$total_beds)

#nyc data from here: https://www1.nyc.gov/site/doh/covid/covid-19-data.page
nyc_hosp <- nyc_hosp <- read_csv("data/nyc_hosp.csv") %>% clean_names() %>% 
  mutate(cum_hosp = cumsum(hospitalizations),
         nyc_pop = 8900000,
         date = mdy(date_of_interest),
         date_num = as.numeric(date) - 18323,
         cum_prop_hosp = cum_hosp/nyc_pop) %>% 
  mutate(cum_hosp_rate = cum_hosp/max(cum_hosp),
         date_rate = date_num/max(date_num))

max_prop_hosp <- max(nyc_hosp$cum_prop_hosp)
sd_prior_max = sd_pop*max_prop_hosp
sd_prior_maxsd = 2000 


# fit weibull model 

beta_parms <- function(mean, sd) {
  out <- tibble(alpha = (mean^2 - mean^3 - mean*sd^2)/sd^2,
                beta = (mean - 2*mean^2 + mean^3 - sd^2 + mean*sd^2)/sd^2)
  
  plot <- plot(density(rbeta(1000, out[[1]], out[[2]])))
  return(out)
  return(plot)
}

gamma_parms <- function(mean, sd) {
  out <- tibble(shape = mean^2/sd^2,
                rate = mean/sd^2,
                scale = 1/(mean/sd^2))
  return(out)
  return(plot)
}

beta_parms(mean = 0.005, sd = 0.003)
gamma_parms(mean = 150, sd = 50)


fit_weib <- brm(bf(cum_prop_hosp|trunc(lb = 0) ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
                   ult ~ 1, omega ~ 1, theta ~ 1,
                   nl = TRUE),
                data = sd_covid_data,
                # family = gaussian(),
                family = gaussian(),
                prior = c(
                  prior(beta(2.76, 549), nlpar = "ult"), #original_prior
                  prior(gamma(4, 0.04), nlpar = "theta"),
                  prior(normal(4, 0.4), nlpar = "omega")),
                iter = 4000,
                sample_prior = "yes")

# fit_may11 <- fit_weib
# fit_weib <- update(fit_may11, newdata = sd_covid_data, iter = 4000) #re-run the model above with today's data (this shortcut just prevents the need to recompile the model)

# priors come from here:
# ult is the Asymptote: Prior assumes asymptote with mean of 4000 cumulative total people ever hospitalized with wide sd of 2000 - parameterized as beta. That is
# based on NYC's curve, in which a total of 0.45% of their population was ever hospitalized.

# theta is the inflection point - Prior assumes mean of 100 with sd of 30 days - parameterized as gaussian. 
# omega is the slope of inflection point - Prior assumes mean of 50 with sd of 20 - parameterized as gamma.


#predictions up to 200 days after the first hospitalization
newdata = tibble(date_num = 0:200)

predict_cumulative <- predict(fit_weib, newdata = newdata, probs = c(0.025, 0.975, 0.25, 0.75))  %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(date_num = 18329 + newdata$date_num,
         date = as_date(date_num),
         method = "predicted") 



#make predictions of daily hospital needs
days_in_hosp <- 10 #adjusted to visually match actual active hosps

predict_dailyhosp <- predict(fit_weib, newdata = newdata, summary = F)  %>%
  as_tibble() %>%
  mutate(iter = 1:nrow(.)) %>%
  gather(key, value, -iter) %>%
  mutate(date_num = 18328 + parse_number(key),
         date = as_date(date_num)) %>%
  arrange(iter, date_num) %>%
  group_by(iter) %>%
  mutate(daily_total = value - lag(value, days_in_hosp)) %>%
  group_by(date_num) %>%
  drop_na(daily_total) %>%
  mutate(daily_total = daily_total*sd_pop) %>%
  group_by(iter, date) %>% 
  filter(!any(daily_total < 0)) %>% 
  group_by(date) %>% 
  summarize(median = median(daily_total),
            mean = mean(daily_total),
            sd = sd(daily_total),
            high95 = quantile(daily_total, probs = 0.975),
            high50 = quantile(daily_total, probs = 0.75),
            low95 = quantile(daily_total, probs = 0.025),
            low50 = quantile(daily_total, probs = 0.25))

## ----echo=FALSE, fig.height=9, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE--------------------
# Make plots

cumulative_plot <- predict_cumulative %>% 
  ggplot(aes(x = date, y = estimate*sd_pop)) +
  geom_line() +
  geom_ribbon(aes(ymin = q2_5*sd_pop, ymax = q97_5*sd_pop), alpha = 0.2 , fill = "dodgerblue") +
  geom_ribbon(aes(ymin = q25*sd_pop, ymax = q75*sd_pop), alpha = 0.2 , fill = "dodgerblue")+
  geom_point(data = sd_covid_data, aes(x = date, y = cum_prop_hosp*sd_pop),
             shape = 21, fill = "yellow") +
  theme_classic() +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  # ylim(0, 1500) +
  labs(y = "Cumulative hospitalized",
       title = "Weibull model results",
       subtitle = "Dots = data. Shading = 50% and 95% prediction intervals") +
  NULL


active_plot <- predict_dailyhosp %>% 
  ggplot() +
  geom_line(aes(x = date, y = median)) +
  geom_ribbon(aes(x = date, ymin = low95, ymax = high95), alpha = 0.2, fill = "dodgerblue") +
  geom_ribbon(aes(x = date, ymin = low50, ymax = high50), alpha = 0.2, fill = "dodgerblue") +
  geom_point(data = all_hosp, aes(x = date, y = hospitalized_currently),
             shape = 21, fill = "yellow") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  theme_classic() +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Active hospitalizations",
       title = "Predicting active hospitalizations from Weibull model",
       subtitle = "Dots = data. Shading = 50% and 95% prediction intervals") +
  coord_cartesian(ylim = c(0,100)) +
  NULL


both_plot_weib <- plot_grid(cumulative_plot, active_plot, ncol = 1)

both_plot_weib


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------
#prior v post

posts <- posterior_samples(fit_weib) %>% as_tibble() %>% mutate(iter = 1:nrow(.))

x = seq(1,200, by = 10)

priors <- posts %>% select(prior_b_ult, prior_b_omega, prior_b_theta, iter) %>% sample_n(1000) %>% 
  expand_grid(x) %>% 
  mutate(pred = (prior_b_ult*x^prior_b_omega)/(prior_b_theta^prior_b_omega + x^prior_b_omega),
         model = 'prior_prediction') %>% 
  select(x, pred, model)

posteriors <- posts %>% select(b_ult_Intercept, b_omega_Intercept, b_theta_Intercept) %>% sample_n(1000) %>% 
  expand_grid(x) %>% 
  mutate(pred = (b_ult_Intercept*x^b_omega_Intercept)/(b_theta_Intercept^b_omega_Intercept + x^b_omega_Intercept),
         model = 'posterior_prediction') %>% 
  select(x, pred, model)


all_posts <- bind_rows(priors, posteriors) %>% 
  as_tibble() %>% 
  mutate(date_num = 18329 + x,
         date = as_date(date_num))

all_posts %>% 
  group_by(date, model) %>% 
  summarize(mean = mean(pred),
            low95 = quantile(pred, probs = 0.025),
            high95 = quantile(pred, probs = 0.975)) %>% 
  ggplot(aes(x = date, y = mean*sd_pop, ymin = low95*sd_pop, ymax = high95*sd_pop,
             color = model, fill = model)) +
  geom_line() + 
  geom_ribbon(alpha = 0.2) +
  theme_classic() +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  # ylim(0, 1500) +
  labs(y = "Cumulative hospitalized",
       x = "",
       title = "Prior versus Posterior",
       subtitle = "Difference in models indicates the influence of the data") +
  NULL


## ----echo=FALSE, fig.height=1, message=TRUE, warning=TRUE, paged.print=TRUE------------------------------------
ggplot() + 
annotate("text", x=.5, y=0.5, label="R0 == 1  +  frac(italic(r),italic(b))", parse=TRUE, family = "serif") +
  theme_void() +
  labs(title = "R0 formula") +
  theme(text = element_text(family = "serif"))


## ----echo=FALSE, fig.height=2, message=TRUE, warning=TRUE, paged.print=TRUE------------------------------------
library(tidyverse)
ggplot() +
  annotate("text", x = 0.5, y = 0.6,
                   label = "italic(log)(y[i]) %~% italic(N)(mu[i], sigma)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.55,
           label = "italic(mu[i]) == italic(alpha) + italic(beta)*italic(x)[i]", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.5,
           label = "italic(alpha) %~% italic(N)(0,1)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.45,
           label = "italic(beta) %~% italic(N)(0,1)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.4,
           label = "sigma %~% italic(HalfCauchy)(0,1)", parse = T, family = "serif") +
  coord_cartesian(ylim = c(0.4, .62)) +
  labs(title = "Regression formula:") +
  theme_void()


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------
#Load current SD covid data and plot it
sd_covid_data <- read.csv(here::here("data/data_kelo.csv")) %>% 
  clean_names() %>% 
  mutate(date_num = as.numeric(mdy(date)) - 18330,
         date = mdy(date),
         source = "SD_DOH via Keloland news",
         incidence = positive_cases - lag(positive_cases),
         hospitalized = parse_number(as.character(hospitalized)),
         incidence1 = incidence + 1)  ##adds 1 to all incidence to allow for zeros in log-linear regression



## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------

#create dataset of early incidence (up to May 1)
sd_covid_data_short <- read.csv(here::here("data/data_kelo.csv")) %>% 
  clean_names() %>% 
  mutate(date_num = as.numeric(mdy(date)) - 18330,
         date = mdy(date),
         source = "SD_DOH via Keloland news",
         incidence = positive_cases - lag(positive_cases),
         hospitalized = parse_number(as.character(hospitalized)),
         incidence1 = incidence + 1) %>%   ##adds 1 to all incidence to allow for zeros in log-linear regression
filter(date_num <= 55)

# #Run linear regression
lin_reg <- brm(log(incidence1) ~ date_num, data = sd_covid_data_short,
               family = gaussian(),
               prior = c(prior(normal(0,1), class = "Intercept"),
                         prior(normal(0,0.5), class = "b"),
                         prior(cauchy(0,1), class = "sigma")),
               cores = 4, sample_prior = "yes")
#

#
saveRDS(lin_reg, file = here::here("outputs/lin_reg.rds"))
lin_reg <- readRDS(here::here("outputs/lin_reg.rds"))
posts <- posterior_samples(lin_reg)
mean_r <- round(mean(posts$b_date_num), 2)
sd_r <- round(sd(posts$b_date_num),2)



## ----echo=FALSE, fig.height=4, fig.width=4, message=FALSE, warning=FALSE, paged.print=FALSE, cache = T---------
fit_mod <- fitted(lin_reg) %>%
  as_tibble() %>%
  clean_names() %>%
  rowid_to_column() %>%
  mutate(date = as_date(rowid + 18330) )


plot_r <- fit_mod %>%
  ggplot(aes(x = date, y = estimate-1)) +
  geom_point(data = sd_covid_data %>% filter(date_num > 0), aes(x = date, y = log(incidence1)-1)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, aes(ymin = q2_5-1, ymax = q97_5-1)) +
  labs(y = "log(incidence)",
       x = NULL,
       title = "Linear fit of log incidence over time",
       subtitle = bquote('slope'~italic("r = ")~.(mean_r)~"\u00B1"~.(sd_r)~'(mean \u00B1 sd)'))  +
  theme_classic()
# 
ggsave(plot_r, file = here::here("plots/plot_r.jpg"), width = 6, height = 6)
# saveRDS(plot_r, file = "plots/plot_r.rds")
# readRDS(here::here("plots/plot_r.rds"))
plot_r


## ----echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------
#extract slope mean and CI and estimate R0 from equation above
shape_gt = 6^2/2^2 #fit by eye to represent range of reported generation times in Park et al. 2020
rate_gt = 6/2^2 #fit by eye to represent range of reported generation times in Park et al. 2020

# plot(density(rgamma(1000, shape = shape_gt, rate = rate_gt)))

posts <- as_tibble(posterior_samples(lin_reg)) %>% 
  rename(r = b_date_num) %>% 
  select(r) %>% 
  mutate(gt_days = runif(nrow(.), 4,8)) %>% 
  mutate(R0 = 1 + gt_days*r)

r0_mean_sd <- posts %>% 
  summarize(mean = round(mean(R0),2),
            sd = round(sd(R0),2))

kable_styling(kable(r0_mean_sd, caption = "Table 1. R0 mean and standard deviation sampled from to fit the SIR model.", 
      format = "pandoc"),full_width = F) %>%
   column_spec(column = 1:2, width = "2in") %>%
   kable_styling(c("bordered", "condensed"), full_width = F)


## ----echo=FALSE, fig.height=2, message=TRUE, warning=TRUE, paged.print=TRUE------------------------------------
ggplot() +
  annotate("text", x = 0.5, y = 0.6,
           label = "frac(italic(dS), italic(dt)) ==  frac(italic(beta)*S*I,N)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.55,
           label = "frac(italic(dI), italic(dt))  ==  frac(italic(beta)*S*I,N) - gamma*I", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.5,
           label = "frac(italic(dR), italic(dt)) == gamma*I", parse = T, family = "serif") +
  coord_cartesian(ylim = c(0.48, .62)) +
  labs(title = "SIR formula:") +
  theme_void() +
  theme(text = element_text(family = "serif"))


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------
#SIR model
closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  S <- x[1]
  I <- x[2]
  R <- x[3]
  ## now extract the parameters
  beta <- params["beta"] 
  gamma <- params["gamma"] 
  N <- S+I+R
  ## now code the model equations
  dSdt <- -beta*S*I/N
  dIdt <- beta*S*I/N-gamma*I
  dRdt <- gamma*I
  ## combine results into a single vector
  dxdt <- c(dSdt,dIdt,dRdt)
  ## return result as a list!
  list(dxdt)
}

#parameters inits
num_days_infect = 7
gamma = 1/num_days_infect
parms <- c(beta=beta,gamma= gamma)
times <- seq(from = 0, to = 200, by = 1)
xstart <- c(S = 0.99999, I = 0.000001, R = 0.000009)   # assumes almost no intitial immunity

N <- 1000
r0sims4 <- posts %>% 
  select(R0) %>% 
  sample_n(N) %>% pull()

out4 <- list()
for (i in 1:N) {
  out4[[i]] <- ode(
    func=closed.sir.model,
    y=xstart,
    times=times,
    parms= c(beta=gamma*r0sims4[i],gamma = gamma)) %>%
    as.data.frame() %>% 
    gather(variable, value, -time) %>% 
    mutate(R0 = r0sims4[i])
  out_noimmune4 <- out4
}


m_test <- bind_rows(out_noimmune4) %>% 
  mutate(iter = rep(1:N, each = 603),
         date = as_date(time + as.numeric(ymd("2020-02-24"))))

test_sims <- m_test %>% 
  group_by(date, variable) %>% 
  mutate(number = value*884235) %>% 
  summarize(mean = median(number),
            lci95 = quantile(number, probs = 0.025),
            lci50 = quantile(number, probs = 0.25),
            lci25 = quantile(number, probs = 0.345),
            uci25 = quantile(number, probs = 0.655),
            uci50 = quantile(number, probs = 0.75),
            uci95 = quantile(number, probs = 0.975))

#get parameters of hosp, icu, and vent rate to enter into a beta distibution

hosp_rate_mean = 0.01
hosp_rate_sd = 0.005
alpha_beta = (hosp_rate_mean^2 - hosp_rate_mean^3 - hosp_rate_mean*(hosp_rate_sd^2))/hosp_rate_sd^2
beta_beta = (hosp_rate_mean - 2*hosp_rate_mean^2 + hosp_rate_mean^3 - hosp_rate_sd^2 + hosp_rate_mean*hosp_rate_sd^2)/hosp_rate_sd^2


icu_rate_mean = 0.0015
icu_rate_sd = 0.0005
alpha_beta_icu = (icu_rate_mean^2 - icu_rate_mean^3 - icu_rate_mean*(icu_rate_sd^2))/icu_rate_sd^2
beta_beta_icu = (icu_rate_mean - 2*icu_rate_mean^2 + icu_rate_mean^3 - icu_rate_sd^2 + icu_rate_mean*icu_rate_sd^2)/icu_rate_sd^2


vent_rate_mean = 0.00084
vent_rate_sd = 0.0001
alpha_beta_vent = (vent_rate_mean^2 - vent_rate_mean^3 - vent_rate_mean*(vent_rate_sd^2))/vent_rate_sd^2
beta_beta_vent = (vent_rate_mean - 2*vent_rate_mean^2 + vent_rate_mean^3 - vent_rate_sd^2 + vent_rate_mean*vent_rate_sd^2)/vent_rate_sd^2

# all_hosp <- as_tibble(m_test) %>% 
#   filter(variable != "I") %>%
#   pivot_wider(names_from = variable, values_from = value) %>% 
#   group_by(iter) %>% 
#   mutate(i_cumulative = 1 - min(R) - S,
#          hosp_rate = 0.04*0.375,
#          hosp_length_days = 7,
#          icu_hosp_number = 1:201,
#          icu_rate_of_hosp_rate = 0.015*0.375,
#          icu_length_days = 8,
#          vent_rate_of_icu_rate = 0.0084*0.375,
#          vent_length_days = 10,
#          hospital_beds = hosp_rate*(i_cumulative - lag(i_cumulative , 7)) + icu_rate_of_hosp_rate*(lag(i_cumulative , 7) - lag(i_cumulative , 8)),
#          icu_beds = icu_rate_of_hosp_rate*(i_cumulative - lag(i_cumulative , 8)),
#          ventilators = vent_rate_of_icu_rate*(i_cumulative - lag(i_cumulative, 10))) %>% 
#   gather(medical_need, value, c(hospital_beds, icu_beds, ventilators)) 



all_hosp <- as_tibble(m_test) %>% 
  filter(variable != "I") %>%
  pivot_wider(names_from = variable, values_from = value) %>% 
  group_by(iter) %>% 
  mutate(i_cumulative = 1 - min(R) - S,
         hosp_rate = rbeta(201, alpha_beta, beta_beta),
         hosp_length_days = 6,
         icu_hosp_number = 1:201,
         icu_rate_of_hosp_rate = rbeta(201, alpha_beta_icu, beta_beta_icu),
         icu_length_days = 8,
         vent_rate_of_icu_rate = rbeta(201, alpha_beta_vent, beta_beta_vent),
         vent_length_days = 10) %>% 
  mutate(hospital_beds = hosp_rate*(i_cumulative - lag(i_cumulative , 6)) + 
           icu_rate_of_hosp_rate*(lag(i_cumulative , 6) - lag(i_cumulative , 5)),
         icu_beds = icu_rate_of_hosp_rate*(i_cumulative - lag(i_cumulative , 5)),
         ventilators = vent_rate_of_icu_rate*(i_cumulative - lag(i_cumulative, 10))) %>% 
  gather(medical_need, value, c(hospital_beds, icu_beds, ventilators)) 


cum_hosp <- all_hosp %>% 
  group_by(iter) %>% 
  filter(medical_need == "hospital_beds") %>% 
  drop_na(value) %>% 
  mutate(cum_hosp = cumsum(value*884235))

# cum_hosp <- as_tibble(m_test) %>% 
#   filter(variable == "I") %>%  
#   group_by(iter) %>% 
#   mutate(cum_i = cumsum(value*884225)) %>% 
#   mutate(hosp_rate = rbeta(201, alpha_beta, beta_beta),
#          hosp_length_days = 7,
#          icu_rate_of_hosp_rate = 0.015*0.375,
#          vent_rate_of_icu_rate = 0.0084*0.375,
#          cum_hosp = hosp_rate*cum_i) 
#   



## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.height=7, fig.width=7--------------------
#make plots
full_plot <- test_sims %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(fill = variable, ymin = lci95, ymax = uci95), alpha = 0.3) +
  geom_ribbon(aes(fill = variable, ymin = lci50, ymax = uci50), alpha = 0.3) +
  geom_ribbon(aes(fill = variable, ymin = lci25, ymax = uci25), alpha = 0.3) +
  geom_line(aes(group = variable)) + 
  scale_fill_viridis_d() +
  geom_point(data = sd_covid_data, aes(x = date, y = active_cases),
             shape = 21, size = 2, fill = "yellow") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "1000 simulations of SIR",
       subtitle = "(dots = positive tests)",
       y = "Number of people",
       x = NULL) +
  # facet_wrap(~generation_time) +
  theme(text = element_text(size = 11)) +
  theme_classic()+
  NULL 
  
zoomed_plot <- m_test %>% 
  group_by(date, variable) %>% 
  filter(variable == "S") %>% 
  mutate(susceptible = value*884235,
         I_cumulative = 884235-susceptible) %>% 
  summarize(mean = median(I_cumulative),
            lci95 = quantile(I_cumulative, probs = 0.025),
            lci50 = quantile(I_cumulative, probs = 0.25),
            lci25 = quantile(I_cumulative, probs = 0.345),
            uci25 = quantile(I_cumulative, probs = 0.655),
            uci50 = quantile(I_cumulative, probs = 0.75),
            uci95 = quantile(I_cumulative, probs = 0.975)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(fill = variable, ymin = lci95, ymax = uci95), fill = "#7851a9", alpha = 0.3) +
  geom_ribbon(aes(fill = variable, ymin = lci50, ymax = uci50), fill = "#7851a9", alpha = 0.3) +
  geom_ribbon(aes(fill = variable, ymin = lci25, ymax = uci25), fill = "#7851a9", alpha = 0.3) +
  geom_line(aes(group = variable)) +
  geom_point(data = sd_covid_data,
             aes(x = date, y = positive_cases), shape = 21,
             fill = "yellow", size = 2) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_x_date(limits = ymd(c("2020-02-22", "2020-7-01"))) +
  coord_cartesian(ylim = c(0,5000)) +
  labs(title = "1000 SIR simulations of cumulative COVID-19 cases",
       subtitle = "(purple = simulation of actual cases, dots = positive tests)",
       y = "Cumulative cases",
       x = NULL) +
  # facet_wrap(~generation_time)+
  theme(text = element_text(size = 11)) +
  theme_classic() + 
  NULL

both <- plot_grid(full_plot, zoomed_plot, ncol = 1)
both
ggsave(both, file = here::here("plots/cumulative_cases.jpg"), width = 8, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.height=9, fig.width=9--------------------
sd_hospital_beds <- read_excel(here::here("data/sd_hospital_beds.xlsx")) %>% 
  clean_names()

all_beds <- sum(sd_hospital_beds$total_beds)


#hospitalized data
hosp <- all_hosp %>% 
  group_by(date, medical_need) %>% 
  mutate(number = value*884235) %>% 
  drop_na(number) %>% 
  summarize(mean = median(number),
            lci95 = quantile(number, probs = 0.025),
            lci50 = quantile(number, probs = 0.25),
            lci25 = quantile(number, probs = 0.345),
            uci25 = quantile(number, probs = 0.655),
            uci50 = quantile(number, probs = 0.75),
            uci95 = quantile(number, probs = 0.975)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = lci95, ymax = uci95, fill = medical_need), alpha = 0.3) +
  geom_ribbon(aes(ymin = lci50, ymax = uci50, fill = medical_need), alpha = 0.3) +
  geom_ribbon(aes(ymin = lci25, ymax = uci25, fill = medical_need), alpha = 0.3) +
  geom_line(aes(group = medical_need)) +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~medical_need) +
  geom_hline(yintercept = all_beds) +
  # annotate("text", x = sd_covid_plot_data$date[5], y = 2980, "All Hospital Beds\nin SD") +
  labs(title = "Medical Capacity Needs - predicted",
       y = "Number",
       x = NULL) +
  theme_classic() +
  scale_x_date(date_breaks = "months" , date_labels = "%b")+
  theme(text = element_text(size = 11),
        legend.position = "top") +
  NULL


hosp_zoomed <- cum_hosp %>% 
  group_by(date) %>% 
  drop_na(cum_hosp) %>% 
  summarize(mean = median(cum_hosp),
            lci95 = quantile(cum_hosp, probs = 0.025),
            lci50 = quantile(cum_hosp, probs = 0.25),
            lci25 = quantile(cum_hosp, probs = 0.345),
            uci25 = quantile(cum_hosp, probs = 0.655),
            uci50 = quantile(cum_hosp, probs = 0.75),
            uci95 = quantile(cum_hosp, probs = 0.975)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = lci95, ymax = uci95, fill = medical_need), alpha = 0.3,fill = "#00204DFF") +
  geom_ribbon(aes(ymin = lci50, ymax = uci50, fill = medical_need), alpha = 0.3,fill = "#00204DFF") +
  geom_ribbon(aes(ymin = lci25, ymax = uci25, fill = medical_need), alpha = 0.3,fill = "#00204DFF") +
  geom_line() +
  geom_point(data = sd_covid_data, 
             aes(x = date, y = hospitalized),
             shape = 21, fill = "yellow",
             size = 2) +
  scale_x_date(limits = ymd(c("2020-03-01", "2020-08-15"))) +
  coord_cartesian(ylim = c(0,500)) +
  # facet_wrap(~generation_time) +
  labs(title = "How well do models match current data?",
       subtitle = "(dots = real data)",
       y = "Cumulative number hospitalized",
       x = NULL) +
  theme_classic() +
  theme(text = element_text(size = 11)) +
  NULL

both_hosp <- plot_grid(hosp, hosp_zoomed, ncol = 1, rel_heights = c(1,0.8))
both_hosp
ggsave(both_hosp, file = here::here("plots/both_hosp.jpg"), width = 8, height = 8)


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=TRUE------------------------------------------------
beds_needed <- all_hosp %>% 
  group_by(date, medical_need) %>% 
  mutate(number = value*884235) %>% 
  drop_na(number) %>% 
  summarize(mean = mean(number),
            lci = quantile(number, probs = 0.025),
            uci = quantile(number, probs = 0.975)) %>% 
  ungroup() %>% 
  group_by(medical_need) %>% 
  mutate(mean = round(max(mean),0),
         lower95 = round(max(lci),0),
         upper95 = round(max(uci),0)) %>% 
  distinct(medical_need, .keep_all = T) %>% 
  select(medical_need, mean, lower95, upper95) %>% 
  ungroup() %>% 
  mutate(medical_need = case_when(medical_need == "hospital_beds" ~ "Hospital Beds",
                                  medical_need == "icu_beds" ~ "ICU Beds",
                                  TRUE ~ "Ventilators")) 

write.csv(beds_needed, file = here::here("data/beds_needed.csv"))

kable_styling(kable(beds_needed, caption = "Table 2. Estimated peak medical needs in South Dakota. ", format = "pandoc",
      col.names = c("Need", "Mean", "Lower95", "Upper95")),
      full_width = F) %>%
   column_spec(column = 1:2, width = "3in") %>%
   kable_styling(c("bordered", "condensed"), full_width = F)

#make predictions to save and upload to google drive
predictions <- all_hosp %>% 
  group_by(date, medical_need) %>% 
  mutate(number = value*884235) %>% 
  drop_na(number) %>% 
  summarize(mean = mean(number),
            sd = sd(number),
            lci = quantile(number, probs = 0.025),
            uci = quantile(number, probs = 0.975)) %>% 
  ungroup() %>% 
  group_by(medical_need) %>% 
  filter(mean == max(mean)) %>% 
  mutate(date_prediction_made = ymd(Sys.Date()),
         team = "Wesner/Van Peursem/Flores/Lio",
         model_source = "https://github.com/jswesner/covid_sd",
         notes = "lowered hosp rate to 3.5 average. Simulated generation times from 4-8 days") %>% 
  rename(peak_date = date) %>% 
  select(date_prediction_made, peak_date, medical_need, mean, sd, team, model_source, notes)

write.csv(predictions, file = here::here("outputs/predictions.csv"), row.names = F)

