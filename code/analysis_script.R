## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE---------------------------------------------------------
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


## ----echo=FALSE, fig.height=2, message=FALSE, warning=FALSE, paged.print=FALSE-------------------------------------------
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


## ----include=FALSE, cache = F--------------------------------------------------------------------------------------------
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


# fit_weib <- brm(bf(cum_prop_hosp|trunc(lb = 0) ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
#                    ult ~ 1, omega ~ 1, theta ~ 1,
#                    nl = TRUE),
#                 data = sd_covid_data,
#                 # family = gaussian(),
#                 family = gaussian(),
#                 prior = c(
#                   prior(beta(2.76, 549), nlpar = "ult"), #original_prior
#                   prior(gamma(4, 0.04), nlpar = "theta"),
#                   prior(normal(4, 0.4), nlpar = "omega")),
#                 iter = 4000,
#                 sample_prior = "yes")

fit_priorday <- readRDS(here::here("code/fit_weib.rds"))
fit_weib <- update(fit_priorday, newdata = sd_covid_data) #re-run the model above with today's data (this shortcut just prevents the need to recompile the model)

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

## ----echo=FALSE, fig.height=9, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE---------------------------------------------------------
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

