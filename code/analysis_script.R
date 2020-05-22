## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------------------------------------------------------------------------------------
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


## ----echo=FALSE, fig.height=2, message=FALSE, warning=FALSE, paged.print=FALSE------------------------------------------------------------------------------
ggplot() +
  annotate("text", x = 0.5, y = 0.7,
                   label = "(y_cumulative[i]) %~% italic(Poisson)(lambda[i])", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.52,
           label = "italic(log(lambda[i])) == frac(italic(maxX)*date[i]^omega, theta^omega + date[i]^omega)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.35,
           label = "italic(max) %~% italic(N)(7,1)", parse = T, family = "serif") +
  annotate("text", x = 0.5, y = 0.27,
           label = "italic(theta) %~% italic(N)(50, 10)", parse = T, family = "serif") +
   annotate("text", x = 0.5, y = 0.19,
           label = "italic(omega) %~% italic(Gamma)(4, 2)", parse = T, family = "serif") +
  coord_cartesian(ylim = c(0.11, .8)) +
  labs(title = "Weibull non-linear regression:") +
  theme_void() +
  theme(text = element_text(family = "serif"))


## ----message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE-----------------------------------------------------------------------------------------
sd_covid_data <- read.csv(here::here("data/data_kelo.csv")) %>%
  clean_names() %>%
  mutate(date_num = as.numeric(mdy(date)) - 18329,
         date = mdy(date),
         source = "SD_DOH via Keloland news",
         incidence = positive_cases - lag(positive_cases),
         cum_hosp = parse_number(as.character(hospitalized)))  ##adds 1 to all incidence to allow for zeros in log-linear regression


# max_prop_hosp <- max(nyc_hosp$cum_prop_hosp)
# sd_prior_max = sd_pop*max_prop_hosp
# sd_prior_maxsd = 2000 

cum_hosp <- read_csv("data/cum_hosp.csv", na = c("", ".")) %>% 
  mutate(date = mdy(date_hosp),
         date_numeric  = as.numeric(date),
         date_num = date_numeric - min(date_numeric)) 

# get_prior(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
#              ult ~ 1 + (1|group), omega ~ 1 + (1|group), theta ~ 1 + (1|group),
#              nl = TRUE),
#           data = cum_hosp,
#           family = poisson(link = "log"))
# 
# gamma_parms <- function(mean, sd) {
#   out <- tibble(shape = mean^2/sd^2,
#                 rate = mean/sd^2,
#                 scale = 1/(mean/sd^2))
#   return(out)
#   return(plot)
# }
# 
# gamma_parms(mean = 2, sd = 1)

cum_hosp_state <- cum_hosp %>% group_by(date, date_num) %>% summarize(cum_hosp = sum(cum_hosp))

#fit weibull with poisson likelihood
fit_weib_state <- brm(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
                         ult ~ 1 , omega ~ 1 , theta ~ 1 ,
                         nl = TRUE ),
                      data = cum_hosp_state,
                      family = poisson(link = "log"),
                      prior = c(
                        prior(normal(7,1), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
                        prior(normal(50, 10), nlpar = "theta"), #time (days since start) of inflection
                        prior(gamma(4,2), nlpar = "omega")), # slope at inflection. Gamma to ensure it is positive (has to be since data are cumulative) assumes slope of exp(2) with sd of 0.5
                      iter = 1000, chains = 1,
                      sample_prior = "yes")


## ----echo=FALSE, fig.height=9, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------------------------


#predictions up to 200 days after the first hospitalization
newdata = tibble(date_num = 0:200)

predict_cumulative <- predict(fit_weib_state, newdata = newdata, probs = c(0.025, 0.975, 0.25, 0.75))  %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(date_num = 18329 + newdata$date_num,
         date = as_date(date_num),
         method = "predicted") 



#make predictions of daily hospital needs
days_in_hosp <- 10 #adjusted to visually match actual active hosps

predict_dailyhosp <- predict(fit_weib_state, newdata = newdata, summary = F)  %>%
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
  mutate(daily_total = daily_total) %>%
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


cumulative_plot <- predict_cumulative %>% 
  ggplot(aes(x = date, y = estimate)) +
  geom_line(color = "dodgerblue") +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5), alpha = 0.2, fill = "dodgerblue" ) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, fill = "dodgerblue" ) +
  geom_point(data = cum_hosp_state, aes(x = date, y = cum_hosp),
             shape = 21, fill = "yellow") +
  # facet_wrap(~ group, scales = "free") +
  theme_classic() +
  scale_x_date() +
  # scale_x_date(date_breaks = "months" , date_labels = "%b") +
  # ylim(0, 1500) +
  labs(y = "Cumulative ever hospitalized",
       title = "Weibull model results",
       subtitle = "Dots = data. Shading = 50% and 95% prediction intervals") +
  NULL

active_plot <- predict_dailyhosp %>% 
ggplot() +
  geom_line(aes(x = date, y = median), color = "dodgerblue") +
  geom_ribbon(aes(x = date, ymin = low95, ymax = high95), fill = 'dodgerblue', alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = low50, ymax = high50), fill = 'dodgerblue', alpha = 0.2) +
  geom_point(data = sd_covid_data, aes(x = date, y = hospitalized_currently),
             shape = 21, fill = "yellow") +
  # scale_x_date(date_breaks = "months" , date_labels = "%b") +
  theme_classic() +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Active hospitalizations",
       title = "Predicting active hospitalizations from Weibull model",
       subtitle = "Shading = 50% and 95% prediction intervals") +
  # coord_cartesian(ylim = c(0,100)) +
  NULL
# active_plot

both_plot_group <- plot_grid(cumulative_plot, active_plot, ncol = 1, align = "v")

both_plot_group


## ----message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE-----------------------------------------------------------------------------------------
fit_weib_group <- brm(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
                         ult ~ 1 + (1|group), omega ~ 1 + (1|group), theta ~ 1 + (1|group),
                         nl = TRUE ),
                      data = cum_hosp,
                      family = poisson(link = "log"),
                      prior = c(
                        prior(normal(7,2), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
                        prior(normal(50, 10), nlpar = "theta"), #time (days since start) of inflection
                        prior(gamma(4,2), nlpar = "omega")), # slope at inflection. Gamma to ensure it is positive 
                      sample_prior = "yes")

# plot(conditional_effects(fit_weib_group))


## ----echo=FALSE, fig.height=9, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------------------------------------------

#group fits
newdata <- tibble(date_num = seq(0,300, by = 1),
                  group = c("Minnehaha County"))
newdata_rest <- tibble(date_num = seq(0,300, by = 1),
                  group = c("Rest of South Dakota"))

postpreds_minn <- predict(fit_weib_group, newdata = newdata,probs = c(0.95, 0.05, 0.75, 0.25), re_formula = NULL)  %>% 
  as_tibble() %>% clean_names() %>%  mutate(group = "Minnehaha County",
                                            date_num = newdata$date_num)

postpreds_rest <- predict(fit_weib_group, newdata = newdata_rest, probs = c(0.95, 0.05, 0.75, 0.25), re_formula = NULL) %>% 
  as_tibble() %>% clean_names() %>%  mutate(group = "Rest of South Dakota",
                                            date_num = newdata_rest$date_num)


post_all <- bind_rows(postpreds_minn, postpreds_rest)


predict_group_cumulative <- post_all %>% 
  ggplot(aes(x = date_num, y = estimate, fill = group)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = q5,ymax = q95), alpha = 0.2) +
  geom_ribbon(aes(ymin = q25,ymax = q75), alpha = 0.2) +
  geom_point(data = cum_hosp, aes(x = date_num, y = cum_hosp, fill = group), shape = 21) +
  # ylim(0,500) +
  # facet_wrap(~group) +
  xlim(0, 250) +
  # ylim(0, 1200) + 
  theme_classic() +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Cumulative hospitalizations",
       title = "Predicting active hospitalizations from Weibull model",
       subtitle = "Shading = 50% and 95% prediction intervals") +
  # coord_cartesian(ylim = c(0,100)) +
  NULL


#active hospitalizations
days_in_hosp <- 10 #adjusted to visually match actual active hosps

date_convert <- tibble(date_num = as.numeric(min(cum_hosp$date)) + newdata$date_num) %>% 
  mutate(date_order = 1:nrow(.))

predict_dailyhosp_minn <- predict(fit_weib_group, newdata = newdata, summary = F)  %>%
  as_tibble() %>%
  mutate(iter = 1:nrow(.)) %>%
  gather(key, value, -iter) %>%
  mutate(date_order = parse_number(key)) %>% 
  left_join(date_convert) %>% 
  mutate(date = as_date(date_num)) %>%
  arrange(iter, date_num) %>%
  group_by(iter) %>%
  mutate(daily_total = value - lag(value, days_in_hosp)) %>%
  group_by(date_num) %>%
  drop_na(daily_total) %>%
  mutate(daily_total = daily_total) %>%
  group_by(iter, date) %>% 
  filter(!any(daily_total < 0)) %>% 
  group_by(date) %>% 
  summarize(median = median(daily_total),
            mean = mean(daily_total),
            sd = sd(daily_total),
            high90 = quantile(daily_total, probs = 0.95),
            high50 = quantile(daily_total, probs = 0.75),
            low90 = quantile(daily_total, probs = 0.05),
            low50 = quantile(daily_total, probs = 0.25)) %>% 
  mutate(group = "Minnehaha County")

date_convertrest <- tibble(date_num = as.numeric(min(cum_hosp$date)) + newdata_rest$date_num) %>% 
  mutate(date_order = 1:nrow(.))

predict_dailyhosp_rest <- predict(fit_weib_group, newdata = newdata_rest, summary = F)  %>%
  as_tibble() %>%
  mutate(iter = 1:nrow(.)) %>%
  gather(key, value, -iter) %>%
  mutate(date_order = parse_number(key)) %>% 
  left_join(date_convertrest) %>% 
  mutate(date = as_date(date_num)) %>%
  arrange(iter, date_num) %>%
  group_by(iter) %>%
  mutate(daily_total = value - lag(value, days_in_hosp)) %>%
  group_by(date_num) %>%
  drop_na(daily_total) %>%
  mutate(daily_total = daily_total) %>%
  group_by(iter, date) %>% 
  filter(!any(daily_total < 0)) %>%
  group_by(date) %>% 
  summarize(median = median(daily_total),
            mean = mean(daily_total),
            sd = sd(daily_total),
            high90 = quantile(daily_total, probs = 0.95),
            high50 = quantile(daily_total, probs = 0.75),
            low90 = quantile(daily_total, probs = 0.05),
            low50 = quantile(daily_total, probs = 0.25)) %>% 
  mutate(group = "Rest of South Dakota")


daily_hosp_all <- bind_rows(predict_dailyhosp_minn, predict_dailyhosp_rest)


active_daily_group <- daily_hosp_all %>% 
  ggplot(aes(x = date, y= median, fill = group)) + 
  geom_line(aes(color = group)) +
  geom_ribbon(aes(fill = group, color = group, ymin = low90, ymax = high90), alpha = 0.2) +
  geom_ribbon(aes(fill = group, color = group, ymin = low50, ymax = high50), alpha = 0.2) +
  # geom_point(data = daily_hosp, aes(x = date, y = daily_hosp, group = group, fill = group), shape = 21) +
  facet_wrap(~group) +
   theme_classic() +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Active hospitalizations",
       title = "Predicting active hospitalizations from Weibull model",
       subtitle = "Shading = 50% and 95% prediction intervals") +
  # coord_cartesian(ylim = c(0,100)) +
  NULL

both_group <- plot_grid(predict_group_cumulative, active_daily_group, ncol = 1, align = "v")

both_group




## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------------------------------------------------------------------------------------
#prior v post

posts <- posterior_samples(fit_weib_state) %>% as_tibble() %>% mutate(iter = 1:nrow(.))

x = seq(1,200, by = 10)

priors <- posts %>% select(prior_b_ult, prior_b_omega, prior_b_theta, iter)  %>% 
  expand_grid(x) %>% 
  mutate(pred = (prior_b_ult*x^prior_b_omega)/(prior_b_theta^prior_b_omega + x^prior_b_omega),
         model = 'prior_prediction') %>% 
  select(x, pred, model)

posteriors <- posts %>% select(b_ult_Intercept, b_omega_Intercept, b_theta_Intercept)  %>% 
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
  summarize(mean = mean(exp(pred)),
            low95 = quantile(exp(pred), probs = 0.025),
            high95 = quantile(exp(pred), probs = 0.975)) %>% 
  ggplot(aes(x = date, y = mean, ymin = low95, ymax = high95,
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
  # ylim(0, 25000) +
  NULL

