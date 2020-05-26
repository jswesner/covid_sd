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
         date_num = date_numeric - min(date_numeric)) %>%
  mutate(pop = case_when(grepl("inneha", group) ~ 197472,
                         TRUE ~ 858469 - 197472),
         cum_hosp_prop = cum_hosp/pop,
         cum_hosp_rate = as.integer(cum_hosp_prop*1000000))

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
# fit_weib_state <- brm(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
#                          ult ~ 1 , omega ~ 1 , theta ~ 1 ,
#                          nl = TRUE ),
#                       data = cum_hosp_state,
#                       family = poisson(link = "log"),
#                       prior = c(
#                         prior(normal(8,1), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
#                         prior(normal(50, 10), nlpar = "theta"), #time (days since start) of inflection
#                         prior(gamma(4,2), nlpar = "omega")), # slope at inflection. Gamma to ensure it is positive (has to be since data are cumulative) assumes slope of exp(2) with sd of 0.5
#                       iter = 1000, chains = 1,
#                       sample_prior = "yes")
# 
# posts_state <- posterior_samples(fit_weib_state)
# 
# 
# saveRDS(fit_weib_state, file = here("outputs/fit_weib_state.rds"))
fit_web_statelast <- readRDS("~/GitHub/covid_sd/outputs/fit_web_state.rds")

#past datasets
# cum_hosp_state70 <- cum_hosp_state %>% filter(date_num <= 70)
cum_hosp_state60 <- cum_hosp_state %>% filter(date_num <= 60)
cum_hosp_state40 <- cum_hosp_state %>% filter(date_num <= 40)
cum_hosp_state20 <- cum_hosp_state %>% filter(date_num <= 20)

#re-run model
fit_weib_state <- update(fit_weib_statelast, new_data = cum_hosp_state)
# fit_weib_state70 <- update(fit_weib_state, newdata = cum_hosp_state70)
fit_weib_state60 <- update(fit_weib_state, newdata = cum_hosp_state60)
fit_weib_state40 <- update(fit_weib_state, newdata = cum_hosp_state40)
fit_weib_state20 <- update(fit_weib_state, newdata = cum_hosp_state20)

#predict
new_dates <- tibble(date_num = seq(0, 300, by = 1))

#get_posts 
post_weib_stateall <- posterior_samples(fit_weib_state) %>% as_tibble() %>% mutate(model = "current", iter = 1:nrow(.)) %>% clean_names()
# post_weib_state70 <- posterior_samples(fit_weib_state70) %>% as_tibble() %>% mutate(model = "day70", iter = 1:nrow(.)) %>% clean_names()
post_weib_state60 <- posterior_samples(fit_weib_state60) %>% as_tibble() %>% mutate(model = "day60", iter = 1:nrow(.)) %>% clean_names()
post_weib_state40 <- posterior_samples(fit_weib_state40) %>% as_tibble() %>% mutate(model = "day40", iter = 1:nrow(.)) %>% clean_names()
post_weib_state20 <- posterior_samples(fit_weib_state20) %>% as_tibble() %>% mutate(model = "day20", iter = 1:nrow(.)) %>% clean_names()
post_weib_state0 <- posterior_samples(fit_weib_state20) %>% as_tibble() %>% mutate(model = "day0_prior_pred", iter = 1:nrow(.)) %>% clean_names() %>% 
  select(contains("prior"), model, iter) %>% 
  rename(b_ult_intercept = prior_b_ult,
         b_omega_intercept = prior_b_omega,
         b_theta_intercept = prior_b_theta)

combine_posts <- bind_rows(post_weib_stateall,
                           # post_weib_state70,
                           post_weib_state60, post_weib_state40, post_weib_state20, post_weib_state0)


#generate fitted and posterior predictions
predict_posts <- combine_posts %>% expand_grid(date_num = seq(0, 200, by = 1)) %>% 
  mutate(y_fit = exp((b_ult_intercept*date_num^b_omega_intercept)/(b_theta_intercept^b_omega_intercept + date_num^b_omega_intercept)),
         y_pred = rpois(nrow(.), exp((b_ult_intercept*date_num^b_omega_intercept)/(b_theta_intercept^b_omega_intercept + date_num^b_omega_intercept))))


predict_posts_sum <- predict_posts %>% 
  gather(fit_pred, y, c(y_fit, y_pred)) %>% 
  group_by(date_num, model, fit_pred) %>% 
  summarize(median = median(y),
            sd = sd(y),
            low95 = quantile(y, probs = 0.025),
            high95 = quantile(y, probs = 0.975),
            low50 = quantile(y, probs = 0.25),
            high50 = quantile(y, probs = 0.75)) %>% 
  ungroup() %>% 
  mutate(model = fct_relevel(model, "current", after = Inf),
         date = as_date(min(cum_hosp$date_numeric) + date_num))

#plot
cum_hosp_state60 <- cum_hosp_state %>% filter(date_num <= 60) %>% expand_grid(fit_pred = c("fit_", "pred")) %>% mutate(model = "day60")
cum_hosp_state40 <- cum_hosp_state %>% filter(date_num <= 40) %>% expand_grid(fit_pred = c("fit_", "pred")) %>% mutate(model = "day40")
cum_hosp_state20 <- cum_hosp_state %>% filter(date_num <= 20) %>% expand_grid(fit_pred = c("fit_", "pred")) %>% mutate(model = "day20")
cum_hosp_state0 <- cum_hosp_state %>% filter(date_num < 0) %>% expand_grid(fit_pred = c("fit_", "pred")) %>% mutate(model = "day0_prior_pred")
cum_hosp_stateall <- cum_hosp_state %>% expand_grid(fit_pred = c("fit_", "pred")) %>% mutate(model = "current")

cum_hosp_stateraw <- bind_rows(cum_hosp_state60,
                          cum_hosp_state40,
                          cum_hosp_state20,
                          cum_hosp_state0,
                          cum_hosp_stateall) %>% 
  mutate(model = fct_relevel(model, "current", after = Inf))


predict_posts_sum %>% 
  filter(fit_pred == "y_pred") %>% 
  ggplot(aes(x = date, y=  median)) + 
  geom_line(aes(group = interaction(model, fit_pred))) + 
  geom_ribbon(aes(ymin = low95, ymax = high95), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  facet_grid(. ~ model) +
  geom_point(data = cum_hosp_stateraw, aes(y = cum_hosp), color = "black", shape = 21) + 
  coord_cartesian(ylim = c(0, 3000)) +
  labs(y = "Cumulative Hospitalizations",
       title = "Model forecasts over time",
       x = "") +
  scale_fill_brewer(type = "qual", palette = 5) + 
  scale_color_brewer(type = "qual", palette = 5) +
  theme_bw() +
  NULL





#GROUPS ---------------
get_prior(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
             ult ~ 1 + (1|group), omega ~ 1 + (1|group), theta ~ 1 + (1|group),
             nl = TRUE),
          data = cum_hosp,
          family = poisson(link = "log"),
          prior = c(
            prior(normal(7,2), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
            prior(normal(50, 10), nlpar = "theta"), #time (days since start) of inflection
            prior(lognormal(0.7, 1), nlpar = "omega")), # slope at inflection. Gamma to ensure it is positive 
          iter = 1000, chains = 4)




#priors are taken from the state model above but with wider standard deviations
# fit_weib_state
# 
# fit_weib_group <- brm(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
#                          ult ~ 1 + (1|group), omega ~ 1 + (1|group), theta ~ 1 + (1|group),
#                          nl = TRUE),
#                       data = cum_hosp,
#                       family = poisson(link = "log"),
#                       prior = c(
#                         prior(normal(6,1), nlpar = "ult"), #asymptote - lower to account for splitting data
#                         prior(normal(50, 10), nlpar = "theta"), #time (days since start) of inflection
#                         prior(gamma(4,2), nlpar = "omega"),
#                         prior(cauchy(0, 1), class = "sd", nlpar = "ult"),
#                         prior(cauchy(0, 1), class = "sd", nlpar = "theta"),
#                         prior(cauchy(0, 1), class = "sd", nlpar = "omega")), # slope at inflection. Gamma to ensure it is positive 
#                       iter = 1000, chains = 1,
#                       sample_prior = "yes")


# saveRDS(fit_weib_group, file = here("outputs/fit_web_group.rds"))
fit_web_grouplast <- readRDS("~/GitHub/covid_sd/outputs/fit_web_group.rds")

#past datasets
# cum_hosp_group70 <- cum_hosp %>% filter(date_num <= 70)
cum_hosp_group60 <- cum_hosp %>% filter(date_num <= 60)
cum_hosp_group40 <- cum_hosp %>% filter(date_num <= 40)
cum_hosp_group20 <- cum_hosp %>% filter(date_num <= 20)

#update models
fit_weib_group <- update(fit_weib_grouplast, newdata = cum_hosp)
# fit_weib_group70 <- update(fit_weib_group, newdata = cum_hosp_group70)
fit_weib_group60 <- update(fit_weib_group, newdata = cum_hosp_group60)
fit_weib_group40 <- update(fit_weib_group, newdata = cum_hosp_group40)
fit_weib_group20 <- update(fit_weib_group, newdata = cum_hosp_group20)

#posts
#predict
new_dates <- tibble(date_num = seq(0, 200, by = 1))

#get_posts 
post_weib_groupall <- posterior_samples(fit_weib_group) %>% as_tibble() %>% mutate(model = "current", iter = 1:nrow(.)) %>% clean_names()
# post_weib_group70 <- posterior_samples(fit_weib_group70) %>% as_tibble() %>% mutate(model = "day70", iter = 1:nrow(.)) %>% clean_names()
post_weib_group60 <- posterior_samples(fit_weib_group60) %>% as_tibble() %>% mutate(model = "day60", iter = 1:nrow(.)) %>% clean_names()
post_weib_group40 <- posterior_samples(fit_weib_group40) %>% as_tibble() %>% mutate(model = "day40", iter = 1:nrow(.)) %>% clean_names()
post_weib_group20 <- posterior_samples(fit_weib_group20) %>% as_tibble() %>% mutate(model = "day20", iter = 1:nrow(.)) %>% clean_names()
post_weib_group0 <- posterior_samples(fit_weib_group20) %>% as_tibble() %>% mutate(model = "day0_prior_pred", iter = 1:nrow(.)) %>% clean_names() %>% 
  select(contains("prior"), contains("r_"), model, iter) %>% 
  rename(b_ult_intercept = prior_b_ult,
         b_omega_intercept = prior_b_omega,
         b_theta_intercept = prior_b_theta)

combine_posts_group <- bind_rows(post_weib_groupall, 
                                 # post_weib_group70, 
                                 post_weib_group60, post_weib_group40, post_weib_group20)


#generate fitted and posterior predictions
predict_posts_a <- combine_posts_group %>% expand_grid(date_num = seq(0, 200, by = 1)) %>% 
  mutate(b_ult_intercept_minn = b_ult_intercept + r_group_ult_minnehaha_county_intercept,
         b_ult_intercept_rest = b_ult_intercept + r_group_ult_rest_of_south_dakota_intercept,
         b_omega_intercept_minn = b_omega_intercept + r_group_omega_minnehaha_county_intercept,
         b_omega_intercept_rest = b_omega_intercept + r_group_omega_rest_of_south_dakota_intercept,
         b_theta_intercept_minn = b_theta_intercept + r_group_theta_minnehaha_county_intercept,
         b_theta_intercept_rest = b_theta_intercept + r_group_theta_rest_of_south_dakota_intercept) %>% 
  mutate(y_fit_minn = exp((b_ult_intercept_minn*date_num^b_omega_intercept_minn)/(b_theta_intercept_minn^b_omega_intercept_minn + date_num^b_omega_intercept_minn)),
         y_pred_minn = rpois(nrow(.), exp((b_ult_intercept_minn*date_num^b_omega_intercept_minn)/(b_theta_intercept_minn^b_omega_intercept_minn + date_num^b_omega_intercept_minn))),
         y_fit_rest = exp((b_ult_intercept_rest*date_num^b_omega_intercept_rest)/(b_theta_intercept_rest^b_omega_intercept_rest + date_num^b_omega_intercept_rest)),
         y_pred_rest = rpois(nrow(.), exp((b_ult_intercept_rest*date_num^b_omega_intercept_rest)/(b_theta_intercept_rest^b_omega_intercept_rest + date_num^b_omega_intercept_rest))))

predict_posts_prior <- post_weib_group0 %>% expand_grid(date_num = seq(0, 200, by = 1)) %>% 
  mutate(b_ult_intercept_minn = b_ult_intercept ,
         b_ult_intercept_rest = b_ult_intercept ,
         b_omega_intercept_minn = b_omega_intercept ,
         b_omega_intercept_rest = b_omega_intercept ,
         b_theta_intercept_minn = b_theta_intercept ,
         b_theta_intercept_rest = b_theta_intercept ) %>% 
  mutate(y_fit_minn = exp((b_ult_intercept_minn*date_num^b_omega_intercept_minn)/(b_theta_intercept_minn^b_omega_intercept_minn + date_num^b_omega_intercept_minn)),
         y_pred_minn = rpois(nrow(.), exp((b_ult_intercept_minn*date_num^b_omega_intercept_minn)/(b_theta_intercept_minn^b_omega_intercept_minn + date_num^b_omega_intercept_minn)) + prior_sd_group),
         y_fit_rest = exp((b_ult_intercept_rest*date_num^b_omega_intercept_rest)/(b_theta_intercept_rest^b_omega_intercept_rest + date_num^b_omega_intercept_rest)),
         y_pred_rest = rpois(nrow(.), exp((b_ult_intercept_rest*date_num^b_omega_intercept_rest)/(b_theta_intercept_rest^b_omega_intercept_rest + date_num^b_omega_intercept_rest)))+ prior_sd_group)

predict_posts_group <- bind_rows(predict_posts_a, predict_posts_prior)  

predict_posts_sumgroup <- predict_posts_group %>% 
  gather(fit_pred, y, c(y_fit_minn, y_pred_minn,
                        y_fit_rest, y_pred_rest)) %>%
  mutate(group = str_sub(fit_pred, -4,-1),
         fit_pred = str_sub(fit_pred, 3, 6)) %>% 
  group_by(date_num, model, fit_pred, group) %>% 
  summarize(median = median(y, na.rm = T),
            sd = sd(y, na.rm = T),
            low90 = quantile(y, probs = 0.05, na.rm = T),
            high90 = quantile(y, probs = 0.95, na.rm = T),
            low50 = quantile(y, probs = 0.25, na.rm = T),
            high50 = quantile(y, probs = 0.75, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(model = fct_relevel(model, "current", after = Inf),
         date = as_date(min(cum_hosp$date_numeric) + date_num))



#plot
cum_hosp_group60 <- cum_hosp %>% filter(date_num <= 60) %>% mutate(group = case_when(grepl("inne", group) ~ "minn", TRUE ~ "rest"), model = "day60") %>% expand_grid(fit_pred = c("fit_", "pred"))
cum_hosp_group40 <- cum_hosp %>% filter(date_num <= 40) %>% mutate(group = case_when(grepl("inne", group) ~ "minn", TRUE ~ "rest"), model = "day40") %>% expand_grid(fit_pred = c("fit_", "pred"))
cum_hosp_group20 <- cum_hosp %>% filter(date_num <= 20) %>% mutate(group = case_when(grepl("inne", group) ~ "minn", TRUE ~ "rest"), model = "day20") %>% expand_grid(fit_pred = c("fit_", "pred"))
cum_hosp_group0 <- cum_hosp %>% filter(date_num < 0) %>% mutate(group = case_when(grepl("inne", group) ~ "minn", TRUE ~ "rest"), model = "day0_prior_pred") %>% expand_grid(fit_pred = c("fit_", "pred"))
cum_hosp_groupall <- cum_hosp %>% mutate(group = case_when(grepl("inne", group) ~ "minn", TRUE ~ "rest"), model = "current") %>% expand_grid(fit_pred = c("fit_", "pred"))

cum_hosp_raw <- bind_rows(cum_hosp_group60,
                          cum_hosp_group40,
                          cum_hosp_group20,
                          cum_hosp_group0,
                          cum_hosp_groupall) %>% 
  mutate(model = fct_relevel(model, "current", after = Inf))

predict_posts_sumgroup %>% 
  filter(fit_pred == "pred") %>% 
  ggplot(aes(x = date, y =  median, color = group, fill = group)) + 
  geom_line(aes(group = interaction(model, fit_pred, group))) + 
  geom_ribbon(aes(ymin = low90, ymax = high90), alpha = 0.4) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.4) +
  facet_grid(. ~ model) +
  # scale_y_log10() + 
  geom_point(data = cum_hosp_raw, aes(y = cum_hosp), color = "black", shape = 21) + 
  coord_cartesian(ylim = c(0, 1500)) +
  labs(y = "Cumulative Hospitalizations",
       title = "Model forecasts over time",
       x = "") +
  scale_fill_brewer(type = "qual", palette = 5) + 
  scale_color_brewer(type = "qual", palette = 5) +
  theme_bw() +
  NULL
