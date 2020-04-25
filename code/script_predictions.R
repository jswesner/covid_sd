library(lubridate)
predictions <- all_hosp %>% 
  group_by(date, medical_need, generation_time) %>% 
  mutate(number = value*884235) %>% 
  drop_na(number) %>% 
  summarize(mean = mean(number),
            sd = sd(number),
            lci = quantile(number, probs = 0.025),
            uci = quantile(number, probs = 0.975)) %>% 
  ungroup() %>% 
  group_by(generation_time, medical_need) %>% 
  filter(mean == max(mean)) %>% 
  mutate(date_prediction_made = ymd(Sys.Date()),
         team = "Wesner/Van Peursem/Flores/Lio",
         model_source = "https://github.com/jswesner/covid_sd") %>% 
  rename(peak_date = date,
         notes = generation_time) %>% 
  filter(!grepl("4", notes)) %>% 
  select(date_prediction_made, peak_date, medical_need, mean, sd, team, model_source, notes)

write.csv(predictions, file = here::here("outputs/predictions.csv"), row.names = F)

          