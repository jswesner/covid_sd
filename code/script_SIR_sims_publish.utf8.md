---
title: "COVID-19 modeling in South Dakota"
date: "April 2020"
output:
  pdf_document: default
  html_document: default
  word_document: default
---
# Authors
*Jeff Wesner, Ph.D.*^1^, *Dan Van Peursem, Ph.D.*^2^, *Jose Flores, Ph.D.*^2,3^, *Yuhlong Lio, Ph.D.*^2^

University of South Dakota

^1^Department of Biology, ^2^Department of Mathematical Sciences, ^3^Department of Computer Science

<Jeff.Wesner@usd.edu>

# Purpose
To predict hospital bed needs, ICU needs, and ventilator needs in South Dakota due to COVID-19. 

# General Approach and Justification
We estimated R0 from current incidence rates in South Dakota. We then fit SIR models using our estimates of R0 and compared model predictions to actual values of hospitalizations reported by the South Dakota Department of Health (data source: https://www.keloland.com/keloland-com-original/why-south-dakotas-number-of-deaths-isnt-always-up-to-date/. We chose this approach because it does not rely on external estimates of R0, but instead derives them from data specific to South Dakota.

Because our estimates of R0 are derived from reported incidence data, they reflect any day-to-day adjustments in R0 due to social distancing (with an unknown lag time). In other words, as social distancing reduces incidence, that will be reflected in our estimates of R0. It is worth noting that reported incidence is almost certainly lower than true incidence. However, this does not alter our estimates of R0, assuming that the rate of underreporting is constant across time. 

# Derivation of R0
We used the following equation to estimate R0 (eqn 3.1 in Wallinga and Lipsitch 2007) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1766383/?report=reader#!po=83.3333:

$$R0 = 1 + r/b$$

where 1/b is the generation time (aka serial interval) in days, and *r* is the slope of a linear regression between daily incidence and time. This approach is recommended during the initial phase of an epidemice when growth is approximately log-linear. 

We estimated a posterior distribution of *r* using reported incidence data in the following regression:

$$log(y_i) \sim N(\mu_i, \sigma)$$
$$\mu_i = \alpha + \beta x_i$$
$$\alpha \sim N(0,1)$$
$$\beta \sim N(0,1)$$
$$\sigma \sim HalfCauchy(0,1)$$
where *log*($y_i$) is log-transformed incidence on date $i$, distributed as a normal distribution with a mean $\mu_i$ and standard deviation $\sigma$, $\alpha$ is the intercept, $\beta$ is the slope (aka *r*).The prior distributions for each parameter are below the regression equation. 

The outcome of that regression is below. It shows a clear dip in incidence over the last 3-4 days, though this has had a minimal effect thus far on our estimates of *r*. More importantly, our main comparison to model fit is the number of cumulative hospitalizations, which continue to track the model predictions (scroll below for the graph).







![](script_SIR_sims_publish_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

We simulated uncertainty in R0 by sampling from the posterior distribution of *r* 500 times and re-calculating the equation for R0 each time. We did this under three scenarios of generation time (4, 6, or 7 days). These were chosen based on Park et al. (2020) who estimated a generation time for COVID-19 of 4-8 days - https://www.mdpi.com/2077-0383/9/4/967. The table below shows the estimated R0 under three assumptions of generation time. Five hundred samples from these values were entered into the following SIR model:
 

Table: Table 1. R0 values and generation times (days) used to fit the SIR model.

generation_time    mean     sd
----------------  -----  -----
GT4                1.50   0.03
GT6                1.74   0.05
GT7                1.87   0.06

$$\frac{dS}{dt} = \frac{\beta*S*I}N$$

$$\frac{dI}{dt} = \frac{\beta*S*I}N - \gamma*I$$

$$\frac{dR}{dt} = \gamma*I$$

where $\gamma$ is 1/$days_{infected}$, $\beta$ is $gamma$*R0, $days_{infected}$ is 7, and $N$ is $S+I+R$. We simulated 200 days of infection and assumed starting values for S = 0.99999, I = 0.000001, and R = 0.000009. 



![](script_SIR_sims_publish_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 
The graphs above show the outcome of the SIR model under 4, 6, or 7 day generation times. Lines are the mean predictions, shaded areas are the 2.5 and 97.5% quantiles, and the dots are the reported data from SD DOH. 

#Hospital Beds, ICU beds, and Ventilators
From the predictions of cases above, we estimated the number of hospital beds, ICU beds, and ventilators needed by assuming that 4% of cases would need hospitalization, 1.5% of cases would need an ICU bed, and 1.05% of cases would need a ventilator. We also assumed a mean stays in the hospital system as a whole of 7, 8, or 10 days for hospitalization, ICU, and ventilator, respectively.

![](script_SIR_sims_publish_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

This plot shows the predicted number of hospital beds, ICU beds, and ventilators needed under each scenario. The horizontal black line shows the total number of hospital beds in South Dakota*. 

The plot on the bottom shows the predicted *cumulative* number of beds compared to the actual cumulative hospital beds used. 
*(sources: https://apps.sd.gov/ph04lassnet/rptPH04LicenseList.Aspx and https://doh.sd.gov/providers/preparedness/hospital-preparedness/system/bed-avail.aspx)

The model with a 7-day generation time appears to best match the actual hospitalization data. It indicates that peak resource use will occur ~June 1 with numbers indicated in Table 2.

Table: Table 2. Estimated peak medical needs in South Dakota. Dates for peak need are currently projected as early June, 2020. 

Need             Mean   Lower95   Upper95
--------------  -----  --------  --------
Hospital Beds    5309      4371      6220
ICU Beds         2155      1775      2525
Ventilators      1494      1234      1747

# Caveats
Our main source of uncertainty in these models is generation time and R0, but all projections indicate that SD is at the very early stages of predicted exponential growth. That makes predictions in the future difficult to state with any certainty. As data are released, we will continue to update these projections semi-daily. 

At present, our data treat South Dakota as a homogenous mixture, though as of this writing most of the cases are concentrated in Minnehaha county. Future models that include regional projections may be warranted. 

Projections also assume a fixed hospitalization rate. This is a simplification that likely leads to conservative estimates in our model, which does not currently account for the fact that older infected persons are more likely to require hospitalization, ICU, or ventilator support at rates above 4%. Future age-structured projections will help to alleviate this uncertainty. 

# Notes
The predictions here are purely our own and may not reflect opinions of our state or our employers. We welcome feedback.
