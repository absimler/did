### BIOL 597: Difference-in-differences, Part II: Time-varying confounds ########

## Please load the following:
library(marginaleffects) 
library(tidyverse)  
library(broom)  
library(ggeffects) ## new one! Generate predictions from fitted models!

wildlife <- read.csv("wildlife.csv")



##### Part II: Time-varying confounds!

# We discussed in class that, in DiD, the estimation of the treatment effect relies on extrapolating where the treatment group would have been in the “after” timepoint, using the control group’s trajectory. This means we assume that the control and treatment groups would have shared PARALLEL trends if nothing had happened.

## this parallel trend assumption holds, so long as the effect of the time-invariant covariates on the outcome is constant over time.... but time-varying effects/relationships can create confounds in difference-in-differences estimation. Let's consider two scenarios, which will be represented using new versions of our response variable ("collisions.v2" and "collisions.v3")...


## Scenario 1) A variable has a time-varying effect on the outcome  #####
# Even if a variable is constant over time (e.g., elevation, or something like that), its effect on the outcome might vary over time.

# For instance, let's assume that our that the "percent protected areas" variable is essentially time-invariant because there was negligible change in the amount of protected area between 2020 and 2022. Even if this variable is time-invariant, if the effect of protected areas changes between our pre- and post-observations. For instance, let's say there's a drought in 2022, so the ecological impact of the protected area on wildlife populations is reduced (due to altered food quality or something like that), meaning that the linkage between protected habitat and wildlife collisions shifts between 2020 and 2022. This can create a confound that simple DiD will not be able to consider.

## the "Collisions.v2" variable was simulated using a generative model based on the scenario above, in which the effect of protected area proportion changes over time.


# Let's refit the same model as in the last exercise, but with this new version of the response:

wildlife <- wildlife %>%
  mutate(time = ifelse(year==2020, 0, 1))

mod.did.tve <- lm(collisions.v2~ time + corridor + time*corridor, data=wildlife)
summary(mod.did.tve)



## Discuss:
## a) How does the estimation of the "true" treatment effect (-20) change, compared to the version of the response in which all confounds had constant effects over time?
## b) How might you consider this in an analysis?










## The bad news is that this sort of scenario "breaks" DiD for unmeasured variables. However, if we DO have a measurement for the variable with a time-varying effect, including an interaction term between that variable and time will allow us to once again estimate the true treatment effect ###
mod.tve.control <- lm(collisions.v2~time + corridor + 
                        time*corridor + time*protectarea, data=wildlife)
summary(mod.tve.control)





### Scenario 2) Time-varying confounders ######
## Another time-varying scenario that might challenge simple DiD is if the confounders aren't actually time-invariant. Time-varying confounders "break" DiD because, if the rate of change for the variable differs between the treated and untreated groups, the differences between groups are no longer constant across the before and after time points. This means that we can't use the control group to calculate the counterfactual trajectory for the treated sites, if they had been untreated.

## Time-varying confounds are ubiquitous and might include things like weather, or, in our case, human population density. In our example, let's pretend that human populations grew MORE in the areas that had corridors, compared to the areas that did not receive corridors. This scenario is simulated using the "collisions.v3" and "humans.varying" variables.


# Let's visualize the "humans.vary" variable, in which human population density changes over time, AND it changes differently between the treated and untreated groups:

ggplot(wildlife, aes(x = as.factor(time), y = humans.vary, 
                  fill = as.factor(corridor)))+
  geom_boxplot() +
  scale_fill_manual(values=c("#4c156b", "#3ea37c"), 
                    name="Treatment group",
                    labels=c("No corridor", "Corridor"))+
  ylab("Human population density (people/km^2)")+
  xlab("Time (before/after treatment)")+
  theme_bw()



# Let's refit the same model as in the last exercise, but with this new version of the response, which assumes human density varies over time (between 2020 and 2022):
mod.did.vary <- lm(collisions.v3~time + corridor + time*corridor , data=wildlife)
summary(mod.did.vary)



## Discuss:
## a) How does the estimation of the "true" treatment effect (-20) change, compared to the version of the response in which all confounds had constant effects over time?
## b) How might you consider this in an analysis?












## As above, the bad news is that this sort of scenario "breaks" DiD for unmeasured variables. However, if we DO have a measurement for the time-varying variable, we can include it directly into our regression and esimate the true treatmenet effect (even if we don't incorporate the other time-invariant variables)  ###

mod.did.vary.control <- lm(collisions.v3~time + corridor + 
                             time*corridor + humans.vary, data=wildlife)
summary(mod.did.vary.control)
  
