### BIOL 597: Difference-in-differences, Part I: Time-invariant confounds: ######

## Please load the following:
library(marginaleffects) 
library(tidyverse)  
library(broom)  
library(ggeffects) ## new one that needs installing! Generate predictions from fitted models!

wildlife <- read.csv("wildlife.csv")

## The "wildlife" dataset contains information about the average number of wildlife collisions/year before (in 2020) and after (in 2022) corridors were installed in the study region. Collisions were recorded at 100 different sites. The dataset contains the following variables:

## corridor = whether or not a site received a corridor (1=treated) or not (0=untreated).
## collisions = the number of collisions per year per 1 km segment
## roads = the mean WIDTH of the road at each site. This variable is time-invariant.
## protectarea = the percentage of the area around each site (500m radius) that is designated as "protected" area (e.g. state, federal, or local level)
## humans = the density of human population (people/km^2) in the area (500m radius) surrounding the site
## year = whether the observation was before (2020) or after (2022) corridors were built.

## There are three more variables (collisions.v2, collisions.v3, and humans.vary) that we will discuss in the next exercise -- ignore them for now!



#### 1) Visualize differences between control and treatment groups: #####
## I've included a script from our previous exercise in this repository (Visualizing balance in treatment groups.R). Steal some of that code (or use an approach of your choosing) to examine whether the sites that received corridors and those that did not receive corridors differed in terms of road width, surrounding wildlife habitat, and surrounding human population density.

## Discuss with a partner:
## a) Which approaches did you use to check for balance in treatment assignment and why?
## b) Based on your results, which variables may represent potential confounders? Does this align with the DAG on the board?








#### 2) Fit the "naive" model and a DAG driven model #########

## The wildlife dataset above was simulated according to the following assumptions:
# 1) Probability of corridor treatment increases with human density, road width, and % protected areas surrounding the site.
# 2) Number of collisions increases with human density, road width, and % protected areas surrounding the site, meaning that
# 3) Human density, road width, and protected area variables are "time-invariant" (their values and effects do not change between 2020-2022).
# 4) The "true" treatment effect for the corridors is -20 (corridors reduce collisions by 20).



## Now that we've identified the potential confounds, let's fit the "naive" regression model, just to have a point of comparison. This model will just use the "after" treatment observations and estimate a difference in means between the treated and untreated groups:

mod.naive <- lm(collisions~corridor, data=subset(wildlife, year==2022))
summary(mod.naive) # Take note of how well the naive model estimates the treatment effect.



## Next, let's fit a model that contains the possible confounding variables:
mod.daggy <- lm(collisions~corridor + humans + protectarea + roads, 
                data=subset(wildlife, year==2022))
summary(mod.daggy)


## Pause here to discuss as a class.






#### 3) Fit a difference-in-differences model ############

## Above, you discussed which time-invariant variables were systematically different between treated and untreated sites. In our last few lectures, we learned how to incorporate those variables (using DAGs, Matching, IPW) to minimize the risk of confounding. In a perfect world, where you know your data generating process and have observed all of the variables that influence it, the DAG-driven model should do a great job isolating the true treatment effect (as illustrated above). 


## But for this next exercise, let's assume we don't have good data for road width, human population density, and wildlife habitat surrounding our sites (which is a realistic situation in many studies). In other words, let's pretend these are "unmeasured" possible confounders, that we can't incorporate into our regression using the DAG-driven approach you just applied above. Instead, we'll use Difference-in-differences.


## Difference-in-differences leverages before- and after-treatment observations to "difference" away the time-invariant differences between groups. In short, DiD is comparing observations to themselves; therefore, the potential for confounding due to omitted variables is minimized because time-invariant differences between treated and untreated "drop out" of the model. 


## Difference in differences can be estimated using a simple regression structure, as discussed in lecture:

# outcome ~ group + time + time*group

##... where each of these variables is categorical. 
## Group is in the form of 0/1, where 0 = untreated and 1= treated.
## Time should be in the form of 0/1, where 0 = the "before" treatment period and 1 is the "after" treatment period.

# This makes the time*group an indicator for whether or not treated groups have been treated yet. When time = 0, this term is 0 by default, but when time = 1 and group = 1, this term gets "switched" on. Since this is an interaction term, it communicates how much the effect of being in the "treatment group (1)" changes between time = 0 and time = 1 (a.k.a. before and after treatment). Therefore, the parameter associated with "time*group" IS our estimated treatment effect. Cool!

## First, let's do a little data wrangling to make our "year" variable into a binary time variable, where 0 = pre-treatment and 1 = post-treatment.

wildlife <- wildlife %>%
              mutate(time = ifelse(year==2020, 0, 1))


# We also can do a little visualization to see how the before/after trends for our treated and untreated groups might differ.

ggplot(wildlife, aes(x = as.factor(time), y = collisions, 
                  fill = as.factor(corridor))) +
  geom_boxplot() + 
  scale_fill_manual(values=c( "#4c156b", "#3ea37c"), 
                     name="Treatment group",
                     labels=c("No corridor", "Corridor"))+
  ylab("Number of wildlife collisions/year")+
  xlab("Time (before/after treatment)")+
  theme_bw()



## Now, to estimate our DiD, all we need to do is fit the following regression:
mod.did <- lm(collisions~time + corridor + time*corridor, data=wildlife)
summary(mod.did)


# Discuss:
# a) How well did the DiD estimate the 'true' treatment effect? What does each variable in the regression above communicate? 
# b) How does estimation of the treatment effect compare to the naive and DAG-driven models above?


## We can also visualize what our DiD regression coefficients are communicating, using the ggpredict() package, which will create predictions from our fitted model:
predictions <- ggpredict(mod.did, ci.lvl=0.95, terms=c("time", "corridor"))


##We can plot our predictions for the treated and untreated groups over time:
ggplot(predictions, aes(x = as.factor(x), y =predicted, color=group)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_line(aes(group = group), size=1) +
  scale_color_manual(values=c( "#4c156b", "#3ea37c"), 
                     name="Treatment group",
                     labels=c("No corridor", "Corridor"))+
  ## This part just plots the prediction for where the corridor sites WOULD have been in 2023, if they had not received treatment, based on the untreated group's time trend:
  annotate(geom = "segment", x = "0", xend = "1",
           y = coef(mod.did)[1] + coef(mod.did)[3], 
           yend = coef(mod.did)[1] + coef(mod.did)[3] + coef(mod.did)[2] ,
           linetype = "dashed", color = "#3ea37c") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = coef(mod.did)[1] +  coef(mod.did)[2], 
           yend = coef(mod.did)[1] + coef(mod.did)[3] + coef(mod.did)[2] ,
           linetype = "solid", color = "orangered3", 
           arrow = arrow(ends="both", length = unit(.3,"cm")),
           size=1) +
  annotate(geom = "text", x = 1 + 1.25, y = coef(mod.did)[1] +  coef(mod.did)[2] + 10,
           label = "Treatment\neffect = -20", size = 3) +
  ylab("Number of collisions/year") +
  xlab("Time (Before/After Treatment)") +
  theme_bw()


## Discuss:
# b) What does the figure above tell us about the preexisting differences between treated and untreated groups?



