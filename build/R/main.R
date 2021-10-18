# CODE FOR TURNOVER PREDICTION --- [9-10-21]
# Author: Zeralm
# R VERSION: 4.1.1 (2021-08-10) (Kick Things)

# The following code generates descriptive statistics and basic plots, cleans 
# data and performs some hypothesis tests.
# --------------------------------------------------------------
# DESCR FUNCTIONS


# --------------------------------------------------------------

libs <- c("dplyr", "ggplot2", "readr", "egg", "reshape2", "DescTools", "survival", "ggfortify")
install.packages(libs[!(libs %in% installed.packages())])
lapply(libs, library, character.only = TRUE)
data <- read_csv("data/turnover-data-set_utf.csv", show_col_types = FALSE, 
                 col_types = "dffifffffffddddd") # Automatically factorizes

# --------------------------------------------------------------

# - CLEANING
data <- data[!duplicated(data) & !apply(is.na(data), 1, any),] # Gets rid of dupl rows
levels(data$event) = c("Quit", "Stayed")
summary(data)

# - FIRST PLOTS

# Age distr
graph1 <- ggplot(data, mapping = aes(x = age)) + geom_density() + facet_grid(event~.) + 
  labs(title = "Age distribution") + 
  theme(plot.title = element_text(hjust = 0.5), strip.text.y = element_text(angle = 0), strip.background = element_rect(fill = "white"))
graph1
# experience vs age
graph2.1 <- data[data$event == "Stayed",] %>% group_by(age) %>% summarize(mean(stag)) %>% 
  rename(avg_stag = `mean(stag)`) %>% 
  ggplot(mapping = aes(x = age, y = avg_stag)) + geom_col(fill = "Grey")  +  
  labs(title = "Average experience, people who stayed", x = "Age", y = "Experience") + 
  theme(plot.title = element_text(hjust = 0.5))

graph2.2 <- data[data$event == "Quit",] %>% group_by(age) %>% summarize(mean(stag)) %>% 
  rename(avg_stag = `mean(stag)`) %>% 
  ggplot(mapping = aes(x = age, y = avg_stag)) + geom_col(fill = "Grey")  +  
  labs(title = "Average experience, people who left", x = "Age", y = "Experience") + 
  theme(plot.title = element_text(hjust = 0.5))

graph3 <- melt(data %>% select(event,(extraversion:novator))) %>% 
  ggplot(mapping = aes(x = variable, y = value, fill = event)) + 
  stat_summary(fun = mean, position = "dodge" ,geom = "bar", color = "black") + labs(title = "Big 5 scores") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(color = "white"), legend.title = element_blank(), legend.margin = margin(3,1,3,6)) +
  scale_fill_brewer(palette = "Greys")
graph3
graph4 <- data %>% ggplot(mapping = aes(x = age, event)) + geom_boxplot(aes(fill = event), color = "black", show.legend = FALSE) + 
  coord_flip() + labs(title = "Age distribution", x = "Age") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.title = element_blank()) +
  scale_fill_brewer(palette = "Greys")

graph5 <- data %>% ggplot(mapping = aes(x = industry)) + 
  geom_bar(aes(fill = event), color = "black", position = "dodge", show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5) ,axis.title.y = element_blank(), legend.title = element_blank()) + 
  labs(title = "Number of people per industry", x = "") + scale_x_discrete(guide = guide_axis(n.dodge = 3)) + 
  scale_fill_brewer(palette = "Greys")

graph6 <- data %>% ggplot(mapping = aes(x = profession)) + 
  geom_bar(aes(fill = event), color = "black", position = "dodge", show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_blank(), legend.title = element_blank()) +
  labs(title = "Number of people per profession", x = "") + scale_x_discrete(guide = guide_axis(n.dodge = 3)) + 
  scale_fill_brewer(palette = "Greys")

graph7 <- data %>% ggplot(mapping = aes(x = way)) + 
  geom_bar(aes(fill = event), color = "black", position = "dodge", show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_blank(), legend.title = element_blank()) +
  labs(title = "Commute choice", x = "") + scale_fill_brewer(palette = "Greys")

graph8 <- melt(data %>% select(event, age, stag)) %>% ggplot(mapping = aes(x = variable, y = value, fill = event)) +
  stat_summary(fun = mean, color = "black", position = "dodge", geom = "bar", show.legend = FALSE) + labs(title = "Average age and experience", x = "", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
  scale_fill_brewer(palette = "Greys")


melt(data %>% select(event ,age, stag))

ggarrange(graph1, graph2.1, graph2.2, nrow = 3, widths = c(2, c(1, 1)))
ggarrange(graph4 ,graph5, graph3, graph6, graph8, graph7)

# - STATISTICAL TESTS

# H0: B1 = B2 = B3 = B4 = B5 = 0
# H1: B1 != 0 or B2 != 0, or both, or..., or all

big5_event <- data[data$event == "Quit",  names(data) %in% c("independ", "anxiety", "extraversion", "novator", "selfcontrol")]
big5_no_event <- data[data$event == "Stayed",  names(data ) %in% c("independ", "anxiety", "extraversion", "novator", "selfcontrol")]
big5.pvalue <- HotellingsT2Test(big5_event, big5_no_event)$p.value

# H0: p = 0.5 (probability of leaving)
# H1: p < 0.5

male_event = dim(data[(data$event == "Quit") & (data$gender == "m"),  "gender"])[1]
len_male_evth = dim(data[data$gender == "m", "gender"])[1]
male.pvalue <- pbinom(male_event, len_male_evth, 0.5) #Cannot reject


female_event = dim(data[(data$event == "Quit") & (data$gender == "f"),  "gender"])[1]
len_female_evth = dim(data[data$gender == "f","gender"])[1]
female.pvalue <- pbinom(female_event, len_female_evth, 0.5) #Cannot reject

head_male_event = dim(data[(data$event == "Quit") & (data$head_gender == "m"),  "head_gender"])[1]
len_head_male_evth = dim(data[data$head_gender == "m","head_gender"])[1]
headmale.pvalue <- pbinom(head_male_event, len_head_male_evth, 0.5) #Cannot reject

head_female_event = dim(data[(data$event == "Quit") & (data$head_gender == "f"),  "head_gender"])[1]
len_head_female_evth = dim(data[data$head_gender == "f","head_gender"])[1]
headfemale.pvalue <- pbinom(head_female_event, len_head_female_evth, 0.5) #Cannot reject

# H0: B1 = 0
# H1: B1 != 0

age_event = data[data$event == "Quit", "age"]
age_no_event = data[data$event == "Stayed", "age"]
age.pvalue <- t.test(age_event, age_no_event)$p.value #Cannot reject

stag_event = data[data$event == "Quit", "stag"]
stag_no_event = data[data$event == "Stayed", "stag"]
stag.pvalue <- t.test(stag_event, stag_no_event)$p.value #Cannot reject


pvalues = c(big5.pvalue, male.pvalue, female.pvalue, headmale.pvalue, headfemale.pvalue, age.pvalue, stag.pvalue)
testnames = c("Hoteling's t-test: Big 5" , "binomial test: males", "binomial test: females", "binomial test: head males", "binomial test: head females", "t-test: age", "t-test: stag")
significant_at_0.5 = pvalues < 0.05

data.frame(testnames,pvalues, significant_at_0.5)

# - Survival analysis

# Why survival analysis and not logistic regression:
# - The coefficients on tenure are not useful. The variable is mismanaged. 
# We don't want to predict turnover of current employees, therefore current tenure is irrelevant.
# - Survival analysis allows you to be precise and detect breakpoints and trends
# - If you actually want to test for breakpoints or changes in slope (splines), 
# you should probably plot the dataset like a distribution of tenure-survival. 
# That is almost survival analysis already.
# - It will be more accurate answering our questions on turnover. 

newdat <- read_csv("data/turnover-data-set_utf.csv", show_col_types = FALSE, 
                 col_types = "dififffffffddddd") 
newdat <- newdat[!duplicated(newdat) & !apply(is.na(newdat), 1, any),]

train <- newdat[sample(nrow(newdat), nrow(newdat) * 0.7), ]
test <- newdat[setdiff(seq_len(nrow(newdat)), sample(nrow(newdat), nrow(newdat) * 0.7)), ]

objSurv <- with(train, Surv(stag, event))
survfit(objSurv ~ 1)
survplot1 <- autoplot(survfit(Surv(stag, event) ~ 1, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))
survplot2 <- autoplot(survfit(Surv(stag, event) ~ gender, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))
survplot3 <- autoplot(survfit(Surv(stag, event) ~ way, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))
survplot4 <- autoplot(survfit(Surv(stag, event) ~ head_gender, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))
survplot5 <- autoplot(survfit(Surv(stag, event) ~ greywage, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))
survplot6 <- autoplot(survfit(Surv(stag, event) ~ coach, data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))

ggarrange(survplot1, survplot2, survplot3, survplot4, survplot5, survplot6)

survplot7 <- autoplot(survfit(Surv(stag, event) ~ 1 ,data = newdat)) + labs(title = "Survival curve") + theme(plot.title = element_text(hjust = 0.5))

survplot7

colnames(newdat)

cox <- coxph(Surv(stag, event) ~ 1 +  gender + age + industry + 
               + greywage + way + extraversion + independ + selfcontrol + 
               anxiety + novator+ coach + head_gender, data = newdat)

summary(cox)
autoplot(survfit(cox)) + labs(title = "Survival curve, cox model")


aa_fit = aareg(Surv(stag, event) ~ 1 +  gender + age + 
                  greywage + way + extraversion + independ + selfcontrol + 
                  anxiety + novator + coach + head_gender, data = newdat)

autoplot(aa_fit) + labs(title = "Coeficients on hazard over time")


# Aalen model
# Some hazard effects clearly vary over time (a(t) - XB(t)), the cox implementation should be taken with a grain of salt
# Grey wage is a predictor of quitting
# Going to work by foot is a negative predictor of quitting
# Age is a predictor of quitting on longer tenures
# Higher conscientiousness is associated to lower turnover in the first years.

# Recommendation: hire in surroundings, offer help in accommodation
# People with longer tenure and age are at risk. Maybe senior promotion is not
# encouraged, or salary for more senior roles is not competitive, so people
# with more experience are more prone to leave.
# Look for signals of conscientiousness when hiring (psychometric tests are likely to
# be tricked in conscientiousness)


# References:
# Cox DR. A note on the graphical analysis of survival data. Biometrika. 1979;66:188–190
# Kaplan EL, Meier P. Nonparametric estimation from incomplete observations. J Am Stat Assoc. 1958;53:457–481