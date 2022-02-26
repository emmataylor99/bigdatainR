### Political Cynicism Project
install.packages("car")
install.packages("Hmisc")
library(Hmisc)
library(tidyverse)
library(readr)


### import data
anes_timeseries_2020_csv_20210719 <- read_csv("C:/Users/emmat/OneDrive/Desktop/anes_timeseries_2020_csv_20210719.zip")
df <- anes_timeseries_2020_csv_20210719

### renaming variables
names(df)
df <- rename(df, elites_trust = V202411) ## higher = less trustworthy
df <-rename(df, corruption = V202425) ## higher = less perceived corruption
df <-rename(df, trust_gov = V201233) ## higher = less trust
df <-rename(df, fair_vote = V202219) ## higher = votes NOT counted fairly
df <-rename(df, gov_performance = V202427) ## worse is higher
df <-rename(df, officials_not_care = V202212) ## lower = officials don't care the most
df <-rename(df, elites_not_care = V202410) ## lower = politicians do not care
df <-rename(df, politicians_care_rich = V202415) ## lower = politicians only care about rich
df <-rename(df, no_say_ingov = V202213) ## lower = people have no say
df <-rename(df, rural_urban = V202355) ## 1 = rural; 2 = small; 3 = suburb; 4 = city
df <-rename(df, poli_interest = V202406) ## 1 = v interested, higher = less
df <-rename(df, elites_problem = V202412) ## lower = agree that politicians are the main problem
df <-rename(df, democracy_satisfaction = V202440) ## higher = less satisfied
df <-rename(df, gender = V201600) ## 0 = male, 1 = female
df <-rename(df, engagement = V202575) ## not using
df <-rename(df, registration = V201018) ## not using
df <-rename(df, primary = V201020) ## 0 = did not vote in primary, 1 = did vote
df <-rename(df, primary_choice = V201021) ## not using
df <-rename(df, did_vote = V202066) # 0 = did not (for whatever reason), 1 = did 
df <-rename(df, did_vote_pres = V202072) ## not using
df <-rename(df, vote_choice_pres = V202073) ## two different for third party & extreme models

###selection of relevant variables

df1 <- select(df, vote_choice_pres, did_vote_pres, did_vote, primary, registration,
      gender, democracy_satisfaction, elites_problem, poli_interest, rural_urban, no_say_ingov, politicians_care_rich,
      elites_not_care, officials_not_care, gov_performance, fair_vote, trust_gov, corruption, elites_trust)

### filtering out people who did not vote, creating data frame to use for models that predict vote choice

voters <- filter(df1, vote_choice_pres>=0, did_vote_pres==1, did_vote==4, primary>=0, registration>=0,
              gender>=0, democracy_satisfaction>=0, elites_problem>=0, poli_interest>=0, rural_urban>=0, no_say_ingov>=0, politicians_care_rich>=0,
              elites_not_care>=0, officials_not_care>=0, gov_performance>=0, fair_vote>=0, trust_gov>=0, corruption>=0, elites_trust>=0)

### duplicating vote choice column to create two separate models 
###(vote_choice_pres = third party vote model)
###(vote_choice_pres2 = extreme vote model)

voters <- voters %>% 
  mutate(vote_choice_pres2 = vote_choice_pres)

### creating a data set without questions that depend on "did you vote" for a vote or not model
voteornot <- select(df1, -vote_choice_pres, -did_vote_pres)
voteornot <- filter(voteornot, did_vote >= -1, primary>=0, registration>=0,
                    gender>=0, democracy_satisfaction>=0, elites_problem>=0, poli_interest>=0, rural_urban>=0, no_say_ingov>=0, politicians_care_rich>=0,
                    elites_not_care>=0, officials_not_care>=0, gov_performance>=0, fair_vote>=0, trust_gov>=0, corruption>=0, elites_trust>=0)


### THIRD PARTY VOTE CHOICE MODEL: replacing values in the vote choice column for the vote choice model that has Trump&Biden as 1 and all other candidates as 2
voters["vote_choice_pres"][voters["vote_choice_pres"] == 1] <- 0
voters["vote_choice_pres"][voters["vote_choice_pres"] == 2] <- 0
voters["vote_choice_pres"][voters["vote_choice_pres"] == 7] <- 0
voters["vote_choice_pres"][voters["vote_choice_pres"] == 3] <- 1
voters["vote_choice_pres"][voters["vote_choice_pres"] == 4] <- 1
voters["vote_choice_pres"][voters["vote_choice_pres"] == 8] <- 1
voters["vote_choice_pres"][voters["vote_choice_pres"] == 5] <- 1
voters["vote_choice_pres"][voters["vote_choice_pres"] == 11] <- 1
voters["vote_choice_pres"][voters["vote_choice_pres"] == 12] <- 1

### EXTREME VOTE CHOICE MODEL: replacing values in the second vote choice column so that Biden is 1 and everyone else is 2
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 1] <- 0
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 3] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 4] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 2] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 7] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 8] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 5] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 11] <- 1
voters["vote_choice_pres2"][voters["vote_choice_pres2"] == 12] <- 1

### VOTE OR NOT MODEL: replacing values in the did you vote question
voteornot["did_vote"][voteornot["did_vote"] == 1] <- 0
voteornot["did_vote"][voteornot["did_vote"] == 2] <- 0
voteornot["did_vote"][voteornot["did_vote"] == 3] <- 0
voteornot["did_vote"][voteornot["did_vote"] == -1] <- 1
voteornot["did_vote"][voteornot["did_vote"] == 4] <- 1


summary(voters)
summary(voteornot)

### Dropping registration from both data frames because question logic is too complex

voteornot1 <- select(voteornot, -registration)

voters1 <- select(voters, -registration)

### Recoding gender to 0 (male) and 1 (female); primary to 0 (no) and 1 (yes)
voteornot1["gender"][voteornot1["gender"] == 1] <- 0
voteornot1["gender"][voteornot1["gender"] == 2] <- 1

voters1["gender"][voters1["gender"] == 1] <- 0
voters1["gender"][voters1["gender"] == 2] <- 1

voteornot1["primary"][voteornot1["primary"] == 2] <- 0

voters1["primary"][voters1["primary"] == 2] <- 0

summary(voters1)
summary(voteornot1)

### Probit model for vote or not
voteornotmodel <- glm(did_vote ~ primary + gender + 
                        democracy_satisfaction + elites_problem + poli_interest + 
                        rural_urban + no_say_ingov + politicians_care_rich +
                        elites_not_care + officials_not_care + gov_performance + 
                        fair_vote + trust_gov + corruption + 
                        elites_trust, family = binomial(link = "probit"), data = voteornot1)

summary(voteornotmodel)
##dropping vars in next model: elites_problem; no_say_ingov; politicians_care_rich;
##dropping: officials_not_care; trust_gov
##keeping: primary, gender, democracy_satisfaction, poli_interest, rural_urban, 
##keeping: elites_not_care, gov_performance, fair_vote, corruption, elites_trust
voteornotmodel <- glm(did_vote ~ primary + gender + democracy_satisfaction  + 
                        poli_interest + rural_urban + elites_not_care +  gov_performance + 
                        fair_vote +  corruption + 
                        elites_trust, family = binomial(link = "probit"), data = voteornot1)
summary(voteornotmodel)
vif(voteornotmodel) ## variance inflation factor - measure of multicollinearity
                    ## low VIFs = good; variables explanatory, significance valid

### PROBIT FOR THIRD PARTY MODEL: using same I. vars., vote_choice_pres for third party D.
### This probit also now using voters1 dataframe
thirdpartymodel <- glm(vote_choice_pres ~ primary + gender + democracy_satisfaction  + 
                        poli_interest + rural_urban + elites_not_care + gov_performance + 
                        fair_vote + corruption + 
                        elites_trust, family = binomial(link = "probit"), data = voters1)
summary(thirdpartymodel)
vif(thirdpartymodel)

### PROBIT FOR EXTREME CANDIDATE MODEL: using same I. vars, vote_choice_pres2 now D. var.
### Extrememodel also using voters1 dataframe
extremistmodel <- glm(vote_choice_pres2 ~ primary + gender + democracy_satisfaction + 
                         poli_interest + rural_urban + elites_not_care + gov_performance + 
                         fair_vote + corruption + 
                         elites_trust, family = binomial(link = "probit"), data = voters1)
summary(extremistmodel)
vif(thirdpartymodel)

### plotting for associations of variables of interest

### plot with levels of political interest by voter status
voteinterestplot <- ggplot(voteornot1, aes(x=did_vote, y=poli_interest, group=did_vote)) +
  geom_boxplot(fill="white") +
  labs(x = 'Nonvoters vs. Voters', y='Level of Political Disinterest', title='Political Disinterest Amongst Nonvoters and Voters') +
  stat_summary(fun=mean, geom="point", shape=8, size=5) +
  theme_minimal()

voteinterestplot

### plot with third party voters and their dissatisfaction with democracy
thirdpartyplot <- ggplot(voters1, aes(x=vote_choice_pres, y=democracy_satisfaction, group=vote_choice_pres)) +
  geom_boxplot(fill="white") +
  labs(x = 'Mainstream Party Voters vs. Third Party Voters', y='Level of Dissatisfaction with Democracy', title='Political Dissatisfaction: Mainstream vs. Third Party') +
  stat_summary(fun=mean, geom="point", shape=8, size=5) +
  theme_minimal()

thirdpartyplot


