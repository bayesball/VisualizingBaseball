## Introduction to ggplot2
## ----------------------------------------------------------
#| warning: false
library(dplyr)
library(Lahman)
teams2015 <- filter(Teams, yearID == 2015)
names(teams2015)[18:19] <- c("X2B", "X3B")
teams2015$SF <- as.numeric(teams2015$SF)
teams2015$HBP <- as.numeric(teams2015$HBP)
teams2015 <- mutate(teams2015,
                    X1B = H - X2B - X3B - HR,
                    TB = X1B + 2 * X2B + 3 * X3B + 4 * HR,
                    SLG = TB / AB,
                    OBP = (H + BB + HBP) / 
                      (AB + BB + HBP + SF))


## ----------------------------------------------------------
library(ggplot2)
ggplot(data=teams2015, aes(x=OBP, y=SLG))


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=OBP, y=SLG)) +
  geom_point()


## ----------------------------------------------------------
ggplot(data=teams2015, 
       aes(x=OBP, y=SLG, color=lgID)) +
  geom_point()


## ----------------------------------------------------------
ggplot(data=teams2015, 
       aes(x=OBP, y=SLG, shape=lgID)) +
  geom_point()


## ----------------------------------------------------------
ggplot(data=teams2015, 
       aes(x=OBP, y=SLG, size=W)) +
  geom_point()


## ----------------------------------------------------------
ggplot(data=teams2015, 
       aes(x=OBP, y=SLG)) +
  geom_point() +
  facet_grid(~ lgID)


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=OBP)) + 
  geom_histogram(bins=5)


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=lgID)) + geom_bar()


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=lgID, y=SLG)) +
  geom_boxplot()


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=lgID, y=SLG)) +
  geom_jitter(width=0.1)


## ----------------------------------------------------------
ggplot(data=teams2015, aes(x=lgID, y=SLG)) +
  geom_jitter(width=0.1) +
  ggtitle("Slugging Percentages in the 2015 Season by League") +
  xlab("League") + ylab("Slugging Percentage")

## Chapter 1 - History of Baseball
## ----------------------------------------------------------
library(Lahman)
library(dplyr)
library(ggplot2)


## ----------------------------------------------------------
select(sample_n(Teams, 8), yearID, teamID, HR, G)


## ----------------------------------------------------------
S <- summarize(group_by(Teams, yearID),
               HR = sum(HR),
               G = sum(G))


## ----------------------------------------------------------
S2 <- filter(S, yearID >= 1900)


## ----------------------------------------------------------
ggplot(S2, aes(yearID, HR / G)) +
  geom_point() +
  geom_smooth(method="loess", span=0.3, se=FALSE)


## ----------------------------------------------------------
S <- summarize(group_by(Batting, yearID, playerID),
                 HR=sum(HR, na.rm=TRUE))


## ----------------------------------------------------------
S1 <- summarize(group_by(S, yearID),
                 HR=max(HR, na.rm=TRUE))
S2 <- filter(S1, yearID >= 1900)


## ----------------------------------------------------------
head(S2)


## ----------------------------------------------------------
ggplot(S2, aes(yearID, HR)) +
    geom_point() +
    geom_smooth(method="loess", span=0.3, se=FALSE)

## Chapter 2 - Career Trajectories
## ----------------------------------------------------------
plot_hr_trajectory <- function(playername){
  require(Lahman)
  require(dplyr)
  require(stringr)
  require(ggplot2)
  names <- unlist(str_split(playername, " "))
  info <- filter(People, nameLast==names[2],
                       nameFirst==names[1])

  bdata <- filter(Batting, playerID==info$playerID)
  bdata <- mutate(bdata,
          birthyear = ifelse(info$birthMonth >= 7, 
                  info$birthYear + 1, info$birthYear),
          Age = yearID - birthyear)

  ggplot(bdata, aes(yearID, HR / AB)) + 
    geom_point() +
    geom_smooth(method="loess", se=FALSE)
}


## ----------------------------------------------------------
p1 <- plot_hr_trajectory("Mickey Mantle")
p1


## ----------------------------------------------------------
p2 <- plot_hr_trajectory("Mike Schmidt")
p2


## ----------------------------------------------------------
ggplot(rbind(p1$data, p2$data), aes(Age, HR / AB)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  facet_wrap(~ playerID, ncol=1)

## Chapter 3 - Runs Expectancy
## ----------------------------------------------------------
library(readr)
library(knitr)
library(ggplot2)


## ----------------------------------------------------------
RR <- read_csv("https://bayesball.github.io/VB/data/runs2015.csv")


## ----------------------------------------------------------
kable(RR)


## ----------------------------------------------------------
ggplot(RR, aes(Bases, Mean, label=O)) +
    geom_point(size=3) + 
    geom_label(color="black", size=4,
               fontface="bold") +
    ylab("Runs Scored in \n Remainder of Inning") +
    xlab("Runners on Base") +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16))

## Chapter 4 - Count Effects
## ----------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)


## ----------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/count2015a.csv")
head(d)


## ----------------------------------------------------------
ggplot(d, aes(N.Pitches, Runs, label=count)) +
  geom_point() +
  geom_path(data=filter(d, strikes==0),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, strikes==1),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, strikes==2),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==0),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==1),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==2),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==3),
     aes(N.Pitches, Runs), color="blue") +
  xlab("Pitch Number") +
  ylab("Runs Value") +
  ggtitle("") +
  geom_hline(yintercept=0, color="black") +
  geom_label()


## ----------------------------------------------------------
S <- read_csv("https://bayesball.github.io/VB/data/count2015b.csv")
head(S)


## ----------------------------------------------------------
ggplot(S, aes(N.Pitches, Runs, label=count, size=N)) +
  xlab("Number of Pitch") +
  ylab("Runs Value") +
  geom_hline(yintercept=0, color="black") +
  geom_label()

## Chapter 5 - PITCHf/x Data
## ----------------------------------------------------------
library(readr)
CK <- read_csv("https://bayesball.github.io/VB/data/kershaw2016.csv")
head(CK)


## ----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)


## ----------------------------------------------------------
S_CK <- filter(summarize(group_by(CK, pitch_type),
                  N=n()),
            pitch_type %in% c("SL", "FF", "CU", "CH"))
ggplot(S_CK, aes(pitch_type, N)) +
  geom_point(size=3, color="blue") +
  coord_flip() +
  ggtitle("Frequencies of Pitch Type of Clayton Kershaw") +
  theme(plot.title = element_text(size = 14,
                hjust = 0.5))


## ----------------------------------------------------------
ggplot(filter(CK, pitch_type %in%
                c("SL", "FF", "CU", "CH")),
       aes(pitch_type, start_speed)) +
  geom_boxplot() + coord_flip() +
  ggtitle("Pitch Speeds") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5)) +
     ylim(70, 100)


## ----------------------------------------------------------
CK <- filter(CK, pitch_type %in% c("CU",
                          "FF", "SL"))
ggplot(CK,
  aes(pfx_x, pfx_z, shape=pitch_type)) +
  geom_point(color="blue", size=2, alpha=0.5) +
  ggtitle("Pitch Breaks") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5)) +
  xlab("Horizontal Break") + ylab("Vertical Break")


## ----------------------------------------------------------
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.85
outKzone <- 0.85
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)
ggplot(CK) +
  geom_point(data= filter(CK, pitch_type=="CU"),
             aes(px, pz), shape=1) +
  geom_point(data= filter(CK, pitch_type=="FF"),
             aes(px, pz), shape=2) +
  geom_point(data= filter(CK, pitch_type=="SL"),
             aes(px, pz), shape=3) +
  geom_path(aes(x, y), data=kZone, lwd=1, col="blue") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black")) +
  ggtitle("Pitch Locations") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))


## ----------------------------------------------------------
ggplot(CK) +
  geom_density_2d(aes(px, pz), color="black") +
  geom_path(aes(x, y), data=kZone, lwd=1, col="blue") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black")) +
  ggtitle("Pitch Locations") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))


## ----------------------------------------------------------
SO <- summarize(group_by(CK, pitch_type, des), N=n())
SO <- mutate(SO,
      Outcome=ifelse(str_detect(des, "Foul") == TRUE, "Foul",
      ifelse(str_detect(des, "Swing") == TRUE |
               des == "Missed Bunt", "Swing and Miss",
      ifelse(str_detect(des, "Ball") == TRUE, "Ball",
      ifelse(str_detect(des, "In play") == TRUE, "In play",
             des)))))
SOS <- summarize(group_by(SO, pitch_type, Outcome),
                 F=sum(N))
SOS1 <- summarize(group_by(SO, pitch_type),
                 Total=sum(N))
inner_join(SOS, SOS1) %>%
  mutate(Percentage = 100 * F / Total) -> SOS
ggplot(SOS,
        aes(Outcome, Percentage)) +
  geom_point(size=3, color="blue") +
  coord_flip() + facet_wrap(~ pitch_type, ncol=1) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black"))


## ----------------------------------------------------------
CK <- mutate(CK,
             Foul = str_detect(des, "Foul"),
             InPlay = str_detect(des, "In play"),
             Miss = str_detect(des, "Swing"),
             Swing = Foul | InPlay | Miss)
CK_swing <- filter(CK, Swing == TRUE)
ggplot(CK_swing, aes(px, pz, color=Miss)) +
  geom_point(alpha=0.75) +
  facet_wrap(~ pitch_type, ncol=2) +
  geom_path(aes(x, y), data=kZone, lwd=1, col="black") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  scale_colour_manual(values = c("gray60", "blue")) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black"))

## Chapter 6 - Batted Balls
## ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(Lahman)
library(readr)


## ----------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/homeruns.csv")
head(d)


## ----------------------------------------------------------
ggplot(d, aes(180 - Horiz_Angle)) +
  geom_density() +
  xlim(30, 180 - 30) +
  xlab("Horizontal Angle") +
  ylab("Density") +
  geom_vline(xintercept=90) +
  annotate("text", x=40, y=0.015,
           label="Left\nField", size=6) +
  annotate("text", x=140, y=0.015,
           label="Right\nField", size=6)


## ----------------------------------------------------------
ggplot(d, aes(180 - Horiz_Angle, True_Dist)) +
  geom_point(alpha=0.1) + geom_smooth() +
  ylim(300, 500) + xlim(45, 130) +
  xlab("Horizontal Angle") +
  ylab("Distance")


## ----------------------------------------------------------
Names <- str_split(d$Hitter, ",")
one_row <- function(j, k)
  str_trim(Names[[j]][k])
d$LastName <- sapply(1:24299, one_row, 1)
d$FirstName <- sapply(1:24299, one_row, 2)
d2 <- inner_join(d,
                 select(People, nameLast, nameFirst, bats),
                 by=c("LastName"="nameLast",
                      "FirstName"="nameFirst"))


## ----------------------------------------------------------
d2$Batting <- ifelse(d2$bats=="R",
                     "Right-Handed Hitter",
                     "Left-Handed Hitter")


## ----------------------------------------------------------
ggplot(filter(d2, bats=="R" | bats=="L"),
       aes(180 - Horiz_Angle)) +
  geom_density(size=1.0)  + xlim(45, 130) +
  xlab("Horizontal Angle") +
  ylab("Density") +
  facet_wrap(~ Batting, ncol=1) +
  theme(strip.text = element_text(face="bold", size=16))


## ----------------------------------------------------------
S <- summarise(group_by(d2, Ballpark),
               NL=sum(180 - Horiz_Angle < 90),
               NR=sum(180 - Horiz_Angle > 90),
               PL=NL / (NL + NR))


## ----------------------------------------------------------
ggplot(filter(S, NL + NR > 200), aes(Ballpark, PL)) +
  geom_point() + coord_flip() +
  ylab("Proportion of Home Runs to Left") +
  geom_hline(yintercept = 0.5)


## ----------------------------------------------------------
S200 <- filter(S, NL + NR > 200)
S200 <- arrange(S200, desc(PL))
Sextreme <- rbind(slice(S200, 1:8),
                  slice(S200, 28:31))
ballparks <- as.character(arrange(Sextreme, PL)$Ballpark)
d2$Ballpark <- factor(d2$Ballpark,
                      levels=ballparks)


## ----------------------------------------------------------
ggplot(filter(d2, bats=="R" | bats=="L",
              Ballpark %in% Sextreme$Ballpark),
       aes(180 - Horiz_Angle)) +
  geom_density() +
  facet_wrap(~ Ballpark, ncol=4) +
  geom_vline(xintercept = 90, color="blue") +
  xlab("Horizontal Angle") + ylab("Density")

## Chapter 7 - Plate Discipline
## ----------------------------------------------------------
library(tidyverse)
library(ggplot2)


## ----------------------------------------------------------
d1 <- read_csv("https://bayesball.github.io/VB/data/Dashboard_2016.csv")
d2 <- read_csv("https://bayesball.github.io/VB/data/Plate_Discipline_2016.csv")
d <- inner_join(d1, d2, by="playerid")
vars <- c(14, 25:33)
d_subset <- d[, vars]
names(d_subset) <- c("OBP", "O_Swing", "Z_Swing", "Swing",
                     "O_Contact", "Z_Contact",
                     "Contact", "Zone",
                     "F_Strike", "SwStr")
names(d)[c(14, 25:33)] <- names(d_subset)


## ----------------------------------------------------------
ggplot(d, aes(Swing, Contact)) +
  geom_point(size=2) +
  geom_smooth(se=FALSE) +
  xlab("Swing Rate") + ylab("Contact Rate")


## ----------------------------------------------------------
d$K_Rate <- with(d, ifelse(K > .1875, "HI", "LO"))
d$y <- ifelse(d$K_Rate=="HI", 1, 0)
glm(y ~ Contact + Swing, data=d, family=binomial) -> F
ggplot(d, aes(Swing, Contact, 
              color=K_Rate)) +
  geom_point(size=3) +
  xlab("Swing Rate") + ylab("Contact Rate") +
  geom_abline(intercept = coef(F)[1] / (-coef(F)[2]),
              slope = coef(F)[3] / (-coef(F)[2])) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("black", "grey60"))


## ----------------------------------------------------------
d$BB_Cat <- with(d, ifelse(BB > .082, "HI", "LO"))
d$y <- ifelse(d$BB_Cat=="HI", 1, 0)
glm(y ~ Contact + Swing, data=d, family=binomial) -> F
ggplot(d, aes(Swing, Contact, 
              color=BB_Cat)) +
  xlab("Swing Rate") + ylab("Contact Rate") +
  geom_point(size=3) +
  geom_abline(intercept = coef(F)[1] / (-coef(F)[2]),
              slope = coef(F)[3] / (-coef(F)[2])) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("black", "grey60"))


## ----------------------------------------------------------
d <- mutate(d,
            K_Type=ifelse(K < .12, "TOP",
                  ifelse(K > .25, "BOTTOM", NA)))
select(filter(d, K_Type == "TOP"),
       Name.x, Team.x, K)
select(filter(d, K_Type == "BOTTOM"),
       Name.x, Team.x, K)


## ----------------------------------------------------------
d <- mutate(d,
            BB_Type=ifelse(BB > .13, "TOP",
                  ifelse(BB < .05, "BOTTOM", NA)))
select(filter(d, BB_Type == "TOP"),
       Name.x, Team.x, BB)
select(filter(d, BB_Type == "BOTTOM"),
       Name.x, Team.x, BB)


## ----------------------------------------------------------
ggplot(filter(d, K_Type %in% c("TOP", "BOTTOM")),
        aes(Z_Contact, O_Contact, color=K_Type)) +
         geom_point(size=3) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("grey50", "black" ))


## ----------------------------------------------------------
ggplot(filter(d, BB_Type %in% c("TOP", "BOTTOM")),
        aes(Z_Swing, O_Swing, color=BB_Type)) +
         geom_point(size=3) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("grey50", "black" )) 

## Probability and Modeling
## ----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)


## ----------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/WSGame7.csv")
d$Play_Number <- 1:dim(d)[1]
d$WE  <- as.numeric(str_replace(d$WE, "%", ""))
head(d)


## ----------------------------------------------------------
ggplot(d, aes(Play_Number, WE / 100)) +
  geom_point(size=2) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("") +
  ylab("Probability Indians Win") +
  geom_hline(yintercept = .50, color="blue", size=1.5) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=0.90,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=0.98, 
           label="INNING", size=6) +
  xlab("Play Number")


## ----------------------------------------------------------
ggplot(d, aes(Play_Number, LI)) +
  geom_segment(aes(xend = Play_Number, yend = 0),
               size = 2, lineend = "butt") +
  xlab("Play Number") +
  ylab("Leverage")  +
  ylim(0, 5.8) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=5,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=5.5, label="INNING", size=6) 


## ----------------------------------------------------------
ggplot(d, aes(Play_Number, WPA)) +
  geom_segment(aes(xend = Play_Number, yend = 0),
               size = 2, lineend = "butt") +
  xlab("Play Number") +
  ylab("Win Probability Added") +
  ylim(-0.24, 0.6) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=0.53,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=0.60, label="INNING", size=6) +
  annotate('text', x=71, y=0.45, label="Davis\nHR") +
  annotate('text', x=85, y=0.38, label="Zobrist\n2B") +
  annotate('text', x=77, y=-0.22, label="Baez\nSO") 

## Chapter 9 - Streakiness and Clutch Play
## ----------------------------------------------------------
library(BayesTestStreak)
library(gridExtra)


## ----------------------------------------------------------
moving_average_plot


## ----------------------------------------------------------
walker_id <- find_id("Neil Walker")
aoki_id <- find_id("Nori Aoki")


## ----------------------------------------------------------
walker <- streak_data(walker_id, pbp2016, "H", AB=TRUE)
aoki <- streak_data(aoki_id, pbp2016, "H", AB=TRUE)


## ----------------------------------------------------------
plot_streak_data(walker) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Walker")


## ----------------------------------------------------------
plot_streak_data(aoki) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")


## ----------------------------------------------------------
walker_s_data <- moving_average(walker, 50)
moving_average_plot(walker_s_data) +  
  theme(plot.title = element_text(colour = "blue", 
                                  size = 18, 
        hjust = 0.5)) + ggtitle("Walker")


## ----------------------------------------------------------
aoki_s_data <- moving_average(aoki, 50)
moving_average_plot(aoki_s_data) + 
  theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")


## ----------------------------------------------------------
p1 <- moving_average_plot(walker_s_data) + 
  ylim(.1, .5) +
  annotate("text", x=200, y=0.45,
           label="Neil Walker", size=7) +
  ylab("Moving Average") + xlab("") 
p2 <- moving_average_plot(aoki_s_data) + 
  ylim(.1, .5) +
  annotate("text", x=200, y=0.45,
           label="Nori Aoki", size=7) +
  ylab("Moving Average") + xlab("At Bat Number")
grid.arrange(p1, p2)


## ----------------------------------------------------------
sp <- find_spacings(walker)
geometric_plot(sp) + 
  theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Walker")


## ----------------------------------------------------------
sp <- find_spacings(aoki)
geometric_plot(sp) + 
  theme(plot.title = element_text(colour = "blue", 
                                  size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")

