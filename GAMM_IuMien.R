#GAMM on Iu Mien tone generational and real time differences
#phonetic/social fixed factors are 
#duration, sex, age, style
#random smooths for Speaker and Syllable
# references (from James):
# https://arxiv.org/abs/1703.05339
# https://psyarxiv.com/bd3r4/
# https://github.com/uasolo/FPCA-phonetics-workshop

library(tidyverse)
library(readxl)
library(mgcv)
library(itsadug)
library(gridExtra)
library(cowplot)

theme_set(theme_minimal())

#load the data (if not already loaded)
data <- read_csv("....csv")

#Combine the 1988 data with the 2022 data
##add a column "Year of birth" (YOB): 
data <- data %>%
  mutate(YOB = 2022 - Age, 
         YOR = 2022)

#add a column "Style" to the 1988 data: 
data_1988$YOB = as.numeric(data_1988$YOB)

data_1988 <- rename(data_1988, Time_norm = Time)

data_1988$Speaker = as.numeric(data_1988$Speaker)

data_1988$Speaker <- data_1988$Speaker + 41

data_1988 <- data_1988 %>% 
  mutate(Age = 1988 - YOB, 
         Style = "citation", 
         Location = "Huai Maesaai",  
         `Birth Location` = "Huai Maesaai", 
         Hometown = "Huai Maesaai", 
         YOR = 1988, 
         `Initial Consonant` = "m", 
         Initial = "sonorant")

#combine: 
data_all <- rbind(data, data_1988)

#filter out outlier: 

data_all <- data_all %>%
  filter(!(Speaker == 64 & Token == "maj4"))

#count tokens for 1988
tok <- data_all %>%
  filter(YOR==1988)%>%
  group_by(Token, Speaker) %>%
  ungroup() %>%
  group_by(decade) %>%
  summarise(T1 = sum(Tone==1),
            T2 = sum(Tone==2),
            T3 = sum(Tone==3),
            T4 = sum(Tone==4),
            T5 = sum(Tone==5), 
            T6 = sum(Tone==6)) %>%
  mutate(Total = T1+T2+T3+T4+T5+T6)

#count tokens for 2022
tok <- data_all %>%
  filter(YOR==2022)%>%
  group_by(Token_Number, Speaker) %>%
  filter(Time_norm == min(Time_norm))%>%
  ungroup() %>%
  group_by(Style) %>%
  summarise(T1 = sum(Tone==1),
            T2 = sum(Tone==2),
            T3 = sum(Tone==3),
            T4 = sum(Tone==4),
            T5 = sum(Tone==5), 
            T6 = sum(Tone==6)) %>%
  mutate(Total = T1+T2+T3+T4+T5+T6)

## center time so that intercept will represent mean f0 at rime midpoint
## do this on times_norm because rimes differ in duration
data_all$times_c = scale(data_all$Time_norm, center=TRUE, scale=FALSE)[,1]

## Token must be a factor for mgcv
data_all$Token <- as.factor(data_all$Token)

data_all <- data_all %>%
  mutate(decade = case_when(YOB <=1939 ~ 1930,
                               YOB <=1949 ~ 1940,
                               YOB <=1959 ~ 1950, 
                            YOB <=1969 ~ 1960,
                            YOB <=1979 ~ 1970, 
                            YOB <=1989 ~ 1980,
                            YOB <=1999 ~ 1990,
                            YOB <=2009 ~ 2000, 
                            YOB <=2019 ~ 2010))

data_all <- rename(data_all, Age_group = `Age Group`)

#Figure 2: Plot the tone system in 1988

T1988 <- data_all %>% 
  filter(YOR==1988) %>%
  #filter(Tone=="3"|Tone=="4"|Tone=="5")%>%
  filter(decade=="1940")%>%
  mutate(decade = factor(decade))  %>%
  #mutate(Sex = factor(Sex, levels = c("F", "M"), 
                      #labels = c("Male", "Female")))  %>%
  ggplot(aes(x=Time_norm, 
             y=Semi_Tone,
             color=Tone))+
  scale_color_manual(labels = c("T1 mid","T2 mid falling","T3 high rising-falling","T4 low rising-falling",
                                "T5 mid rising","T6 low falling"), 
                     values = c("grey", "yellow","green", "red","blue","orange"))+
  theme_bw()+
  #facet_wrap(~Sex)+
  geom_smooth(method="lm", formula = y ~ poly(x, 3), se=FALSE)+
  ylab("f0 (semitones)")+
  xlab("Time (normalized)")

#Figure 5: Plot the tone system in 2022
T2022 <- data_all %>% 
  filter(YOR==2022, Style=="citation") %>%
  #filter(Tone=="3"|Tone=="4"|Tone=="5")%>%
  filter(decade=="1940")%>%
  mutate(decade = factor(decade))  %>%
  #mutate(Sex = factor(Sex, levels = c("F", "M"), 
  #labels = c("Male", "Female")))  %>%
  ggplot(aes(x=Time_norm, 
             y=Semi_Tone,
             color=Tone))+
  scale_color_manual(labels = c("T1 mid","T2 mid falling","T3 high rising-falling","T4 low rising-falling",
                                "T5 low rising","T6 low falling"), 
                     values = c("grey", "yellow","green", "red","blue","orange"))+
  theme_bw()+
  #facet_wrap(~Sex)+
  geom_smooth(method="lm", formula = y ~ poly(x, 3), se=FALSE)+
  ylab("f0 (semitones)")+
  xlab("Time (normalized)")

ggsave("T2022_old.png", plot = T2022,
       dpi = 300)



#T3
#Figure 6a, 6b, 7a, 7b
T3_plot2 <- T3 %>% 
  mutate(decade = factor(decade, levels = c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010")))  %>%
  mutate(Sex = factor(Sex, levels = c("F", "M"), 
                      labels = c("Female", "Male")))  %>%
  filter(YOR==1988, Tone=="3")%>%
  ggplot(aes(x=Time_norm, 
             y=Semi_Tone,
             group=interaction(Token, Speaker)))+
  facet_grid(Sex~decade)+
  geom_line()+
  ylab("f0 (semitones)")+
  xlab("Time (normalized)")+
  theme_bw()+ 
  theme(axis.text.x = element_blank(),
        legend.position = "none")+
  ggtitle("a. Tone 3 by decade of birth, 1988")+
  theme(axis.text.x = element_blank(),
        plot.title=element_text(size=10),
        legend.position = "none")

T3_plot2

ggsave("T3_1988a.png", plot=T3_plot2, dpi=300)

#Make other variables factors too
data_all$Age_group  <- as.factor(data_all$Age_group)
data_all$Sex <- as.factor(data_all$Sex)
data_all$Style <- as.factor(data_all$Style)
data_all$Tone <- as.factor(data_all$Tone)
data_all$Initial <- as.factor(data_all$Initial)
data_all$Speaker <- as.factor(data_all$Speaker)

#Make Age and duration numeric
data_all$Education <- as.numeric(data_all$Education)
data_all$T_int <- as.numeric(data_all$T_int)
data_all$Age <- as.numeric(data_all$Age)
data_all$decade <- as.numeric(data_all$decade)
data_all$YOB <- as.numeric(data_all$YOB)
data_all$YOR <- as.numeric(data_all$YOR)


#Incorporate an AR(1) process in the model. 
#"This correction process assumes that the error at time t 
#is the sum of a proportion ρ of the preceding error at time t − 1 
#and Gaussian noise." Chuang et al. 2021.
data_all$AR.start = FALSE 
data_all$AR.start[data_all$Time_norm==0.00000000] = TRUE

#just look at individual tones separately
T3 <- data_all %>% filter(Tone == 3)
T4 <- data_all %>% filter(Tone == 4)
T5 <- data_all %>% filter(Tone == 5)
T45 <- data_all %>% filter(Tone == 5 | Tone == 4)

#Figure 8: style and duration are probably correlated. 
# can we estimate their effects separately?
#compare density plot for the two styles
durT3 <-ggplot(T3, aes(x=T_int, fill=Style)) + 
  geom_density(alpha=.3) + 
  xlab("Duration")+
  scale_fill_manual(name = "Context",
                    labels = c("citation", "phrase-medial"),
                    values = c("blue", "orange"))+
  ggtitle("Tone 3")

durT4 <-ggplot(T4, aes(x=T_int, fill=Style)) + 
  geom_density(alpha=.3) + 
  xlab("Duration")+
  scale_fill_manual(name = "Context",
                    labels = c("citation", "phrase-medial"),
                    values = c("blue", "orange"))+
  ggtitle("Tone 4")

dur34 <- cowplot::plot_grid(durT3 + theme(legend.position = "bottom"), 
                            durT4 + 
                              theme(legend.position = "none" ), 
                            nrow = 1, align = "v")

prow <- plot_grid(
  durT3 + theme(legend.position="none"),
  durT4 + theme(legend.position="none"),
  align = 'vh',
  nrow = 1
)

prow
legend <- get_legend(
  durT3 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

dur34 <- plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
dur34
ggsave("dur34.png", dpi=200)

## GAMM full model includes:
## - Parametric terms for Sex, Style
## - smooth for timepoint by Sex and Style;
## - smooth for decade of birth (cannot use default since only 8 decades) by Sex and Style;
## - smooth for duration (T_int) (use default k) by Sex and Style;
## - tensor production interaction for timepoint and decade of birth by Sex and Style
## - tensor production interaction for timepoint and duration by Sex and Style
## - tensor production interaction for duration and decade of birth by Sex and Style
## - random slopes and intercepts for syllable and Speaker

#bam with AR1 process
T3_dec = bam(Semi_Tone ~ Sex + Style + #YOR + Initial+
                    s(times_c, by=Sex)+  s(times_c, by=Style)+
                    s(decade, k=8, by=Sex) + s(decade, k=8, by=Style) +
                    s(T_int, by=Sex) + s(T_int, by=Style) +
                    ti(times_c, decade, k=c(10, 8), by=Sex) +
                    ti(times_c, decade, k=c(10, 8), by=Style) +
                    ti(times_c, T_int, by=Sex)+
                    ti(times_c, T_int, by=Style)+
                    ti(T_int, decade, k=c(10,8), by=Sex)+
                    ti(T_int, decade, k=c(10,8), by=Style)+
                    s(times_c, Speaker, bs="fs", m=1) +
                    s(times_c, Token, bs ="fs", m=1),
                dat=T3, discrete = TRUE, AR.start=T3$AR.start, rho=0.9, 
             family = "scat")

##check residuals
qq.gam(T3_dec)

##autocorrelations of residuals in the gamm is not correlated after AR1 process
acf(resid_gam(T3_dec))

T45_dec = bam(Semi_Tone ~ Sex + Tone + Style +#YOR + #Initial+
                s(times_c, by=Tone) + s(times_c, by=Sex)+ s(times_c, by=Style)+
                s(decade, k=8, by=Tone) + s(decade, k=8, by=Sex) + s(decade, k=8, by=Style)+
                s(T_int, by=Tone) + s(T_int, by=Sex) +s(T_int, by=Style)+
                ti(times_c, decade, k=c(10,8), by=Tone)+
                ti(times_c, decade, k=c(10,8), by=Sex)+
                ti(times_c, decade, k=c(10,8), by=Style)+
                ti(times_c, T_int, by=Tone)+
                ti(times_c, T_int, by=Sex)+
                ti(times_c, T_int, by=Style)+
                ti(T_int, decade, k=c(10,8), by=Tone)+
                ti(T_int, decade, k=c(10,8), by=Sex)+
                ti(T_int, decade, k=c(10,8), by=Style)+
                s(times_c, Speaker, bs="fs", m=1) +
                s(times_c, Token, bs ="fs", m=1),
              dat=T45, discrete = TRUE, AR.start=T45$AR.start, rho=0.9, 
              family= "scat")

summary(T45_dec, re.test=FALSE)

qq.gam(T45_dec)

##Model comparison
#include Sex? 
T3_noSex <- update(T3_dec, .~. -Sex -s(times_c, by=Sex) 
                   - ti(times_c, decade, k=c(10, 8), by=Sex)
                   -s(decade, k=8, by=Sex)-s(T_int, by=Sex) 
                   -ti(times_c, T_int, by=Sex)
                   -ti(T_int, decade, k=c(10,8), by=Sex) )

compareML(T3_dec, T3_noSex)
#include Sex 

#Style?
T3_noSty <- update(T3_dec, .~. -Style- s(times_c, by=Style) 
                   -s(decade, k=8, by=Style) -s(T_int, by=Style)
                   -ti(times_c, decade, k=c(10,8), by=Style)
                   -ti(times_c, T_int, by=Style)
                   -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T3_dec, T3_noSty)
##with Style has lower AIC, keep it

#Include YOR? 
T3_noYOR <- update(T3_dec, .~. -YOR)

compareML(T3_dec, T3_noYOR)

#T3_noYOR has lower AIC, so can take it out

#Initial?
T3_noIn <- update(T3_noYOR, .~. -Initial)

compareML(T3_noIn, T3_noYOR)

#only small difference, take it out

#duration? 
T3_nodur <- update(T3_dec, .~. -s(T_int, by=Sex) 
                   - s(T_int, by=Style)  
                   -ti(times_c, T_int, by=Sex)
                   -ti(times_c, T_int, by=Style)
                   -ti(T_int, decade, k=c(10,8), by=Sex)
                   -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T3_dec, T3_nodur)

#Decade of birth?
T3_nodec <- update(T3_dec, .~. -s(decade, k=8, by=Sex) 
                   - s(decade, k=8, by=Style) 
                   -ti(times_c, decade, k=c(10, 8), by=Sex)
                   -ti(T_int, decade, k=c(10,8), by=Sex)
                   -ti(times_c, decade, k=c(10,8), by=Style)
                   -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T3_dec, T3_nodec)


#Tone 4: should include Sex or not? 
T45_noSex <- update(T45_dec, .~. -Sex 
                    -s(times_c, by=Sex) 
                    - ti(times_c, decade, k=c(10, 8), by=Sex)
                    -s(decade, k=8, by=Sex)-s(T_int, by=Sex) 
                    -ti(times_c, T_int, by=Sex)
                    -ti(T_int, decade, k=c(10,8), by=Sex))

compareML(T45_dec, T45_noSex)
#include Sex 
#Style?
T45_noSty <- update(T45_dec, .~. -Style- s(times_c, by=Style) 
                    -s(decade, k=8, by=Style) -s(T_int, by=Style)
                    -ti(times_c, decade, k=c(10,8), by=Style)
                    -ti(times_c, T_int, by=Style)
                    -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T45_dec, T45_noSty)

#should include duration?
T45_nodur <- update(T45_dec, .~. -s(T_int, by=Sex)
                    -s(T_int, by=Tone)
                    - s(T_int, by=Style) 
                    -ti(times_c, T_int, by=Tone)
                    -ti(times_c, T_int, by=Sex)
                    -ti(times_c, T_int, by=Style)
                    -ti(T_int, decade, k=c(10,8), by=Tone)
                    -ti(T_int, decade, k=c(10,8), by=Sex)
                    -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T45_dec, T45_nodur)
#Decade of birth?
T45_nodec <- update(T45_dec, .~. -s(decade, k=8, by=Tone)
                    -s(decade, k=8, by=Sex) 
                    - s(decade, k=8, by=Style) 
                    -ti(times_c, decade, k=c(10, 8), by=Sex)
                    - ti(times_c, decade, k=c(10,8), by=Tone)
                    -ti(times_c, decade, k=c(10,8), by=Style)
                    -ti(T_int, decade, k=c(10,8), by=Sex)
                    -ti(T_int, decade, k=c(10,8), by=Tone)
                    -ti(T_int, decade, k=c(10,8), by=Style))

compareML(T45_dec, T45_nodec)

#Figures 9-12: plot the effects of decade, sex, and duration

png('plot1b.png', res = 100, width=480, height=480, pointsize = 16)

plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", Style="citation", decade=1940), 
            rug=F, rm.ranef=T, col="blue", 
            ylim=c(-3,7), ylab="f0 (semitones)", xlab= "time (centered)", main="a. 1940, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", Style="connected", decade=1940), rug=F, 
            rm.ranef=T, col="darkorange", add=T)
text(-0.1, -1, "500 msec, citation", col="blue", cex=1.2)
text(-0.1, -2.5, "200 msec, medial", col="darkorange", cex=1.2)
dev.off()

png('plot2b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=1950), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="b. 1950, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", Style="connected", decade=1950), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, -1, "500 msec", col="blue", cex=1.2)
# text(-0.3, -2.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot2bc.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=1960), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="c. 1960, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", 
                                              Style="connected", decade=1960), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, -1, "500 msec", col="blue", cex=1.2)
# text(-0.3, -2.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot2bcd.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=1970), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="d. 1970, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", 
                                              Style="connected", decade=1970), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, -1, "500 msec", col="blue", cex=1.2)
# text(-0.3, -2.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot3b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=1980), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="e. 1980, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", 
                                              Style="connected", decade=1980), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot3bc.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=1990), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="f. 1990, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", 
                                              Style="connected", decade=1990), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot3ab.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=2000), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="g. 2000, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", Style="connected", decade=2000), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot3abc.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="M", 
                                              Style="citation", decade=2010), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="g. 2000, male") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="M", 
                                              Style="connected", decade=2010), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot4b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", Style="citation", decade=1940),
            rug=F, rm.ranef=T, col="blue", ylim=c(-3,7),
            ylab="f0 (semitones)", xlab= "time (centered)", main="a. 1940, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", Style="connected", decade=1940), rug=F, rm.ranef=T, col="orange", add=T)

text(-0.1, -1, "500 msec, citation", col="blue", cex=1.2)
text(-0.1, -2.5, "200 msec, medial", col="darkorange", cex=1.2)
dev.off()

png('plot5b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", 
                                              Style="citation", decade=1950), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7),
            ylab="f0 (semitones)", xlab= "time (centered)", main="b. 1950, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F",
                                              Style="connected", decade=1950), rug=F, rm.ranef=T, col="orange", add=T)

# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot5ab.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", Style="citation", decade=1960), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7),
            ylab="f0 (semitones)", xlab= "time (centered)", main="c. 1960, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F",Style="connected", decade=1960), rug=F, rm.ranef=T, col="orange", add=T)

# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot6.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F",
                                              Style="citation", decade=1970), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="d. 1970, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", 
                                              Style="connected",decade=1970), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot6b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F",Style="citation", decade=1980), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="e. 1980, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", Style="connected",decade=1980), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot7.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", 
                                              Style="citation", decade=1990), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="f. 1990, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", 
                                              Style="connected", decade=1990), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot7a.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", Style="citation", decade=2000), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="g. 2000, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", Style="connected", decade=2000), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot7b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.5, Sex="F", 
                                              Style="citation", decade=2010), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-2,7), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="h. 2010, female") 
plot_smooth(T3_dec, view="times_c", cond=list(T_int=0.2, Sex="F", 
                                              Style="connected", decade=2010), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 5, "200 msec", col="orange", cex=1.2)
dev.off()


#plot the effects of decade, sex, and duration for T4

png('plot8.png', res = 100, width=480, height=480, pointsize = 16)

plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5,Tone=4, Style="citation", 
                                               Sex="M", decade=1940), 
            rug=F, rm.ranef=T, col="blue", 
            ylim=c(-5,3), ylab="f0 (semitones)", xlab= "time (centered)", main="a. 1940, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2,Tone=4, Style="connected", Sex="M", decade=1940), rug=F, 
            rm.ranef=T, col="darkorange", add=T)
text(0, -3.75, "500 msec, citation", col="blue", cex=1.2)
text(0, -4.75, "200 msec, medial", col="darkorange", cex=1.2)
dev.off()

png('plot9.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, Sex="M", 
                                               Style="citation",decade=1950), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="b. 1950, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="M", decade=1950), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, -1, "500 msec", col="blue", cex=1.2)
# text(-0.3, -2.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot10.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5,Tone=4,  
                                               Style="citation", Sex="M", decade=1960), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="c. 1960, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2,Tone=4,  
                                               Style="connected", Sex="M", decade=1960), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot11.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="M", decade=1970), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="d. 1970, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="M", decade=1970), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot11a.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="M", decade=1980), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="e. 1980, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="M", decade=1980), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot11b.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="M", decade=1990), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="f. 1990, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="M", decade=1990), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot11c.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="M", decade=2000), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), ylab="f0 (semitones)", 
            xlab= "time (centered)", main="g. 2000, male") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="M", decade=2000), 
            rug=F, rm.ranef=T, col="darkorange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 4.5, "200 msec", col="darkorange", cex=1.2)
dev.off()

png('plot12.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1940),
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3),
            ylab="f0 (semitones)", xlab= "time (centered)", main="a. 1940, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2,Tone=4, 
                                               Style="connected", Sex="F", decade=1940), rug=F, rm.ranef=T, col="orange", add=T)
text(0, -3.75, "500 msec, citation", col="blue", cex=1.2)
text(0, -4.75, "200 msec, medial", col="darkorange", cex=1.2)
dev.off()

png('plot12a.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1950),
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3),
            ylab="f0 (semitones)", xlab= "time (centered)", main="b. 1950, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2,Tone=4, 
                                               Style="connected", Sex="F", decade=1950), rug=F, rm.ranef=T, col="orange", add=T)
dev.off()

png('plot13.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1960), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3),
            ylab="f0 (semitones)", xlab= "time (centered)", main="c. 1960, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="F", decade=1960), rug=F, rm.ranef=T, col="orange", add=T)
dev.off()

png('plot13a.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1970), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3),
            ylab="f0 (semitones)", xlab= "time (centered)", main="d. 1970, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="F", decade=1970), rug=F, rm.ranef=T, col="orange", add=T)
dev.off()

png('plot14.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1980), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="e. 1980, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="F", decade=1980), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot14a.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=1990), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="f. 1990, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="F", decade=1990), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.4, 6, "500 msec", col="blue", cex=1.2)
# text(-0.4, 5, "200 msec", col="orange", cex=1.2)
dev.off()

png('plot15.png', res = 100, width=480, height=480, pointsize = 16)
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.5, Tone=4, 
                                               Style="citation", Sex="F", decade=2000), 
            rug=F, rm.ranef=T, col="blue", ylim=c(-5,3), 
            ylab="f0 (semitones)", xlab= "time (centered)", main="g. 2000, female") 
plot_smooth(T45_dec, view="times_c", cond=list(T_int=0.2, Tone=4, 
                                               Style="connected", Sex="F", decade=2000), rug=F, rm.ranef=T, col="orange", add=T)
# text(-0.3, 6, "500 msec", col="blue", cex=1.2)
# text(-0.3, 5, "200 msec", col="orange", cex=1.2)
dev.off()
