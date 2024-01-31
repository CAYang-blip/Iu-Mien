#------------- TONE DATA PROCESSING PROGRAM -----------------

# Tone Processing -- Takes you from the output file of the Praat F0 
# measurement script to a data set to be used in the Rbrul
# linear mixed effects modeling.  The authors of this program are
# X and Y, based on Stanford (2008, 2016).  
# Program written May - August, 2020.

# Stay safe and healthy.

#  The following packages need to be installed. Open the relevant libraries:

library(tidyverse)
library(stringr)
library(rstatix)

#  
# change working directory to local location of folder on your machine.
#  the working directory is assumed to contain the praat data.
#  Here we assume it is in a csv file.

data_cit <- read_csv("praat_data.csv")

data_con <-read_csv("praat_data_con.csv")

data <- bind_rows(data_cit,data_con)

data$Pitch <- as.numeric(data$Pitch)

# also get rid of random undefineds 

data <- data %>%
  filter( (Pitch != "undefined") )

#  add a tones column by processing the tokens

data <- data %>% mutate(Tone = str_sub(Token,-1))


#filter out unwanted tokens
data <- data %>% filter(Token != "bia2", Token != "bia2?", Token != "4", 
                        Token != "v", Token != "he2?", 
                        Token != "me7?", Token != "bu8",
                        Token != "bu8?", Token !="tui2", 
                        Token !="te7", Token !="xɔi1", 
                        Token != "te2?", Token != "bum4?", 
                        Token != "jao6?", Token != "put8?", Token !="put8",
                        Token != "jaao6", Token != "biən2", Token != "koŋ1")

# flatten out the data for further processing.  That is, take out
# the time and pitch info temporarily while we add in Preceding and Following tones, 
# and initial consonants.

data_f <- data %>% 
  distinct(Token_Number,Speaker,Style,.keep_all = T) %>%
  select(-Time,-Pitch)

#  We now begin to process the tokens locating and typing
#  initial consonants

tokens <- unique(data_f$Token)

tokens

# Locate the vowels in each token

vowels <- "a|e|u|o|i|A|E|I|O|U|ə|ɛ|ɔ"

v_loc <- regexpr(vowels,tokens)

# Use a substring to get onsets

ons <- substr(tokens,1,v_loc-1)
ons

# Initial consonant type information

vu <- "p|t|k|q|ts|tS|tɕ|s|x|c"
va <- "ph|th|kh|qh|tsh|tSh|tch"
vb <- "b|d|g|dz|v|z|Z|G"
vs <- "j|m|n|l|r|ɲ|ŋ|y|w"

# token information gathered

token_info <- tibble( Token = tokens, `Initial Consonant` = ons )

# the onset type info is gathered

token_info <- token_info %>%
              mutate(Initial = case_when(
                     `Initial Consonant` == ""     ~ "vowel",
                     grepl(va,`Initial Consonant`) ~ "aspirated",
                     grepl(vu,`Initial Consonant`) ~ "voiceless",
                     grepl(vb,`Initial Consonant`) ~ "voiced",
                     grepl(vs,`Initial Consonant`) ~ "sonorant"))

#  This information is now re-integrated with data_f by a left join

data_f <- left_join(data_f, token_info, by = "Token")

#  We are now ready to re-join this data set to the main one

data <- data %>% left_join( data_f,
           by = c("Token_Number","Speaker","Token","Tone", "Style"))

#  We need to filter out tokens whose n-count is less than 4

data <- data %>%
        group_by(Speaker,Token_Number, Style) %>%
        mutate(Toks = n()) %>%
        filter(Toks > 3) %>%
        select(-Toks)


#count tokens by tone per speaker per tone
count_tone <- data_f %>%
  distinct() %>%
  group_by(Speaker,Tone) %>%
  summarise(number_of_occurences = n())

#count tokens by tone 
count_token <- data_f %>%
  distinct() %>%
  group_by(Tone) %>%
  summarise(number_of_occurences = n())

#count tokens by Speaker
count_Speaker <- data_f %>%
  distinct() %>%
  group_by(Speaker) %>%
  summarise(number_of_occurences = n())

#  Normalization of the times -- using only times from praat

data <- data %>% 
        group_by(Speaker,Token_Number,Style) %>%
        mutate(T_int = max(Time)-min(Time)) %>%
        mutate(Time_norm = (Time - min(Time))/T_int)

#  Obtaining the semi-tones P <- 12*(log(P/ref_mean)) / log(2) 
#  Without the gross outliers we now can get the speaker's pitch
#  reference mean for the reference tone (Tone 1, [33]) or 
#  the overall mean F0

#  get the reference mean per speaker

#ref_tone <- "1"
ref_mean <- data %>%
  group_by(Speaker) %>%
  #filter(Tone == ref_tone ) %>% comment this out for overall mean
  summarise(r_mean = mean(Pitch))


data <- data %>% 
        left_join( ref_mean, by = "Speaker") %>%
        group_by(Speaker,Token_Number, Style) %>%
        mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
        select(-r_mean)


## ham-fisted outlier ID first to visually check
T_plot <- data %>% 
  mutate(Tone=as_factor(Tone)) %>%
  filter(Tone==3, Speaker==20|Speaker==23|Speaker==24|Speaker==25|Speaker==26)%>%
  ggplot(aes(x=Time_norm, 
             y=Semi_Tone, 
             group=Token_Number, 
             color=Tone, 
             linetype=Style)) +
  facet_grid(Style~Code, scales="free_y") + 
  geom_line()

T_plot

#  We now filter out all tones with semitone values that are outside
# -8 to +10 semitone range

data <- data %>%
      filter(Semi_Tone >= -8 & Semi_Tone <= 10)

##  Now filter out tokens that show pitch halving or doubling
# First identify where two consecutive rows show a jump in T1 of more than 60 Hz

td_pd <- data %>%
  group_by(Speaker, Token_Number, Style) %>% 
  summarise( token_rows = n(),
             T1_dif = Pitch %>% na.omit() %>% 
               diff() %>% abs() %>% max(),
             PD = if_else( T1_dif > 60,
                           "YES", "NO"))

sum(td_pd$PD == "YES")

data <- data %>% 
  left_join(td_pd)
data <- data %>% select(-T1_dif)

data <- data %>% filter(PD == "NO")
data <- data %>% select(-PD)
data <- data %>% select(-token_rows)

#filter out Speaker 7 because she is from Lampang, a different dialect
data <- data %>% filter(Speaker != 7)

#count tokens by tone
count_tone <- data_f %>%
  distinct() %>%
  group_by(Tone) %>%
  summarise(number_of_occurences = n())

#  We will also add demographic information on the speaker.  A file with
#  this information is also found in the working directory

speaker_info <- read_csv("Speaker_info.csv")

#  attach the speaker info

data_f <- data_f %>% 
              left_join( speaker_info, by = "Speaker")

#  We also connect this data to the main data file

data <- data %>%
  left_join( speaker_info, by = "Speaker")

# get the orthogonal polynomial coefficients
#  We need to filter out tokens whose n-count is less than 4

data <- data %>%
  group_by(Speaker,Token_Number, Style) %>%
  mutate(Toks = n()) %>%
  filter(Toks > 3) %>%
  select(-Toks)

#filter out incorrectly annotated tokens (interval over whole sentence annotated)
data <- data %>% filter(T_int < 1)

# get the orthogonal polynomial coefficients
data_ex <- data %>%
  select(Speaker, Token_Number,Token,Tone,`Initial Consonant`, Age, Sex, T_int,
         Time_norm, Semi_Tone) %>%
  filter( !is.na(Semi_Tone)) %>%
  group_by(Speaker, Token_Number, Token, T_int, Tone, `Initial Consonant`, Age) %>%
  summarise(Coefs = lm( Semi_Tone ~ poly(Time_norm,3,raw = FALSE)) %>%
              coef() %>% as.vector(),
            Start_Time = min(Time_norm),
            End_Time = max(Time_norm)) %>%
  mutate(Cnames = c('a','b','c','d')) %>%
  pivot_wider(names_from = Cnames, values_from =Coefs)  %>%
  ungroup

#
#  Join to the data file

data_f <- data_f %>% left_join(data_ex)

data_f <- data_f %>%
  select(-token_rows, -Time_norm, -Start_Time, -End_Time)

#locate the temporal location of the peak (maximum f0 value)

data_maxvals <- data %>%
  filter(Tone =="3"|Tone=="4")%>%
  filter(Time_norm>0.1)%>%
  group_by(Speaker, Token_Number, Style) %>%
  filter(Semi_Tone == max(Semi_Tone, na.rm = TRUE))

#save the tibble as csv to use in Rbrul
write_excel_csv(data_maxvals,path = "C:/Users/cathr/OneDrive/Documents/peaks2022.csv", col_names = TRUE)

#peak timing plot
loc_maxT <- data_maxvals %>%
  filter(Tone ==3, Location == "Huai Mae Sai"|Location=="Huai Maesaai")%>%
  # mutate(Generation = case_when (Age <=30 ~"3 Young", 
  #                                Age <= 50 ~ "2 Middle", 
  #                                Age <= 84 ~ "1 Old"))
  mutate(decade = case_when( Age <= 25 ~ "12-25 yrs",
                                     Age <= 35 ~ "26-35 yrs",
                                     Age <= 45 ~ "36-45 yrs",
                                     Age <= 55 ~ "46-55 yrs",
                                     Age <= 65 ~ "56-65 yrs",
                                     Age <= 75 ~ "66-75 yrs",
                                     Age <= 85 ~ "76-85 yrs"))
 # # mutate(`Age Group` = case_when( Age <= 25 ~ "12-25 yrs",
 #                                   Age <= 30 ~ "26-30 yrs",
 #                                   Age <= 35 ~ "31-35 yrs",
 #                                   Age <= 40 ~ "36-40 yrs",
 #                                   Age <= 45 ~ "41-45 yrs",
 #                                   Age <= 50 ~ "46-50 yrs",
 #                                   Age <= 55 ~ "51-55 yrs",
 #                                   Age <= 60 ~ "56-60 yrs",
 #                                   Age <= 65 ~ "61-65 yrs",
 #                                   Age <= 75 ~ "66-75 yrs", 
 #                                  Age <= 84 ~ "76-84 yrs"))
#violin plot
T4_peak <- ggplot(data = loc_maxT, aes(group = decade, x=decade, y=Time_norm))+
  geom_violin(draw_quantiles = c(.25, .50, .75))+
  facet_wrap(~Style)

T4_peak

ggsave("T4_peak.png", dpi = 300)

#peak timing plot WITH the 1988 data added to "peaks.csv"
peaks <-read.csv("peaks8822.csv")
T3peak <- peaks %>%
  filter(Tone ==3, Recording_year ==2022)%>%
  mutate(`Decade of birth` = case_when( YOB <= 1939 ~ "1932-1939",
                                  YOB <= 1949 ~ "1940-1949",
                                  YOB <= 1959 ~ "1950-1959",
                                  YOB <= 1969 ~ "1960-1969",
                                  YOB <= 1979 ~ "1970-1979",
                                  YOB <= 1989 ~ "1980-1989",
                                  YOB <= 1999 ~ "1990-1999",
                                  YOB <= 2010 ~ "2000-2010"))
#violin plot
T3_peak <- ggplot(data = filter(T3peak, YOB<=1999), aes(group = `Decade of birth`, 
                                     x=`Decade of birth`, y=Time_norm, 
                                     fill = `Decade of birth`, trim=TRUE))+
  facet_wrap(~Style)+
  geom_violin(draw_quantiles = c(.50))+
  theme(legend.position = "none")+
  ylab("f0 peak location")+
  ggtitle("Tone 3 f0 peak location by decade of birth")

T3_peak

ggsave("T3_peak_2.png", dpi = 300)
#Tone 4 peak location
#peak timing plot WITH the 1988 data added to "peaks.csv"

T4peak <- peaks %>%
  filter(Tone ==4, Recording_year ==2022)%>%
  mutate(`Decade of birth` = case_when( YOB <= 1939 ~ "1932-1939",
                                        YOB <= 1949 ~ "1940-1949",
                                        YOB <= 1959 ~ "1950-1959",
                                        YOB <= 1969 ~ "1960-1969",
                                        YOB <= 1979 ~ "1970-1979",
                                        YOB <= 1989 ~ "1980-1989",
                                        YOB <= 1999 ~ "1990-1999",
                                        YOB <= 2010 ~ "2000-2010"))
#violin plot
T4_peak <- ggplot(data = T4peak, aes(group = `Decade of birth`, 
                                     x=`Decade of birth`, y=Time_norm, 
                                     fill = `Decade of birth`, trim=TRUE))+
  geom_violin(draw_quantiles = c(.50))+
  facet_wrap(~Style, nrow=1)+
  theme(legend.position = "none")+
  ylab("Peak location")+
  ggtitle("Tone 4 peak location by decade of birth")

T4_peak

ggsave("T4_peak.png", dpi = 300)

# Modeling data written to csv file which is used in Rbrul. This
# output section can be edited to work with your file structure. At this point
# the output files appear in the working directory.

file_path <- getwd()
file_name_for_analyzed <- "/data_analysis.csv"
file_name_for_praat_update <- "/data_plotting.csv"
p1 <- paste(file_path,file_name_for_analyzed,sep="")
p2 <- paste(file_path,file_name_for_praat_update,sep="") 

#  We also write out the main data file which is used for plotting

write_excel_csv(data_f,path = p1, col_names = TRUE)
write_excel_csv(data, path = p2, col_names = TRUE  )

# End of Program

#Run Rbrul to do the lme

source("http://www.danielezrajohnson.com/Rbrul.R")

rbrul()

