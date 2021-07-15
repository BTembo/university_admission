
## -- Load the required libraries -- ##

library(ggplot2)
library(readxl)
library(stringr)
library(digest)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## -- General comments -- ##

# I deliberate use the full call for function, which includes 'package' name followed by '::' then the 'function'
# for example, dplyr::filter to mean filter is from the dplyr package.
# such a practise can be extremely helpful for new R users.

## -- Read in the data -- ##

# Admission data is based on this announcement: https://www.facebook.com/UNZAOfficial/posts/4120685381313333

df_advertised <- readxl::read_xlsx("unza_programmes.xlsx",
                                   sheet = "Processed_Combined")

df_advertised


df_admitted <- read.csv('processed_admitted_candidates_2022_intake.csv',
                        stringsAsFactors = F) %>% 
  dplyr::as_tibble()

df_admitted


## How many students were admitted to UNZA for the 2022 intake?
df_admitted_students <- df_admitted %>%
  dplyr::select(crypto_fullname, crypto_nrc, gender, programme_as_admitted) %>% 
  # get unique data.frame
  dplyr::distinct()
                
df_admitted_students

length(df_admitted$crypto_fullname)
# Based on the published list by UNZA, 5,063 students were admitted for the 2022 intake.

length(df_admitted_students$crypto_fullname)
# However, after cleaning the data only 4,737 students were admitted for the 2022 intake.
# This represents a difference of 326 admissions


## What is the gender split of the 2022 intake?
table(df_admitted_students$gender)

#This represents 43.51% (2061/4737) and 56.49% (2676/4737)


## How many Bachelors programmes per school?
df_prog_sch <- df_advertised %>% 
  dplyr::select(school, school_abbr,
                programme_as_advertised) %>% 
  dplyr::mutate(test_bachelors = stringr::str_detect(tolower(programme_as_advertised), 'bachelor')) %>% 
  dplyr::filter(!is.na(programme_as_advertised),
                test_bachelors == T) %>% 
  dplyr::distinct()
  
df_prog_sch  

table(df_prog_sch$school_abbr)

# same information but in dataframe format
df_num_prog_sch <- as.data.frame(table(df_prog_sch$school_abbr)) %>% 
  dplyr::rename(school_abbr = Var1,
                num_prog = Freq)

df_num_prog_sch


## How is the admission by school and gender?
df_sch_adm_combined <- df_advertised %>%
  dplyr::select(-series, -allocation_certainty, -programme_as_advertised) %>%
  dplyr::full_join(df_admitted_students, by = c('programme_as_admitted')) %>% 
  dplyr::mutate(test_bachelors = stringr::str_detect(tolower(programme_as_admitted), 'bachelor')) %>% 
  dplyr::filter(!is.na(crypto_fullname),
                test_bachelors == T) %>%
  dplyr::distinct()

df_sch_adm_combined
# A total of 4,670 admissions were at Bachelor's level

table(df_sch_adm_combined$school_abbr)

# same information but in dataframe format
df_num_sch_admission <- as.data.frame(table(df_sch_adm_combined$school_abbr)) %>% 
  dplyr::rename(school_abbr = Var1,
                num_admitted = Freq) %>% 
  dplyr::full_join(df_num_prog_sch, by = c('school_abbr')) %>% 
  dplyr::mutate(adm_per_prog = round(num_admitted/num_prog, 2))


df_num_sch_admission





plt1 <- df_num_sch_admission %>% 
  ggplot2::ggplot() +
  ggplot2::geom_col(aes(reorder(school_abbr, adm_per_prog), adm_per_prog)) +
  labs(x = "School", 
       y = "Admissions per Bachelor's Prog. on offer",
       title = "UNZA's 2022 Intake (as of 12th July 2021)",
       caption = "\u00A9Bernard Tembo (2021)" ) +
  coord_flip() +
  # theme(plot.caption = element_text(color = "navyblue", face = "italic")) +
  theme_classic()

plt1
