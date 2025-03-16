library(tidyverse)
library(rvest)
library(xml2)

##########################################
# Thank you to Ian Johnston, 
# Malaspina University-College, Nanaimo, BC 
# (now Vancouver Island University)
# for making this web page available
##########################################

#Scrape data from website
raw_scrape <- read_html("https://johnstoniatexts.x10host.com/homer/iliaddeaths.htm")

#keeping only the human-readable text from the html code
human_readable <- raw_scrape %>% 
  html_elements("span") %>%
  html_text()

#seems to follow following column format:
# 1. Killer
# 2. Killer (again)
# 3. who they killed, how, and when in the book it happened
# 4. Who they killed (again)
# that is, the scraped data has ~4 list items per line of text on the website
# however, that is not always true- see this test frame for reference: 
test_frame <- as.data.frame(matrix(human_readable[16:787],ncol =4,byrow = T))

#we also know that many characters killed multiple others, 
#so don't want to go too crazy with deduping
#I think our best bet is using regex to keep the string
#that contains the "wounds/kills [name] ()"
#and work from there

#selecting only the text that we want
#(trims off top paragraphs and bottom links)
text_of_interest <- human_readable[16:785] 

#keeping obs we want, making into a data frame, and 
#creating some columns using some best-guesses
iliad_deaths <- text_of_interest[str_detect(text_of_interest, "[:alpha:] ([:upper:])")] %>%
  as.data.frame() %>%
  rename(action = 1) %>%
  mutate(actor = str_extract(action, "^[:alpha:]+"), 
         actor_side = gsub("\\)", "", gsub("\\(","", str_extract(action, "\\([:upper:]\\)"))),
         wounded = str_detect(action, "wounds"), 
         hit = str_detect(action, "hits"), 
         killed = str_detect(action, "kills"), 
         victim = gsub("(kills|wounds) ", "", 
                       str_extract(action, "(kills|wounds|hits) [:alpha:]+")),
         victim_side = gsub("\\)", "", gsub("\\(", "", str_extract(
                       str_extract(action, "(kills|wounds|hits) [:alpha:]+ \\([:upper:]\\)"), 
                       "\\([:upper:]\\)"))),
         injury = str_squish(gsub(" \\(", "", gsub("\\),+", "", gsub("\\.\\)","", gsub("[0-9]+", "", gsub("\\) \\(+", "", 
                          str_extract(str_extract(gsub("\n"," ", action), 
                              "(kills|wounds|hits) [:alpha:]+ \\([:upper:]\\)[:punct:]* \\(*[:print:]+\\)*"),
                              "\\)[:punct:]* \\(*[:print:]+\\)*"))))))),
         page_reference = as.numeric(str_extract(action, "[:digit:]+\\.[:digit:]+"))) %>%
  mutate(injury = if_else(injury == " "| injury == "" | injury == ";", NA, injury))

#check created vars
#we know actor has a ton of missingness, skipping for now

#check actor_side
table(iliad_deaths$actor_side, useNA = "always")

#confirm each record has an injury/death type indicated
subset(iliad_deaths, wounded == F & hit == F & killed == F)

#check victim
subset(iliad_deaths, is.na(victim))
#all non-standard responses, will revisit

#check victim_side
table(iliad_deaths$victim_side, useNA = "always")
subset(iliad_deaths, is.na(victim_side))
#also mostly missing or non-standard, will revisit

#check page reference
subset(iliad_deaths, is.na(page_reference))
#missing from source

#okay! this is looking pretty good- 
#going to add in some manual fixes
#prior to saving our analysis file