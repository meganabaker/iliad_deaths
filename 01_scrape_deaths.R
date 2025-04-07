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
iliad_deaths_d1 <- text_of_interest[str_detect(text_of_interest, "[:alpha:] ([:upper:])")] %>%
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
table(iliad_deaths_d1$actor_side, useNA = "always")

#confirm each record has an injury/death type indicated
subset(iliad_deaths_d1, wounded == F & hit == F & killed == F)

#check victim
subset(iliad_deaths_d1, is.na(victim))
#all non-standard responses, will revisit

#check victim_side
table(iliad_deaths_d1$victim_side, useNA = "always")
subset(iliad_deaths_d1, is.na(victim_side))
#also mostly missing or non-standard, will revisit 

#check page reference
subset(iliad_deaths_d1, is.na(page_reference))
#missing from source

#okay! this is looking pretty good- 
#going to add in some manual fixes
#prior to saving our analysis file
iliad_deaths_d2 <- iliad_deaths_d1 %>%
  #work first on completing victim field
  mutate(victim = case_when(str_detect(action, "twelve sleeping Thracian soldiers") == 1 ~ "Twelve sleeping Thracian soldiers, includes Rhesus",
                            str_detect(action, "two sons of Merops") == 1 ~ "Two sons of Merops (Adrestus and Amphius)",
                            str_detect(action, "twenty-seven anonymous Trojans") == 1 ~ "Twenty-seven anonymous Trojans",
                            victim == "hits Hector" ~ "Hector", 
                            .default = victim)) %>%
  #adding in victim side where it is known
  #otherwise, assuming opposite side of actor
  mutate(victim_side = case_when(victim == "Twelve sleeping Thracian soldiers, includes Rhesus" ~ "T", 
                                 victim == "Two sons of Merops (Adrestus and Amphius)" ~ "T",
                                 victim == "Twenty-seven anonymous Trojans" ~ "T",
                                 is.na(victim_side) & actor_side == "A" ~ "T",
                                 is.na(victim_side) & actor_side == "T" ~ "A",
                                 .default = victim_side)) %>%
  #repairing action fields that won't parse later
  mutate(action = gsub("Iphidamas T)", "Iphidamas (T)", action)) %>%
  mutate(action = gsub("spear in the back)", "spear in the back", action))

#recheck victim_side
table(iliad_deaths_d2$victim_side, useNA = "always")

#great! so now all records have an attack type, 
#actor_side, victim, victim_side, and a page_reference 
#if it was included on the source! 
#now we just have to go back and identify the attackers
#we should just be able to go back to the orig vector,
#find the string, and return one prior index when possible
action_vec <- iliad_deaths_d2$action
actor_match <- c()
for (i in 1:length(action_vec)){
  index <- str_which(human_readable, coll(action_vec[i]))
  if (length(index) != 0){
    actor_match[i] <- human_readable[(index - 1)]
  }
  else {
    actor_match[i] <- NA
  }
}

#Because the data was messy, and we know that
#the existing actor field is largely right save 
#for the excessive missingness, I am going to 
#manually review the field
iliad_deaths_d3 <- as.data.frame(cbind(iliad_deaths_d2, actor_match)) %>%
  mutate(col_actor = coalesce(actor, actor_match))

print_for_review <- iliad_deaths_d3 %>%
  select(col_actor, action)

#okay, one thing we expected was that two-word
#names would have to be updated
#(Telamonian Ajax, Oïlean Ajax)
#what I didn't expect to find was that
#we have a bunch of missing records:

#Telamonian Ajax kills Amphius (T), spear in the gut (5.717)
#Tlepolemus (A) wounds Sarpedon (T) spear in the thigh (5.764)
#Diomedes wounds Ares in the gut (5.980)
#Coön (T) wounds Agamemnon (A), spear in the arm (11.288)
#Socus (T) wounds Odysseus (A), spear in the ribs (11.493)
#Polypoetes (A) kills Pylon (T) (12.194)
#Leonteus (A) kills Orestes (T) (12.201)
#Meriones (A) wounds Deïphobus (T) spear in the arm (13.634)
#Polites (T) kills Echius (A) (15.400)
#Thrasymedes (A) kills Maris (T), spear in the shoulder (16.377)

#in good news, looks like by and large the mapping
#we did above worked! just have to make the updates
#mentioned above and add back the missing records: