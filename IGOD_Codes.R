### Indian Insurgent Group Origins Data ###
############## Ilayda Onder ###############
################ 7/10/2020 ################

# Required packages (install if necessary)
# install.packages("readxl") 
# install.packages("dplyr)
# install.packages("writexl)
library(readxl) # To read excel files
library(dplyr) # To manipulate data frames
library(writexl) # To write excel files

# The original data sources used in constructing the analysis dataset are the following:
### 1. Global Terrorism Database (GTD) can be downloaded via: https://www.start.umd.edu/gtd/
##### Please note that GTD website allows you to download two data files: (1) GTD 1970-2018 and (2) GTD 1993. 
##### This dataset focuses on the time period from 2004 through 2016. So, downloading GTD 1971-2019 will be enough. 

# 1. Extracting variables and observations from Global Terrorism Database (GTD)
gtd1 <- read_excel("globalterrorismdb_0919dist.xlsx") # read GTD excel file
india <- subset(gtd1, country_txt == "India") # subset to India
india <- subset(india, iyear > 2003) # subset to post 2003
india <- subset(india, iyear < 2017) # drop post-2016 attacks
india <- subset(india, INT_ANY != 1) # drop international attacks
india <- subset(india, provstate != "Unknown") # drop the attacks if the target state is unknown
india <- subset(india, targtype1 != 20) # drop the attacks if the target type is unknown
india <- subset(india, targtype1 != 17) # drop the attacks if the target type is terrorists
india <- subset(india, targtype1 != 22) # drop the attacks if the target type is violent political parties
# fix variable names
names(india)[2] <- "year"
names(india)[3] <- "month"
names(india)[4] <- "day"
names(india)[12] <- "state_txt"
names(india)[59] <- "group_txt"
# fix state names
india$provstate[india$provstate == "Jammu and Kashmir"] <- "Jammu & Kashmir"
india$provstate[india$provstate == "Uttaranchal"] <- "Uttarakhand"
india$provstate[india$provstate == "Andhra pradesh"] <- "Andhra Pradesh"
india$provstate[india$provstate == "Delhi"] <- "Nct Of Delhi"
india$provstate[india$provstate == "Orissa"] <- "Odisha"
# assign state codes
india$state[india$state_txt == "Jammu & Kashmir"] <- 1
india$state[india$state_txt == "Himachal Pradesh"] <- 2
india$state[india$state_txt == "Punjab"] <- 3
india$state[india$state_txt == "Chandigarh"] <- 4
india$state[india$state_txt == "Uttarakhand"] <- 5
india$state[india$state_txt == "Haryana"] <- 6
india$state[india$state_txt == "Nct Of Delhi"] <- 7
india$state[india$state_txt == "Rajasthan"] <- 8
india$state[india$state_txt == "Uttar Pradesh"] <- 9
india$state[india$state_txt == "Bihar"] <- 10
india$state[india$state_txt == "Sikkim"] <- 11
india$state[india$state_txt == "Arunachal Pradesh"] <- 12
india$state[india$state_txt == "Nagaland"] <- 13
india$state[india$state_txt == "Manipur"] <- 14
india$state[india$state_txt == "Mizoram"] <- 15
india$state[india$state_txt == "Tripura"] <- 16
india$state[india$state_txt == "Meghalaya"] <- 17
india$state[india$state_txt == "Assam"] <- 18
india$state[india$state_txt == "West Bengal"] <- 19
india$state[india$state_txt == "Jharkhand"] <- 20
india$state[india$state_txt == "Odisha"] <- 21
india$state[india$state_txt == "Chhattisgarh"] <- 22
india$state[india$state_txt == "Madhya Pradesh"] <- 23
india$state[india$state_txt == "Gujarat"] <- 24
india$state[india$state_txt == "Daman & Diu"] <- 25
india$state[india$state_txt == "Dadra & Nagar Haveli"] <- 26
india$state[india$state_txt == "Maharashtra"] <- 27
india$state[india$state_txt == "Andhra Pradesh"] <- 28
india$state[india$state_txt == "Karnataka"] <- 29
india$state[india$state_txt == "Goa"] <- 30
india$state[india$state_txt == "Lakshadweep"] <- 31
india$state[india$state_txt == "Kerala"] <- 32
india$state[india$state_txt == "Tamil Nadu"] <- 33
india$state[india$state_txt == "Puducherry"] <- 34
india$state[india$state_txt == "Andaman & Nicobar Island"] <- 35
india$state[india$state_txt == "Telangana"] <- 36

# CREATE ORIGINS DATASET
origins <- as.data.frame(table(india$group_txt, india$state_txt)) # create frequency table of Indian states and insurgent organizations
names(origins)[1] <- "group_txt" # redefined column names
names(origins)[2] <- "state_txt" # redefined column names
names(origins)[3] <- "attacks" # redefined column names
origins <- subset(origins, attacks > 0) # subset to dyads that experienced at least one attack
origins <- origins[order(origins$group_txt),] # order the data frame according to insurgent group
origins <- subset(origins, group_txt != "Unknown") # drop the insurgent groups if their names are "unknown". GTD does not identify the perpetrator group name associated with each attack
total_attacks <- aggregate(attacks ~ group_txt, data = origins, sum) # create total attacks variable that shows the total number of attacks an insurgent group committed in an Indian state
names(total_attacks)[2] <- "total" # redefined column names
origins <- merge(origins, total_attacks, by = "group_txt", all.x = TRUE) # merge the total_attacks variable with the origins dataset
origins$percent <- origins$attacks / origins$total # create percentage variable that shows the ratio of attacks an insurgent organization committed in an Indian state to the total number of attacks committed by that organization
origins <- origins[order(origins$percent, decreasing = T),] # order the dataset according to percentage variable
# assign insurgent group codes
origins$group <- NA
origins$group[origins$group_txt == "A'chik Matgrik Elite Force (AMEF)"] <- 114
origins$group[origins$group_txt == "Achik Matgrik Army (AMA)"] <- 112
origins$group[origins$group_txt == "Achik National Cooperative Army (ANCA)"] <- 120
origins$group[origins$group_txt == "Achik National Liberation Army (ANLA)"] <- 118
origins$group[origins$group_txt == "Achik National Volunteer Council-B (ANVC-B)"] <- 119
origins$group[origins$group_txt == "Achik Songna An'pachakgipa Kotok (ASAK)"] <- 115
origins$group[origins$group_txt == "Achik Tiger Force"] <- 122
origins$group[origins$group_txt == "Adivasi Cobra Militants of Assam (ACMA)"] <- 41
origins$group[origins$group_txt == "Adivasi National Liberation Army (ANLA)"] <- 18
origins$group[origins$group_txt == "Adivasi People's Army (APA)"] <- 16
origins$group[origins$group_txt == "Al-Arifeen"] <- 57
origins$group[origins$group_txt == "Al-Fajr"] <- 68
origins$group[origins$group_txt == "Al-Nasireen Group"] <- 59
origins$group[origins$group_txt == "Al-Nasirin (India)"] <- 66
origins$group[origins$group_txt == "Al-Shuda Brigade"] <- 69
origins$group[origins$group_txt == "Al-Ummah"] <- 131
origins$group[origins$group_txt == "All Assam Revolutionary Army (AARA)"] <- 23
origins$group[origins$group_txt == "All Kamatapur Liberation Force"] <- 17
origins$group[origins$group_txt == "Babbar Khalsa International (BKI)"] <- 127
origins$group[origins$group_txt == "Base Movement"] <- 4
origins$group[origins$group_txt == "Bharatiya Janata Party"] <- 84
origins$group[origins$group_txt == "Bodo People's Front (BPF)"] <- 28
origins$group[origins$group_txt == "Bru National Liberation Front (BNLF)"] <- 25
origins$group[origins$group_txt == "Citizen's Rights Protection Volunteers"] <- 34
origins$group[origins$group_txt == "Communist Party of India - Maoist (CPI-Maoist)"] <- 1
origins$group[origins$group_txt == "Communist Party of India- Marxist"] <- 80
origins$group[origins$group_txt == "Communist Party of India- Marxist-Leninist"] <- 7
origins$group[origins$group_txt == "Coordination Committee (CORCOM)"] <- 87
origins$group[origins$group_txt == "Democratic Youth Federation of India (DYFI)"] <- 82
origins$group[origins$group_txt == "Dima Halao Daoga (DHD)"] <- 11
origins$group[origins$group_txt == "Dima Hasao National Army"] <- 29
origins$group[origins$group_txt == "Dravidar Viduthalai Kazhagam (DVK)"] <- 132
origins$group[origins$group_txt == "Garo National Liberation Army"] <- 24
origins$group[origins$group_txt == "Gunmen"] <- 30
origins$group[origins$group_txt == "Hill Tiger Force (HTF)"] <- 35
origins$group[origins$group_txt == "Hindu extremists"] <- 130
origins$group[origins$group_txt == "Hindu Illaignar Sena"] <- 137
origins$group[origins$group_txt == "Hmar People's Convention-Democracy (HPC-D)"] <- 106
origins$group[origins$group_txt == "Indian Mujahideen"] <- 52
origins$group[origins$group_txt == "Islamic Front"] <- 62
origins$group[origins$group_txt == "Islamic Movement of Kashmir"] <- 60
origins$group[origins$group_txt == "Jai Shri Ram Hindu Bhai Group"] <- 138
origins$group[origins$group_txt == "Jammu and Kashmir Islamic Front"] <- 65
origins$group[origins$group_txt == "Jharkhand Bachao Aandolan"] <- 72
origins$group[origins$group_txt == "Jharkhand Janmukti Parishad (JJP)"] <- 53
origins$group[origins$group_txt == "Jharkhand Kranti Raksha Dal (Utari Chotanagpur)"] <- 75
origins$group[origins$group_txt == "Jharkhand Liberation Tigers (JLT)"] <- 73
origins$group[origins$group_txt == "Jharkhand Prastuti Committee (JPC)"] <- 74
origins$group[origins$group_txt == "Jharkhand Sangharsh Jan Mukti Morcha"] <- 78
origins$group[origins$group_txt == "Kamtapur Liberation Organization (KLO)"] <- 13
origins$group[origins$group_txt == "Kanglei Yawol Kanna Lup (KYKL)"] <- 95
origins$group[origins$group_txt == "Kangleipak Communist Party (KCP)"] <- 88
origins$group[origins$group_txt == "Karbi Longri National Liberation Front (KLNLF)"] <- 27
origins$group[origins$group_txt == "Karbi Longri North Cachar Liberation Front (KLNLF)"] <- 10
origins$group[origins$group_txt == "Karbi People's Liberation Tigers (KPLT)"] <- 14
origins$group[origins$group_txt == "Karbi Tribe"] <- 22
origins$group[origins$group_txt == "Khalistan Liberation Force"] <- 128
origins$group[origins$group_txt == "Khasi Students Union"] <- 113
origins$group[origins$group_txt == "Kuki Independent Organization (KIO/KIA)"] <- 111
origins$group[origins$group_txt == "Kuki Liberation Army (KLA)"] <- 104
origins$group[origins$group_txt == "Kuki National Front (KNF)"] <- 89
origins$group[origins$group_txt == "Kuki National Liberation Front (KNLF)"] <- 93
origins$group[origins$group_txt == "Kuki Revolutionary Army (KRA)"] <- 90
origins$group[origins$group_txt == "Kuki Tribal Militants"] <- 94
origins$group[origins$group_txt == "Kuki Unification Frontal Organization (KUFO)"] <- 99
origins$group[origins$group_txt == "Lama Group"] <- 116
origins$group[origins$group_txt == "Lashkar-e-Islam (India)"] <- 63
origins$group[origins$group_txt == "Liberation of Achik Elite Force (LAEF)"] <- 121
origins$group[origins$group_txt == "Manipur Naga People's Army (MNPA)"] <- 96
origins$group[origins$group_txt == "Manipur Nationalist Revolutionary Party (MNRP)"] <- 108
origins$group[origins$group_txt == "Maoist Communist Center (MCC)"] <- 47
origins$group[origins$group_txt == "Maoist Communist Party of Manipur"] <- 103
origins$group[origins$group_txt == "Maoists"] <- 3
origins$group[origins$group_txt == "Militants"] <- 67
origins$group[origins$group_txt == "Miscreants"] <- 97
origins$group[origins$group_txt == "Moran Tiger Force (MTF)"] <- 33
origins$group[origins$group_txt == "Muslim extremists"] <- 56
origins$group[origins$group_txt == "Naga National Council (NNC)"] <- 123
origins$group[origins$group_txt == "Naga People"] <- 36
origins$group[origins$group_txt == "National Democratic Front of Bodoland (NDFB)"] <- 9
origins$group[origins$group_txt == "National Liberation Council of Taniland"] <- 42
origins$group[origins$group_txt == "National Liberation Force of Bengalis (Bangali Janamukti Bahini)"] <- 20
origins$group[origins$group_txt == "National Revolutionary Front of Manipur (NRFM)"] <- 92
origins$group[origins$group_txt == "National Santhali Liberation Army (NSLA)"] <- 43
origins$group[origins$group_txt == "National Socialist Council of Nagaland-Isak-Muivah (NSCN-IM)"] <- 8
origins$group[origins$group_txt == "National Socialist Council of Nagaland-Khole-Kitovi (NSCN-KK)"] <- 124
origins$group[origins$group_txt == "National Socialist Council of Nagaland-Unification (NSCN-U)"] <- 110
origins$group[origins$group_txt == "Naxalites"] <- 49
origins$group[origins$group_txt == "Pahadi Cheetah"] <- 76
origins$group[origins$group_txt == "Pattali Makkal Katchi (PMK)"] <- 134
origins$group[origins$group_txt == "People's Committee against Police Atrocities (PCPA)"] <- 139
origins$group[origins$group_txt == "People's Liberation Army (India)"] <- 15
origins$group[origins$group_txt == "People's Liberation Front of India"] <- 50
origins$group[origins$group_txt == "People's Revolutionary Party of Kangleipak (PREPAK)"] <- 91
origins$group[origins$group_txt == "People's Revolutionary Party of Kangleipak-Progressive (PREPAK-P)"] <- 98
origins$group[origins$group_txt == "People's United Liberation Front (PULF)"] <- 77
origins$group[origins$group_txt == "People's War Group (PWG)"] <- 71
origins$group[origins$group_txt == "Popular Front of India"] <- 85
origins$group[origins$group_txt == "Puratchi Puligal"] <- 133
origins$group[origins$group_txt == "Rabha National Security Force"] <- 38
origins$group[origins$group_txt == "Rashtriya Swayamsevak Sangh"] <- 83
origins$group[origins$group_txt == "Revolutionary Communist Centre (RCC)"] <- 51
origins$group[origins$group_txt == "Right-wing extremists"] <- 81
origins$group[origins$group_txt == "Sanatan Sanstha"] <- 79
origins$group[origins$group_txt == "Save Kashmir Movement"] <- 58
origins$group[origins$group_txt == "Separatists"] <- 61
origins$group[origins$group_txt == "Shiv Sena"] <- 86
origins$group[origins$group_txt == "Students Islamic Movement of India (SIMI)"] <- 55
origins$group[origins$group_txt == "Tamil Nadu Liberation Army"] <- 126
origins$group[origins$group_txt == "Tehrik al-Mojahedin"] <- 64
origins$group[origins$group_txt == "Tehrik-e-Galba Islam"] <- 5
origins$group[origins$group_txt == "Tehrik-e-Hurriyat (TeH)"] <- 70
origins$group[origins$group_txt == "Telangana Separatists"] <- 6
origins$group[origins$group_txt == "Thanthai Periyar Dravidar Kazhagam (TPDK)"] <- 135
origins$group[origins$group_txt == "Tiwa Liberation Army"] <- 31
origins$group[origins$group_txt == "Tribal Revolutionary Army (TRA)"] <- 107
origins$group[origins$group_txt == "Tribesmen"] <- 26
origins$group[origins$group_txt == "Tritiya Prastuti Committee (TPC)"] <- 48
origins$group[origins$group_txt == "United Achik Liberation Army (UALA)"] <- 46
origins$group[origins$group_txt == "United Bengali Liberation Front (UBLF)"] <- 40
origins$group[origins$group_txt == "United Democratic Liberation Army (UDLA)"] <- 19
origins$group[origins$group_txt == "United Democratic Terai Liberation Front (UDTLF)"] <- 129
origins$group[origins$group_txt == "United Garo Security Force (UGSF)"] <- 117
origins$group[origins$group_txt == "United Gorkha People's Organization (UGPO)"] <- 21
origins$group[origins$group_txt == "United Karbi Liberation Army (UKLA)"] <- 39
origins$group[origins$group_txt == "United Kuki Liberation Front (UKLF) - India"] <- 101
origins$group[origins$group_txt == "United Liberation Front of Assam (ULFA)"] <- 12
origins$group[origins$group_txt == "United Liberation Front of Barak Valley (ULFBV) - India"] <- 37
origins$group[origins$group_txt == "United National Liberation Front (UNLF)"] <- 44
origins$group[origins$group_txt == "United People's Democratic Solidarity (UPDS)"] <- 45
origins$group[origins$group_txt == "United Reformation Protest of India"] <- 32
origins$group[origins$group_txt == "United Tribal Liberation Army (UTLA)"] <- 109
origins$group[origins$group_txt == "Vishwa Hindu Parishad (VHP)"] <- 54
origins$group[origins$group_txt == "Volunteers of Innocent People of Nagas (VIPN)"] <- 102
origins$group[origins$group_txt == "Yimchunger Liberation Front (YLF)"] <- 125
origins$group[origins$group_txt == "Youths"] <- 136
origins$group[origins$group_txt == "Zeliangrong United Front"] <- 100
origins$group[origins$group_txt == "Zomi Revolutionary Army (ZRA)"] <- 105

# THE RULE FOR ASSIGNING GROUP ORIGINS
# Identify a state/union territory as a terrorist organization’s primary area of operation 
# if 90% of all attacks committed by that organization occurred in that state/union territory. 
# In cases where the organization’s attacks were dispersed across several states, 
# meaning that no state seemed like a primary area of operation, 
# identify each state/union territory that attracted (1) at least 25% of the attacks committed by an organization, and 
# (2) at least 10 attacks by an organization that make at least 10% of the all attacks committed by that organization,
# as the primary areas of operation of that organization.





