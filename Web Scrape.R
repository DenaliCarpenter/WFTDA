install.packages("rvest")
install.packages("stringr")
install.packages("xlsx")

library(rvest)
library(stringr)
library(xlsx)

derbyParse <- read_html("https://wftda.com/wftda-leagues/")
str(derbyParse)

derbyNodes <- html_nodes(derbyParse, "div")
derbyNodes

index <- str_detect(derbyNodes, "col-xs-6 col-sm-4 col-md-3 col-lg-2 text-center leagues-item grid-item league_type-member")

derbyTeams <- derbyNodes[index]
derbyTeams <- derbyTeams[-1:-5]
derbyTeams

derbyNames <- html_nodes(derbyTeams, "h5")
derbyNames

derbyNames <- str_replace(derbyNames, ".*\">(.+)</span></a></h5>","\\1")


#Locale
derbyLocation <- html_nodes(derbyTeams, "div")
indexLocation <- str_detect(derbyLocation, "league-meta league-location")
derbyLocation <- derbyLocation[indexLocation]
derbyLocation <- str_split(derbyLocation, "span")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[-grep("div|class|span", derbyLocation)]
derbyLocation <- str_split(derbyLocation, "\t")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[-grep(">\r\n", derbyLocation)]
derbyLocation <- derbyLocation[grep(" ", derbyLocation)]
derbyLocation <- str_split(derbyLocation, "<")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[grep(",", derbyLocation)]

#Country
derbyCountry <- html_nodes(derbyTeams, "div")
derbyCountry <- html_nodes(derbyCountry, "span")
indexCountry <- str_detect(derbyCountry, "league_country")
derbyCountry <- derbyCountry[indexCountry]
derbyCountry <- str_replace(derbyCountry, ".*\">(.+)</span>", "\\1")

#logo
derbyLogo <- html_nodes(derbyTeams, "img")
indexLogo <- str_detect(derbyLogo, "img src")
derbyLogo <- derbyLogo[indexLogo]
derbyLogo <- str_split(derbyLogo, "=")
derbyLogo <- unlist(derbyLogo)
derbyLogo <- derbyLogo[grep("https", derbyLogo)]
derbyLogo <- str_replace(derbyLogo, "alt", "")
derbyLogo <- str_replace(derbyLogo, "https", "http")

#link
derbyLink <- html_nodes(derbyTeams, "a")
indexLink <- str_detect(derbyLink, "league_name")
derbyLink <- derbyLink[indexLink]
derbyLink <- str_split(derbyLink, "><span")
derbyLink <- unlist(derbyLink)
derbyLink <- derbyLink[grep("href", derbyLink)]
derbyLink <- str_split(derbyLink, "=")
derbyLink <- unlist(derbyLink)
derbyLink <- derbyLink[-grep("href", derbyLink)]

derbyLink <- paste("https://wftda.com", derbyLink)
derbyLink <- str_replace(derbyLinks, "\ ", "")

links <- data.frame(Websites = derbyLink)
write.csv(links, file = "Websites.csv")


#data frame
WFTDA_data <- data.frame(Names = derbyNames,
                         Location = derbyLocation, 
                         Country =derbyCountry,
                         Logo = derbyLogo,
                         Websites = derbyLink)



WFTDA <- WFTDA_data
WFTDA$Country <- as.character(WFTDA$Country)
Leagues <- as.character(WFTDA$Names)
Leagues <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", Leagues)

WFTDA$Country[WFTDA$Country == "us"] <- "United States"
WFTDA$Country[WFTDA$Country == "ar"] <- "Argentina"
WFTDA$Country[WFTDA$Country == "at"] <- "Austria"
WFTDA$Country[WFTDA$Country == "au"] <- "Australia"
WFTDA$Country[WFTDA$Country == "be"] <- "Belgium"
WFTDA$Country[WFTDA$Country == "br"] <- "Brazil"
WFTDA$Country[WFTDA$Country == "ca"] <- "Canada"
WFTDA$Country[WFTDA$Country == "cl"] <- "Chile"
WFTDA$Country[WFTDA$Country == "ch"] <- "Switzerland"
WFTDA$Country[WFTDA$Country == "co"] <- "Colombia"
WFTDA$Country[WFTDA$Country == "cz"] <- "Czech Republic"
WFTDA$Country[WFTDA$Country == "de"] <- "Germany"
WFTDA$Country[WFTDA$Country == "dk"] <- "Denmark"
WFTDA$Country[WFTDA$Country == "es"] <- "Spain"
WFTDA$Country[WFTDA$Country == "fi"] <- "Finland"
WFTDA$Country[WFTDA$Country == "fr"] <- "France"
WFTDA$Country[WFTDA$Country == "gb"] <- "Great Britain"
WFTDA$Country[WFTDA$Country == "ie"] <- "Ireland"
WFTDA$Country[WFTDA$Country == "is"] <- "Iceland"
WFTDA$Country[WFTDA$Country == "it"] <- "Italy"
WFTDA$Country[WFTDA$Country == "jp"] <- "Japan"
WFTDA$Country[WFTDA$Country == "mx"] <- "Mexico"
WFTDA$Country[WFTDA$Country == "nl"] <- "Netherlands"
WFTDA$Country[WFTDA$Country == "no"] <- "Norway"
WFTDA$Country[WFTDA$Country == "nz"] <- "New Zealand"
WFTDA$Country[WFTDA$Country == "pe"] <- "Peru"
WFTDA$Country[WFTDA$Country == "pl"] <- "Poland"
WFTDA$Country[WFTDA$Country == "pt"] <- "Portugal"
WFTDA$Country[WFTDA$Country == "se"] <- "Sweden"
WFTDA$Country[WFTDA$Country == "za"] <- "South Africa"

Country <- WFTDA$Country
Country <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", Country)
Location <- as.character(WFTDA$Location)
Location <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", Location)
Logo <- WFTDA$Logo
Website <- WFTDA$Websites
data <- data.frame(Names = Leagues, 
                   Location = Location, 
                   Country = Country, 
                   Logo = Logo, 
                   Website = Website)


write.csv(data, file = "WFTDA_data.csv")


summary(WFTDA_data$Country)/460
#0.87% from ar
#0.22% from at
#3.48% from au
#0.87% from be
#4.34% from br
#5.87% from ca
#0.22% from ch
#0.22% from cl
#0.43% from co
#0.43% from cz
#2.39% from de
#0.43% from dk
#0.65% from es
#1.09% from fi
#2.83% from fr
#5.43% from gb
#0.43% from ie
#0.22% from is
#0.65% from it
#0.43% from jp
#0.43% from mx
#1.09% from nl
#0.65% from no
#1.52% from nz
#0.22% from pe
#0.22% from pl
#0.22% from pt
#1.30% from se
#66.30% from us
#0.43% from za

unique(WFTDA_data$Location)
