library(rvest)
library(stringr)
library(xlsx)

#reading in the html from the WFTDA website
derbyParse <- read_html("https://wftda.com/wftda-leagues/")

#selecting all of the class div
derbyNodes <- html_nodes(derbyParse, "div")

#finding where 'col-xs-6 col-sm-4 col-md-3 col-lg-2 text-center leagues-item grid-item league_type-member' is in derbyNodes
#removing where they are not
index <- str_detect(derbyNodes, "col-xs-6 col-sm-4 col-md-3 col-lg-2 text-center leagues-item grid-item league_type-member")
derbyTeams <- derbyNodes[index]
derbyTeams <- derbyTeams[-1:-5]

#pulling where the names of roller derby teams are and removing special characters or those not needed
derbyNames <- html_nodes(derbyTeams, "h5")
derbyNames <- str_replace(derbyNames, ".*\">(.+)</span></a></h5>","\\1")
derbyNames <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", derbyNames)


#Locale
#pulling cities and states and removing unwanted characters
derbyLocation <- html_nodes(derbyTeams, "div")
indexLocation <- str_detect(derbyLocation, "league-meta league-location")
derbyLocation <- derbyLocation[indexLocation]
derbyLocation <- str_split(derbyLocation, "span")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[-grep("div|class|span", derbyLocation)]
derbyLocation <- str_split(derbyLocation, "\t")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[-grep(">\n", derbyLocation)]
derbyLocation <- derbyLocation[grep(" ", derbyLocation)]
derbyLocation <- str_split(derbyLocation, "<")
derbyLocation <- unlist(derbyLocation)
derbyLocation <- derbyLocation[grep(",", derbyLocation)]
derbyLocation <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", derbyLocation)
derbyLocation <- trimws(derbyLocation)

#derbyLocation <- gsub("[A-z] [A-z]", "[A-z], [A-z]", derbyLocation)

#Country
#pulling country and removing unwanted characters
derbyCountry <- html_nodes(derbyTeams, "div")
derbyCountry <- html_nodes(derbyCountry, "span")
indexCountry <- str_detect(derbyCountry, "league_country")
derbyCountry <- derbyCountry[indexCountry]
derbyCountry <- str_replace(derbyCountry, ".*\">(.+)</span>", "\\1")
derbyCountry[derbyCountry == "us"] <- "United States"
derbyCountry[derbyCountry == "ar"] <- "Argentina"
derbyCountry[derbyCountry == "at"] <- "Austria"
derbyCountry[derbyCountry == "au"] <- "Australia"
derbyCountry[derbyCountry == "be"] <- "Belgium"
derbyCountry[derbyCountry == "br"] <- "Brazil"
derbyCountry[derbyCountry == "ca"] <- "Canada"
derbyCountry[derbyCountry == "cl"] <- "Chile"
derbyCountry[derbyCountry == "ch"] <- "Switzerland"
derbyCountry[derbyCountry == "co"] <- "Colombia"
derbyCountry[derbyCountry == "cz"] <- "Czech Republic"
derbyCountry[derbyCountry == "de"] <- "Germany"
derbyCountry[derbyCountry == "dk"] <- "Denmark"
derbyCountry[derbyCountry == "es"] <- "Spain"
derbyCountry[derbyCountry == "fi"] <- "Finland"
derbyCountry[derbyCountry == "fr"] <- "France"
derbyCountry[derbyCountry == "gb"] <- "Great Britain"
derbyCountry[derbyCountry == "ie"] <- "Ireland"
derbyCountry[derbyCountry == "is"] <- "Iceland"
derbyCountry[derbyCountry == "it"] <- "Italy"
derbyCountry[derbyCountry == "jp"] <- "Japan"
derbyCountry[derbyCountry == "mx"] <- "Mexico"
derbyCountry[derbyCountry == "nl"] <- "Netherlands"
derbyCountry[derbyCountry == "no"] <- "Norway"
derbyCountry[derbyCountry == "nz"] <- "New Zealand"
derbyCountry[derbyCountry == "pe"] <- "Peru"
derbyCountry[derbyCountry == "pl"] <- "Poland"
derbyCountry[derbyCountry == "pt"] <- "Portugal"
derbyCountry[derbyCountry == "se"] <- "Sweden"
derbyCountry[derbyCountry == "za"] <- "South Africa"
derbyCountry <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", derbyCountry)

#logo
#pulling png of logo and removing unwanted character and replacing them to complete link
derbyLogo <- html_nodes(derbyTeams, "img")
indexLogo <- str_detect(derbyLogo, "img src")
derbyLogo <- derbyLogo[indexLogo]
derbyLogo <- str_split(derbyLogo, "=")
derbyLogo <- unlist(derbyLogo)
derbyLogo <- derbyLogo[grep("https", derbyLogo)]
derbyLogo <- str_replace(derbyLogo, "alt", "")
derbyLogo <- gsub('"', '', derbyLogo)
derbyLogo <- gsub(' ', '', derbyLogo)

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
derbyLink <- str_replace(derbyLink, "\ ", "")
derbyLink <- gsub('"', '', derbyLink)

#data frame
data <- data.frame(Names = derbyNames,
                         Location = derbyLocation, 
                         Country =derbyCountry,
                         Logo = derbyLogo,
                         Websites = as.character(derbyLink))

write.csv(data, file = "WFTDA_data.csv")

