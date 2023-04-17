#Install package "rvest'
#install.packages('rvest')
library('rvest')
library('xml2')
library('ggplot2')
library('dplyr')
library('stringr')
#Create a dataframe from the information provided on the site
#2023
url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2023')
nodes = html_nodes(url, 'table'); nodes
df2023 = html_table(nodes[[2]])%>%as.data.frame()
df2023 <- df2023[df2023$Country == 'Turkey' | df2023$Country == 'Greece' | df2023$Country == 'Egypt' | df2023$Country == 'Australia' |df2023$Country == 'New Zealand',]
df2023$Year <- rep(2023, 5)
#2022
url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2022')
nodes = html_nodes(url, 'table'); nodes
df2022 = html_table(nodes[[2]])%>%as.data.frame()
df2022 <- df2022[df2022$Country == 'Turkey' | df2022$Country == 'Greece' | df2022$Country == 'Egypt' | df2022$Country == 'Australia' |df2022$Country == 'New Zealand',]
df2022$Year <- rep(2022, 5)
#2021
url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
nodes = html_nodes(url, 'table'); nodes
df2021 = html_table(nodes[[2]])%>%as.data.frame()
df2021 <- df2021[df2021$Country == 'Turkey' | df2021$Country == 'Greece' | df2021$Country == 'Egypt' | df2021$Country == 'Australia' |df2021$Country == 'New Zealand',]
df2021$Year <- rep(2021, 5)
#2020
url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
nodes = html_nodes(url, 'table'); nodes
df2020 = html_table(nodes[[2]])%>%as.data.frame()
df2020 <- df2020[df2020$Country == 'Turkey' | df2020$Country == 'Greece' | df2020$Country == 'Egypt' | df2020$Country == 'Australia' |df2020$Country == 'New Zealand',]
df2020$Year <- rep(2020, 5)
#Create dataframes for 5 countres (Турция, Греция, Египет, Австралия, Новая Зеландия)
df_all_year <- rbind(df2020, df2021, df2022, df2023)
#Create diagrams 
ggplot(data = df_all_year, aes(x = Year, y = `Quality of Life Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Quality of Life Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Purchasing Power Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Purchasing Power Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Safety Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Safety Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Health Care Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Health Care Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Cost of Living Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Cost of Living Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Property Price to Income Ratio`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Property Price to Income Ratio") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Traffic Commute Time Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Traffic Commute Time Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Pollution Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Pollution Index") + ylab('Index')
ggplot(data = df_all_year, aes(x = Year, y = `Climate Index`, group = Country, color = Country)) + geom_line(linewidth = 1) + geom_point() + ggtitle("Climate Index") + ylab('Index')
#Create data frame from "(https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/"
#links
url <- read_html("https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/")
selector_name <- ".post-list-item-title-link"
links <- html_nodes(url, selector_name) %>% html_attr('href')
#Adress
selector_name <- 'span.addressItem'
addres <- html_nodes(url, selector_name) %>% html_text()%>% str_remove_all("\\n|\\t")%>%as.array()
addres <- addres[-c(25,27)]
#Names
selector_name <- 'h4.post-list-item-title'
names <- html_nodes(url, selector_name) %>% html_text()%>% str_remove_all("\\n|\\t")%>%str_remove_all("        ")%>%as.array()

df_museums <- data.frame(names, links, addres)
df_museums

