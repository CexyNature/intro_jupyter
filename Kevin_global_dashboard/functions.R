
get_data <- function(File.Name=file.name) {
	require(tidyverse)
	require(countrycode)
	
	baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
	
	loadData = function(fileName, columnName) {
		if(!file.exists(fileName)) {
			data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
				select(-Lat, -Long) %>% 
				pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
				mutate(
					date=as.Date(date, format="%m/%d/%y"),
					`Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
					`Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
				)
		} else {
			load(file=fileName)
		}
		return(data)
	}
	
	allData = 
		loadData(
			"time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
		inner_join(loadData(
			"time_series_covid19_deaths_global.csv", "CumDeaths"),
			by = c("Province/State", "Country/Region", "date")) %>%
		inner_join(loadData(
			"time_series_covid19_recovered_global.csv","CumRecovered"),
			by = c("Province/State", "Country/Region", "date"))
	
	global_data = allData %>% 
		group_by(date, `Country/Region`) %>% 
		summarise_if(is.numeric, sum, na.rm=TRUE) %>%
		select(country = `Country/Region`, date, 
			   cumulative_deaths = CumDeaths, 
			   cumulative_recovered = CumRecovered, 
			   cumulative_confirmed = CumConfirmed) #%>%
		# mutate(
		# 	dateStr = format(date, format="%b %d, %Y"),
		# 	NewConfirmed=cumulative_confirmed - lag(cumulative_confirmed, default=0),
		# 	NewRecovered=cumulative_recovered - lag(cumulative_recovered, default=0),
		# 	NewDeaths   =cumulative_deaths    - lag(cumulative_deaths, default=0)
		# )
	
	## combine csv file with global population totals data:
	## Data originally from : https://data.worldbank.org/indicator/SP.POP.TOTL

	# install.packages("countrycode")
	require(countrycode)
	total_pop <- read.csv("worldbank_total_pop.csv") %>% filter(year==2020)
	suppressWarnings({
		global_data$country_code <- countrycode(global_data$country, # use countrycode pkg to get country ISO-3 letter code
									  origin="country.name", destination="iso3c")
		})
	global_data <- global_data %>% left_join(select(total_pop, -country), by=c("country_code")) %>% 
		select(-country_code, -year) # Take only cumulatives (commented off new cases above)
	global_data <- global_data %>% # filter all countries with no deaths, no population total
		filter(cumulative_deaths > 0 & !is.na(pop_total))
	
	# Convert data to dates
	global_data <- global_data %>% 
		mutate(date = as.Date(date))
	
	# calculate first date 
	# (data already subsetted as cumulative_deaths > 0)
	suppressMessages({first_date <- global_data %>% 
		group_by(country) %>% 
		arrange(date) %>%
		summarise(first_date=as.Date(head(date,1)))})
	
	suppressMessages({last_date <- global_data %>% 
		group_by(country) %>% 
		arrange(date) %>%
		summarise(last_date=as.Date(tail(date,1)))})
	
	# calculate days since first date
	global_data <- global_data %>%
		left_join(first_date, by="country") %>%
		left_join(last_date, by="country") %>%
		mutate(days_since_first = as.numeric(date - first_date)) %>%
		arrange(country, date)
	
	
	write.csv(global_data, file=File.Name, row.names=F)
}
