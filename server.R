
shinyServer(function(input, output) {
    
#### data reading ####
    
    confirmed_cases_df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
        select(-`Province/State`) %>%
        pivot_longer(cols = -(`Country/Region`:`Long`), values_to = "cases", names_to = "date_text") %>%
        mutate(date = mdy(date_text)) %>%
        mutate(week = week(date)) %>%
        arrange(`Country/Region`, date) %>%
        group_by(`Country/Region`, date) %>%
        summarise(cases = sum(cases)) %>%
        mutate(weekly_running_increase = cases - lag(cases, n = 7)) %>%
        mutate(daily_running_increase = cases - lag(cases, n = 1)) %>%
        ungroup()
    
    recovered_df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>%
        select(-`Province/State`) %>%
        pivot_longer(cols = -(`Country/Region`:`Long`), values_to = "recovered", names_to = "date_text") %>%
        mutate(date = mdy(date_text)) %>%
        mutate(week = week(date)) %>%
        arrange(`Country/Region`, date) %>%
        group_by(`Country/Region`, date) %>%
        summarise(recovered = sum(recovered)) %>%
        ungroup()
    
    deaths_df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
        select(-`Province/State`) %>%
        pivot_longer(cols = -(`Country/Region`:`Long`), values_to = "deaths", names_to = "date_text") %>%
        mutate(date = mdy(date_text)) %>%
        mutate(week = week(date)) %>%
        arrange(`Country/Region`, date) %>%
        group_by(`Country/Region`, date) %>%
        summarise(deaths = sum(deaths)) %>%
        mutate(daily_deaths = deaths - lag(deaths, n = 1)) %>%
        ungroup()
    
    stringency_index_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
                                    col_types = cols(RegionName = col_character(), 
                                                     RegionCode = col_character())) %>%
        mutate(date = ymd(Date)) %>%
        mutate(`Country/Region` = case_when(
            CountryName == "United States" ~ "US",
            CountryName == "South Korea" ~ "Korea, South",
            TRUE ~ CountryName
        )) %>%
        select(`Country/Region`, date, StringencyIndex)
    
    
    merged_cases <- confirmed_cases_df %>%
        left_join(recovered_df) %>%
        left_join(deaths_df) %>%
        left_join(stringency_index_df) %>%
        mutate(active_cases = cases - (deaths + recovered))
    

#### RKI function ####    
    
    est_rt_rki_last <- function(ts, GT=4L) {
        # Sanity check
        if (!is.integer(GT) | !(GT>0)) stop("GT has to be postive integer.")
        # Estimate, if s=1 is t-7
        res <- sapply( (2*GT):length(ts), function(t) {
            if (sum(ts[t-2*GT+1:GT], na.rm = T) == 0 | is.na(sum(ts[t-2*GT+1:GT], na.rm = T))) {
                NA
            } else {
                sum(ts[t-(0:(GT-1))], na.rm = T) / sum(ts[t-2*GT+1:GT], na.rm = T)
            }
        })
        
        return(res)
    }
    
    GT <- 4L
    
    if(exists("RKI_data")) {rm(RKI_data)}
    
    
    for (c in unique(confirmed_cases_df$`Country/Region`)) {
        data <- confirmed_cases_df %>%
            filter(`Country/Region` == c)
        
        if (exists("RKI_data")) {
            RKI_data <- bind_rows(
                RKI_data,
                data.frame(Country = c,
                           date = data[(2*GT):nrow(data), "date"],
                           new_cases = data[(2*GT):nrow(data), "daily_running_increase"],
                           R = est_rt_rki_last(data$daily_running_increase, GT = GT))
            )
        } else {
            RKI_data <- data.frame(Country = c,
                                   date = data[(2*GT):nrow(data), "date"],
                                   new_cases = data[(2*GT):nrow(data), "daily_running_increase"],
                                   R = est_rt_rki_last(data$daily_running_increase, GT = GT))
        }
    }
    

#### output ####
    
    output$daily_deaths_graph <- renderPlot({
        
        merged_cases %>%
            select(`Country/Region`, date, daily_deaths) %>%
            filter(`Country/Region` == input$country) %>%
            ggplot(aes(x = date, y = daily_deaths)) + 
            geom_line(size = 1) +
            ggtitle("Deaths") +
            ylab("Deaths") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m")})
    
    output$daily_increase_graph <- renderPlot({
        
        merged_cases %>%
            select(`Country/Region`, date, weekly_running_increase, daily_running_increase) %>%
            filter(`Country/Region` == input$country) %>%
            ggplot(aes(x = date, y = daily_running_increase)) + 
            geom_line(size = 1) +
            ggtitle("New cases daily") +
            ylab("Daily new cases") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m")})

    output$stringency_graph <- renderPlot({
    
        merged_cases %>%
            select(`Country/Region`, date, StringencyIndex) %>%
            filter(`Country/Region` == input$country) %>%
            ggplot(aes(x = date, y = StringencyIndex)) + 
            geom_line(size = 1) +
            ggtitle("Oxford Govt. Response Stringency Index") +
            ylab("Stringency Index") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m")}) 
        
    output$RKI_graph <- renderPlot({
        
        RKI_data %>%
            filter(Country == input$country) %>%
            mutate(R = ifelse(R>=2, 2, R)) %>%
            ggplot(aes(x = date, y = R, color = (R==2))) + 
            geom_line(size = 1, aes(group = 1)) +
            scale_color_manual(values=c("#000000", "#FF0000", "#000000")) +
            ylim(c(0,2)) +
            ggtitle("Effective reproduction rate (Rt)") +
            geom_hline(yintercept = 1, color = "red") +
            ylab("Rt") +
            theme(legend.position = "none") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m")})

})
