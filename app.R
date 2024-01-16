
library(shiny)
#calculate disparity grades for race subgroups  

#detach(plyr)
library(haven)
library(dplyr)
library(ggplot2)
library(openxlsx)

##################################################################################
# Load and fix data 
#################################################################################


# LOAD LARGE DATASET 

# fulldata = haven::read_sas("P:/CH-Ranking/Data/Cumulative Analytic Datasets/t_measure_data.sas7bdat")
# # setwd("H:/fun viz/RaceDisparities")
# library(dplyr)
# measuredata = measuredata %>% filter(measure_id %in% c(126, 54, 56,81,55,51,67,1,24, 63,129,128,127, 147,155,37))
# "Years of Potential Life Lost" = 1, 
# "Children in Poverty" = 24,
# "Median Household Income" = 63,
# "Infant Mortality" = 129,
# "Child Mortality" = 128,
# "Premature Mortality" = 127, 
# "Life Expectancy" = 147, 
# "Flu Vaccinations" = 155, 
# "Low Birthweight" = 37
# tot pop = 51
# white = 126
# black = 54
# hispanic = 56
# asian = 81
# aian = 55 


#save(measuredata, file ="H:/fun viz/RaceDisparities/measuredata.RData")


load("measuredata.RData")


###################################################################################
# FUNCTION TO VISUALIZE STATE RACIAL DISPARITIES 
###################################################################################
#year = 2021 
#measurenum = "1" 
#yaxis = "percentmiss"
#relativeto = "state"
#dispmeasure = "BGV"

#measurenum can be any of the CHRR vars that have race specific subgroups 
#relativeto refers to a state mean or a national mean comparison value 
#dispmeasure can be either theil or BGV 
#yaxis refers to the y axis: either percentmiss or state population 

measurecomp = function(year, measurenum, relativeto, dispmeasure, yaxis, legend = FALSE){
    ds = measuredata[which(measuredata$year==year & measuredata$state_fips != "11" & measuredata$county_fips != "000"),]
    
    mdata = ds[which(ds$measure_id == measurenum),]
    mdata$vals = mdata$raw_value
    
    
    pop = ds[which(ds$measure_id == "51"),]
    pop$pop = pop$raw_value
    
    popwhite = ds[which(ds$measure_id =="126"),]
    popblack = ds[which(ds$measure_id == "54"),]
    pophispanic = ds[which(ds$measure_id == "56"),]
    popasian = ds[which(ds$measure_id == "81"),]
    popaian = ds[which(ds$measure_id == "55"),]
    
    msubblack = mdata %>% group_by(state_fips) %>% summarise(sub = "black", submean = mean(race_black, na.rm = TRUE), nmiss = sum(is.na(race_black)), ncount = length(race_black))
    msubhispanic = mdata %>% group_by(state_fips) %>% summarise(sub= "hispanic", submean = mean(race_hispanic, na.rm = TRUE), nmiss = sum(is.na(race_hispanic)), ncount = length(race_hispanic))
    msubwhite = mdata %>% group_by(state_fips) %>% summarise(sub = "white", submean = mean(race_white, na.rm = TRUE), nmiss = sum(is.na(race_white)), ncount = length(race_white))
    msubasian = mdata %>% group_by(state_fips) %>% summarise(sub = "asian", submean = mean(race_asian, na.rm = TRUE), nmiss = sum(is.na(race_asian)), ncount = length(race_asian))
    msubaian = mdata %>% group_by(state_fips) %>% summarise(sub = "aian", submean = mean(race_aian, na.rm = TRUE), nmiss = sum(is.na(race_aian)), ncount = length(race_aian))
    
    psubblack = popblack %>% group_by(state_fips) %>% summarise(sub = "black", subpop = sum(numerator, na.rm = TRUE))
    psubhispanic = pophispanic %>% group_by(state_fips) %>% summarise(sub= "hispanic", subpop = sum(numerator, na.rm = TRUE))
    psubwhite = popwhite %>% group_by(state_fips) %>% summarise(sub = "white", subpop = sum(numerator, na.rm = TRUE))
    psubasian = popasian %>% group_by(state_fips) %>% summarise(sub = "asian", subpop = sum(numerator, na.rm = TRUE))
    psubaian = popaian %>% group_by(state_fips) %>% summarise(sub = "aian", subpop = sum(numerator, na.rm = TRUE))
    
    m1 = data.frame(msubblack$nmiss, msubwhite$nmiss, msubhispanic$nmiss, msubasian$nmiss, msubaian$nmiss)
    missings = data.frame(state_fips = msubblack$state_fips, nmiss = rowSums(m1), percentmiss = rowSums(m1)/(5*msubblack$ncount))
    
    
    
    black = merge(msubblack, psubblack, by = c("state_fips", "sub"))
    white = merge(msubwhite, psubwhite, by = c("state_fips", "sub"))
    asian = merge(msubasian, psubasian, by = c("state_fips", "sub"))
    aian = merge(msubaian, psubaian, by = c("state_fips", "sub"))
    hispanic = merge(msubhispanic, psubhispanic, by = c("state_fips", "sub"))
    
    
    statesubmeans = rbind(black, white, asian, aian, hispanic)
    
    states = aggregate(vals~ state_fips, data = mdata, FUN = function(x) c(smean = mean(x, na.rm = TRUE), ssd = sd(x)))
    spop = pop %>% group_by(state_fips) %>% summarise(spop = sum(pop, na.rm = TRUE))
    s = merge(statesubmeans, states, by= "state_fips")
    sp = merge(s, spop, by = "state_fips")
    
    mntl = mean(mdata$vals, na.rm = TRUE) #mdata already has state values removed (county level only) 
    ntlpop = sum(pop$pop, na.rm = TRUE) #pop is county level only  
    
    dispsub = sp %>% group_by(state_fips) %>% summarise(bgvsubstate = sum((subpop/spop) * (submean - vals[,1])^2, na.rm = TRUE),
                                                        bgvsubntl = sum((subpop/ntlpop) * (submean - mntl)^2, na.rm = TRUE),
                                                        tsubstate = sum((subpop/spop) * (submean / vals[,1]) * log(submean / vals[,1]), na.rm = TRUE),
                                                        tsubntl = sum((subpop/ntlpop) * (submean / mntl) * log(submean/ mntl), na.rm = TRUE))
    
    
    
    #add state name to dataset so it's easier to understand/interpret 
    fips = read.csv("us-state-ansi-fips.csv")
    fips$st = formatC(fips$st, width = 2, format = "d", flag = "0")
    dispsub = merge(fips, dispsub, by.x = "st", by.y = "state_fips")
    
    #also adding population and percent missingness values  
    dispsub = merge(spop, dispsub, by.x = "state_fips", by.y = "st")
    dispsub = merge(missings, dispsub, by = "state_fips")
    
    xaxis = ifelse(relativeto == "state", 
                   ifelse(dispmeasure == "BGV", 
                          "bgvsubstate", 
                          "tsubstate"),
                   ifelse(dispmeasure == "BGV", 
                          "bgvsubntl",
                          "tsubntl"))
    
    title = ifelse(relativeto == "state", 
                   ifelse(dispmeasure == "BGV", 
                          "Disparities measured using BGV relative to State mean", 
                          "Disparities measured using Theil index relative to State mean"),
                   ifelse(dispmeasure == "BGV", 
                          "Disparities measured using BGV relative to National mean",
                          "Disparities measured using Theil index relative to National mean"))
    ytitle = ifelse(yaxis == "spop", 
                    "State population", "Percent of counties per state missing race subgroup data")
    
    pplot = ggplot(dispsub, aes(get(xaxis), get(yaxis))) + 
        geom_text(aes(label = stusps, color = percentmiss, size = spop), show.legend = FALSE) + 
        labs(x = "Race Disparities", 
             y = ytitle,  
             title = title) +
        theme(legend.position = "none") + 
        #scale_size(range = c(5, 15), breaks = 1000000 * c(1, 2, 4, 8, 16, 32), labels = c(1, 2, 4, 8, 16, 32)) + 
        theme_bw()
    
    return(pplot)
    
    
}

# measurecomp(67) #Driving alone to work 
# measurecomp(1) #Premature death 
# measurecomp(129) #Infant mortality - need to fix axis limits 
# measurecomp(128) #Child mortality 
# measurecomp(37) #low birthweight 
 









# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Racial Disparities across U.S. States"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("measurenum", "Which measure of health?",
                        choices = c("Years of Potential Life Lost" = 1, 
                                    "Children in Poverty" = 24,
                                    "Median Household Income" = 63,
                                    "Infant Mortality" = 129,
                                    "Child Mortality" = 128,
                                    "Premature Mortality" = 127, 
                                    "Life Expectancy" = 147, 
                                    "Flu Vaccinations" = 155, 
                                    "Low Birthweight" = 37)),
            selectInput("year", "Which year?",
                        choices = NULL),
            selectInput("yaxis", "Compare disparities to state population or to the percent of state counties missing racial subgroup data?",
                        choices = c("State Population" = "spop", 
                                    "Racial Data Missingness" = "percentmiss")),
            selectInput("relativeto", "Measure disparities relative to the national mean or to each state's mean?",
                        choices = c("State"="state", "National" = "ntl")),
            selectInput("dispmeasure", "Which numeric formula for disparity measurement?",
                        choices = c("BGV"="BGV", "Theil" = "theil"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dplot")
        )
    ))


# Define server logic required to draw plot
server <- function(input, output, session) {

    observeEvent(input$measurenum, {
        updateSelectInput(
            session,
            "year",
            choices = unique(measuredata$year[!is.na(measuredata$race_white) & measuredata$measure_id == input$measurenum])           
        )
    })
    
    
    output$dplot <- renderPlot({
       
        measurecomp(year=input$year, 
                    measurenum = input$measurenum, 
                    yaxis = input$yaxis, 
                    dispmeasure = input$dispmeasure, 
                    relativeto = input$relativeto,
                    legend = FALSE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
