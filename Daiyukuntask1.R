#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library packages
library(shiny)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(fpp2)


#get covid confirmed data from the github page's html node
web_global_confirm <- read_html(str_c("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), encoding = "UTF-8")
web_global_confirm_info<-web_global_confirm%>%html_nodes("tr")%>%html_text()
#Data processing
web_global_confirm_info_1<-gsub('\n', '', web_global_confirm_info, fixed=TRUE)
web_global_confirm_info_2<-gsub(' ', '', web_global_confirm_info_1, fixed=TRUE)
a<-data.frame(web_global_confirm_info_2)
confirm_colname<-c(strsplit(a[1,1], ',', fixed=TRUE)[[1]])
global_confirmdata<- matrix(nrow=278,ncol=length(confirm_colname))
colnames(global_confirmdata)<-confirm_colname
world_confirmdata<-data.frame(global_confirmdata)
#Write data into the created dataframe
i=2
repeat { 
    x<-c(strsplit(a[i,1], ',', fixed=TRUE)[[1]])
    province<-x[1]
    country<-x[2]
    loc<-as.numeric(x[3:4])
    num<-as.integer(x[5:length(confirm_colname)])
    world_confirmdata[i-1,1]<-province
    world_confirmdata[i-1,2]<-country
    world_confirmdata[i-1,3:4]<-loc
    world_confirmdata[i-1,5:length(confirm_colname)]<-num
    i=i+1
    if(i==280) {
        break
    }
}
#name the column
names(world_confirmdata)<-confirm_colname
#Combine the two columns of province and country
regname<-c(paste(c(world_confirmdata[,2]),c(world_confirmdata[,1])))


#get covid death data from the github page's html node
web_global_coviddeath <- read_html(str_c("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), encoding = "UTF-8")
web_global_coviddeath_info<-web_global_coviddeath%>%html_nodes("tr")%>%html_text()
#Data processing
web_global_coviddeath_info_1<-gsub('\n', '', web_global_coviddeath_info, fixed=TRUE)
web_global_coviddeath_info_2<-gsub(' ', '', web_global_coviddeath_info_1, fixed=TRUE)
b<-data.frame(web_global_coviddeath_info_2)
coviddeath_colname<-c(strsplit(b[1,1], ',', fixed=TRUE)[[1]])
global_coviddeath_data<- matrix(nrow=279,ncol=length(coviddeath_colname))
colnames(global_coviddeath_data)<-coviddeath_colname
world_coviddeath_data<-data.frame(global_coviddeath_data)
#Write data into the created dataframe
j=2
repeat { 
    y<-c(strsplit(b[j,1], ',', fixed=TRUE)[[1]])
    province_2<-y[1]
    country_2<-y[2]
    loc_2<-as.numeric(y[3:4])
    num_2<-as.integer(y[5:length(coviddeath_colname)])
    world_coviddeath_data[j-1,1]<-province_2
    world_coviddeath_data[j-1,2]<-country_2
    world_coviddeath_data[j-1,3:4]<-loc_2
    world_coviddeath_data[j-1,5:length(coviddeath_colname)]<-num_2
    j=j+1
    if(j==280) {
        break
    }
}
#name the column
names(world_coviddeath_data)<-coviddeath_colname
#Combine the two columns of province and country
regname_d<-c(paste(c(world_coviddeath_data[,2]),c(world_coviddeath_data[,1])))



#get covid recover data from the github page's html node
web_global_recover <- read_html(str_c("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), encoding = "UTF-8")
web_global_recover_info<-web_global_recover%>%html_nodes("tr")%>%html_text()
#Data processing
web_global_recover_info_1<-gsub('\n', '', web_global_recover_info, fixed=TRUE)
web_global_recover_info_2<-gsub(' ', '', web_global_recover_info_1, fixed=TRUE)
c<-data.frame(web_global_recover_info_2)
recover_colname<-c(strsplit(c[1,1], ',', fixed=TRUE)[[1]])
global_recover_data<- matrix(nrow=264,ncol=length(recover_colname))
colnames(global_recover_data)<-recover_colname
world_recover_data<-data.frame(global_recover_data)
#Write data into the created dataframe
k=2
repeat { 
    z<-c(strsplit(c[k,1], ',', fixed=TRUE)[[1]])
    province_3<-z[1]
    country_3<-z[2]
    loc_3<-as.numeric(z[3:4])
    num_3<-as.integer(z[5:length(recover_colname)])
    world_recover_data[k-1,1]<-province_3
    world_recover_data[k-1,2]<-country_3
    world_recover_data[k-1,3:4]<-loc_3
    world_recover_data[k-1,5:length(recover_colname)]<-num_3
    k=k+1
    if(k==265) {
        break
    }
}
#name the column
names(world_recover_data)<-recover_colname
#Combine the two columns of province and country
regname_c<-c(paste(c(world_coviddeath_data[,2]),c(world_coviddeath_data[,1])))


# Define UI for the application
ui <- fluidPage(
    navbarPage("global covid data",
               #the main page
               tabPanel("main page",
                        titlePanel("covid data forecast"),
                        mainPanel(
                            helpText("This task is first getting 
                                     the covid data global from the github page's html node 
                                     then use the ETL model to forecast the next 10 day's  possible data")
                        )
                        
                        ),
               #the covid confirmed data page
               tabPanel("COVID confirm data",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("cinput", 
                                            label = "Choose a reigon",
                                            choices = regname,
                                            selected = 1
                                )
                            ),
                            # Show plots of the confirm data global
                            mainPanel(
                                plotOutput("confirmPlot")
                            )
                        )
                        
               ),
               #the covid death data page
               tabPanel("COVID death data",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("reg_d", 
                                            label = "Choose a reigon",
                                            choices = regname_d,
                                            selected = 1
                                )
                            ),
                            # Show plots of the death data global
                            mainPanel(
                                plotOutput("conviddeathPlot")
                            )
                        )
                        
               ),
               #the covid global recover page
               tabPanel("COVID recover data",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("reg_c", 
                                            label = "Choose a reigon",
                                            choices = regname_c,
                                            selected = 1
                                )
                            ),
                            # Show plots of the recover data global
                            mainPanel(
                                plotOutput("recoverPlot")
                            )
                        )
                        
               )
               
        )
    
    
)

# Define server logic required to draw plots
server <- function(input, output) {
    #draw the plots of global covid confirmed data
    output$confirmPlot<-renderPlot({
        #Match the entered region with the region in the table
        index<-match(input$cinput,regname)
        province<-as.character(world_confirmdata[index,1])
        country<-as.character(world_confirmdata[index,2])
        #Change data into time series data
        data_ts<-ts(t(world_confirmdata[index,580:length(confirm_colname)]))
        #draw the plot and forecast
        data_ts%>%forecast(h=10)%>%autoplot()+xlab("recent days")+ylab("confirmed")+ggtitle(country,province)+theme(text = element_text(family = "STHeiti"))+
            theme(plot.title = element_text(hjust = 0.5))
    })
    #draw the plots of global covid death data
    output$conviddeathPlot<-renderPlot({
        #Match the entered region with the region in the table
        index<-match(input$reg_d,regname_d)
        province<-as.character(world_coviddeath_data[index,1])
        country<-as.character(world_coviddeath_data[index,2])
        #Change data into time series data
        data_ts<-ts(t(world_coviddeath_data[index,580:length(coviddeath_colname)]))
        #draw the plot and forecast
        data_ts%>%forecast(h=10)%>%autoplot()+xlab("recent days")+ylab("death")+ggtitle(country,province)+theme(text = element_text(family = "STHeiti"))+
            theme(plot.title = element_text(hjust = 0.5))
    })
    #draw the plots of global covid recover data
    output$recoverPlot<-renderPlot({
        #Match the entered region with the region in the table
        index<-match(input$reg_c,regname_c)
        province<-as.character(world_recover_data[index,1])
        country<-as.character(world_recover_data[index,2])
        #Change data into time series data
        data_ts<-ts(t(world_recover_data[index,580:length(recover_colname)]))
        #draw the plot and forecast
        data_ts%>%forecast(h=10)%>%autoplot()+xlab("recent days")+ylab("recover")+ggtitle(country,province)+theme(text = element_text(family = "STHeiti"))+
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
