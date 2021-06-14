library(shiny)
library(shinydashboard)
library(flexdashboard)
library(lubridate) # manipulasi tanggal
library(ggthemes) # tema tambahan ggplot
library(tidyverse) # praproses data
library(plotly)  # membuat visualisasi  interaktif
library(scales) # mengatur tampilan skala pada plot
library(glue) # mengatur tooltip di plotly
library(tidyquant) # mengambil data saham
library(rsconnect)
library(dplyr)
library(dtplyr)
library(DT)

#DATA DASHBOARD
bank <- read.csv("BankChurners.csv")

#SET VARIABLE AS FACTOR
bank1 <- bank %>% 
    mutate_at(vars(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category), as.factor)


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Credit Card"),
                    dashboardSidebar(collapsed = F,
                                     sidebarMenu(
                                         menuItem(#TAB 1
                                             text = "Customer Overview",
                                             tabName = "Overview",
                                             icon = icon("user", lib = "glyphicon")
                                         ),
                                         menuItem(#TAB 2
                                             text = "Analytics Review",
                                             tabName = "Report",
                                             icon = icon("stats", lib = "glyphicon")
                                         ),
                                         menuItem(#TAB 3
                                             text = "Data",
                                             tabName = "Data",
                                             icon = icon("credit-card", lib = "glyphicon")
                                         ),
                                         menuItem("Source Code", icon = icon("file", lib = "glyphicon"), #TAB 4
                                                  href = "https://github.com/NabiilahArdini/Eco-Status") #nanti masukin link github
                                     )
                    ),
                    dashboardBody(
                        
                        tabItems( 
                            tabItem(# TAB 1 
                                tabName = "Overview",
                                fluidPage(width = 6,
                                          h2(tags$b("Credit Card")),
                                          br(),
                                          div(style = "text-align:justify", 
                                              p("Credit Card is a card issued by a financial institution, typically a bank, 
                      and it enables the cardholder to borrow funds from that institution", 
                                                "Cardholders agree to pay the money back with interest, 
                      according to the institution's terms.",
                                                "Bank ARDI has over 10,000 credit card customers, with detail as follows:"),
                                              br()),
                                          
                                          
                                          box(
                                              radioButtons(inputId = "cust", #harus unik 
                                                           label = "Attrition Flag", 
                                                           choices = levels(factor(bank$Attrition_Flag)),
                                                           selected = "Existing Customer"), width = 4, background = "light-blue"), 
                                          box(plotlyOutput("plot1"), width = 8),
                                          
                                          box(
                                              selectInput(inputId = "jenis_kartu", #harus unik 
                                                          label = "Card Type", 
                                                          choices = levels(factor(bank$Card_Category)),
                                                          selected = "Blue"), width = 4, background = "light-blue"), 
                                          box(plotlyOutput("plot2"), width = 8),
                                          
                                ),#FluidPage
                                fluidPage(tabBox(width = 12,
                                                 title = tags$b("Customer Profile"),
                                                 id = "cust1",
                                                 side = "right",
                                                 tabPanel(tags$b("Education Level"), 
                                                          plotlyOutput("plot3")
                                                 ),
                                                 tabPanel(tags$b("Income Category"), 
                                                          plotlyOutput("plot4"))))
                            ), #TUTUP TAB 1
                            
                            tabItem( #BUKA TAB 2=
                                tabName = "Report", 
                                fluidPage(width = 6,
                                          h2(tags$b("Analytics Review")),
                                          br(),
                                          div(style = "text-align:justify", 
                                              p("This analytics review is provided to fulfill decision making needs"),
                                              br()),
                                          fluidRow(     
                                              infoBox("Total Customer", comma(nrow(bank)), icon = icon("user", lib = "glyphicon"), 
                                                      width = 3, fill = T, color = "yellow"),
                                              infoBox("Total Outstanding", dollar(sum(bank$Total_Revolving_Bal)), icon = icon("usd"), 
                                                      width = 3, fill = T, color = "blue"),
                                              infoBox("Avg. Utilization Ratio", percent(mean(bank$Avg_Utilization_Ratio)), icon = icon("credit-card"), 
                                                      width = 3, fill = T, color = "maroon"),
                                          ),
                                          
                                          fluidRow(
                                              box(
                                                  selectInput(inputId = "jenis_kartu2", #harus unik 
                                                              label = "Card Type", 
                                                              choices = levels(factor(bank$Card_Category)),
                                                              selected = "Blue"), width = 3, background = "olive"), 
                                              box(plotlyOutput("plot5"), width = 9),
                                          ),
                                          
                                          
                                          fluidRow(
                                              column(width = 9, style = "position-align:: left", 
                                                     box(width = NULL, background = "black", 
                                                         "Description [Scatter Plot]: The graph shows a tendency of negative relationship between 
                Credit Limit and Utilization Ratio"))
                                              
                                          ),#fluidRow
                                          
                                          fluidRow(
                                              box(plotlyOutput("plot6"), width = 12))
                                          
                                )#TUTUP fluidPage
                            ),#TUTUP TabItem 2
                            
                            tabItem(
                                tabName = "Data",
                                h2(tags$b("Database Customer Credit Card Bank ARDI")),
                                DT::dataTableOutput("table1")
                            )
                            
                            
                                
                        ) #tutup kurung TAB ITEMS 
                    ) #dashboard body
) #tutup kurung dashboard page

server <- function(input, output) {
    output$plot1 <- renderPlotly({
        bank1 <- bank %>% 
            select(Attrition_Flag, Marital_Status, Card_Category) %>% 
            filter(Attrition_Flag == input$cust, Card_Category == input$jenis_kartu) %>% 
            group_by(Attrition_Flag, Marital_Status, Card_Category) %>% 
            summarise(freq = n()) %>% 
            mutate(pos = cumsum(freq) - (0.5 * freq)) %>% 
            mutate(pct = freq / sum(freq), 
                   pctlabel = paste0(round(pct*100), "%"))
        
        plot_ly(bank1, labels = ~Marital_Status, values = ~freq, type = "pie",
                marker = list(line = list(color = "white", width = 2.5)),
                showlegend = T) %>% 
            layout(title = 'Demografi Berdasarkan Status Pernikahan',
                   xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                   yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
        
    }) 
    output$plot2 <- renderPlotly({
        bank2 <- bank %>% 
            select(Attrition_Flag, Customer_Age, Card_Category) %>%
            filter(Attrition_Flag == input$cust, Card_Category == input$jenis_kartu) %>%  
            group_by(Attrition_Flag, Customer_Age, Card_Category) %>% 
            summarise(freq = n()) %>% 
            mutate(pct = freq / sum(freq), 
                   pctlabel = paste0(round(pct*100), "%"))
        
        plot_status_2 <- ggplot(bank2, aes(x = Customer_Age, 
                                           y = freq,
                                           text = glue("Usia : {Customer_Age}
                                               Jumlah : {freq}"))) +
            geom_bar(stat = "identity", fill = "aquamarine4") +
            labs(title = "Demografi Berdasarkan Usia",
                 x = "Usia Customer",
                 y = "Frekuensi") +
            theme_light()
        
        ggplotly(plot_status_2, tooltip = "text") %>%
            config(displayModeBar = F)
        
    })
    output$plot3 <- renderPlotly({
        bank3 <- bank %>% 
            select(Attrition_Flag, Education_Level, Card_Category) %>%
            filter(Attrition_Flag == input$cust, 
                   Card_Category == input$jenis_kartu) %>%  
            group_by(Attrition_Flag, Education_Level, Card_Category) %>% 
            summarise(freq = n())
        
        plot_status_3 <- ggplot(bank3, aes(y = reorder(Education_Level, freq), 
                                           x = freq,
                                           text = glue("Pendidikan : {Education_Level}
                                               Jumlah : {comma(freq)}"),
                                           fill = Education_Level)) +
            geom_col() + 
            scale_x_continuous(label = comma_format()) +
            labs(title = "Profile Customer berdasarkan Education Level",
                 x = "Frekuensi",
                 y = NULL) +
            theme_minimal() +
            theme(plot.title = element_text(size = 9),
                  legend.title = element_blank()) #menghapus judul legend
        
        ggplotly(plot_status_3, tooltip = "text") %>%
            config(displayModeBar = F)
        
    })
    
    output$plot4 <- renderPlotly({
        bank4 <- bank %>% 
            select(Attrition_Flag, Income_Category) %>%
            filter(Attrition_Flag == input$cust) %>% 
            group_by(Attrition_Flag, Income_Category) %>% 
            summarise(freq = n()) %>% 
            mutate(a = c(5,2,3,4,1,6)) 
        
        plot_status_4 <- ggplot(bank4, aes(x = reorder(Income_Category, a), y = freq, text = glue("Income : {Income_Category} 
                                                                                          Jumlah : {comma(freq)}"),
                                           fill = Income_Category)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("cadetblue3", "cadetblue3", "cadetblue3", "cadetblue3", "cadetblue3", "cadetblue4")) +
            #geom_text(aes(label = freq), position = position_stack(vjust = 0.7)) +
            labs(title = "Profile Customer berdasarkan Income",
                 x = "Frekuensi",
                 y = NULL) +
            scale_y_continuous(label = comma_format()) +
            theme_light() +
            theme(plot.title = element_text(size = 10),
                  legend.title = element_blank(), 
                  legend.position = "none")
        
        ggplotly(plot_status_4, tooltip = "text") %>%
            config(displayModeBar = F)
        
    })
    
    output$plot5 <- renderPlotly({
        bank5 <- bank %>%
            select(Credit_Limit, Avg_Utilization_Ratio, Card_Category) %>%
            filter(Card_Category == input$jenis_kartu2) %>%
            group_by(Card_Category)
        
        plot2_a <- ggplot(bank5, aes(x= Credit_Limit, y= Avg_Utilization_Ratio, 
                                     text = glue("Credit Limit : {comma(Credit_Limit)} 
                                        Utilization : {Avg_Utilization_Ratio}"))) +
            geom_point(position = "identity", col = "darkslategray3") +
            labs(title = "Hubungan Credit Limit dengan Utilization Ratio",
                 x= "Credit Limit",
                 y= "Avg Utilization Ratio") +
            scale_x_continuous(labels = dollar_format()) +
            scale_y_continuous(labels = percent_format()) +
            theme_tq_dark()
        
        ggplotly(plot2_a, tooltip = "text") %>%
            config(displayModeBar = F)
    }) 
    
    output$plot6 <- renderPlotly({
        bank6 <- bank %>%
            select(Attrition_Flag, Total_Revolving_Bal)
        plot2_b <- ggplot(data = bank6, aes(x = Total_Revolving_Bal, fill = Attrition_Flag)) +
            geom_density(alpha = 0.8) +
            scale_x_continuous(labels = dollar_format()) +
            labs(x = NULL,
                 y= NULL, 
                 title = "Distribusi Credit Card Outstanding Balance") +
            scale_fill_brewer(palette = "Set1") +
            theme_gray() +
            theme(legend.title = element_blank())
        
        ggplotly(plot2_b, tooltip = "text") %>% 
            config(displayModeBar = F)
    })
    
    output$table1 <- renderDataTable({
        bank7 <- bank %>% 
            select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, 
                   -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2)
        datatable(data = bank7, options =  list(scrollX = TRUE, scrollY = "600px"))
    })
    
    
    
} #kurawal tutup server

shinyApp(ui, server)


