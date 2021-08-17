
library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(title = "Olympedia BMI version", skin = "black",
                    dashboardHeader(title = "Olympedia BMI version"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text = "History of Olympic", tabName = "page1", icon = icon("award")),
                        menuItem(text = "Body Mass Index", tabName = "page2", ),
                        menuItem(text = "BMI Calculator", tabName = "page3"),
                        menuItem(text = "Source Data", tabName = "page4")
                      )
                    ),
                     dashboardBody(
                      
                      #Page_1
                      
                      tabItems(
                        tabItem(tabName = "page1", 
                                fluidPage(
                                  h1(tags$b("History of Olympic 1896 - 2016")),
                                  div(style = "text-align:justify",
                                      p(h4("The Olympic Games are an international sports festival, held every four years. 
                                          The ultimate goals are to cultivate human beings, through sport, and contribute 
                                          to world peace. Summer Games and Winter Games are held separately. 
                                          The Olympic Games are an international sports festival, held every four years. 
                                          The ultimate goals are to cultivate human beings, through sport, and contribute to world peace. 
                                          Summer Games and Winter Games are held separately.
                                          Rio de Janeiro 2016 Olympic Games, athletic festival held in Rio de Janeiro that took place August 5–21,
                                           2016. The Rio Games were the 28th occurrence of the modern Olympic Games. 
                                           The event marked the first time that either the Summer or the Winter Olympics was held in South America.")),
                                      
                                      h3(tags$b("The Ancient Olympic Games")),
                                      div(style = "text-align:justify",
                                          p(h4("The history of the Olympics began some 2,300 years ago. 
                                                Their origin lays in the Olympian Games, which were held in the Olympia area of ancient Greece. 
                                                Although there are some theories on its initial purposes, 
                                                the Games have been said to have started as a festival of art and sport, to worship gods. 
                                                The ancient Olympic Games, however, ended in 393 because of the outbreaks of wars in 
                                                the region in which they were held.")),
                                          
                                          h3(tags$b("The Modern Olympic Games")),
                                          div(style = "text-align:justify",
                                              p(h4("After a 1,500 year absence of the ancient Olympic Games, the event was resumed in the late nineteenth century, 
                                              thanks to the efforts of Baron Pierre de Coubertin, a French educator. In 1894, his proposal to revive the Olympic 
                                              Games was unanimously approved at the International Congress in Paris, and the first Olympic Games were held in Athens, Greece, 
                                              two years later. He also devised the five-ring emblem that is familiar to most people as the Games’ symbol, which represents the 
                                              unity of the five continents.")),
                                              br()
                                          )
                                      )
                                  )
                                ),
                                fluidPage(
                                  box(width = 8,
                                         title = tags$b("Athletes Participant by Year"),
                                                  plotlyOutput("plot_1")
                                       ),
                                  box(solidHeader = T,
                                      width = 4,
                                      height = 465,
                                      backgroud = "green",
                                      radioButtons(inputId = "Select_Season",
                                                   label = "Season :",
                                                   choices = unique(olympic$Season),
                                                   selected = "Summer",
                                                   inline = T),
                                      pickerInput(inputId = "Select_region" ,
                                                  label = "Country :",
                                                  choices = as.character(unique(sort(olympic$region))),
                                                  options = list(`actions-box` = TRUE),
                                                  selected = "USA",
                                                  multiple = T, width = 487),
                                      pickerInput(inputId = "Select_Sport" ,
                                                  label = "Sport :",
                                                  choices = as.character(unique(sort(olympic$Sport))),
                                                  options = list(`actions-box` = TRUE),
                                                  selected = "Athletics",
                                                  multiple = T, width = 487),
                                      girafeOutput(outputId = "plot_4")
                                ),
                                fluidPage(
                                  box(title = tags$b("Highest Country Earning Medal by Year"),
                                      width = 12,
                                      height = 300,
                                      plotlyOutput("plot_1"))
                                )
                                )
                        ),
                        tabItem(tabName = "page2",
                                fluidPage(
                                  h1(tags$b("About Body Mass Index")),
                                  div(style = "text-align:justify",
                                      p(h4("Refer to care point health US, body mass index (BMI) is a guideline used by health professionals and 
                                                     bariatric surgeons to determine the overall health of an individual in terms of weight and body fat. 
                                                     BMI is calculated using a ratio of your height and weight, and can be affected by how often you exercise and 
                                                     by the types of foods you eat. Those who have above-average BMIs that fall into the overweight and obese 
                                                     categories can take steps to become healthier through diet, exercise, and weight-loss surgery so they can 
                                                     improve their overall quality of life and live longer with fewer health complications."))
                                  ),
                                  br(),
                                  div(tags$img(src ="Body Mass Index.png"), style="text-align: center"),
                                  br(),
                                  div(style = "text-align:justify",
                                      p(h4("In this publication will interprets about the effect of BMI in olympic athlete from historical data on the modern Olympic Games, 
                                                 from Athens 1896 to Rio 2016. Each row corresponds to an individual athlete competing in an individual event, including the athlete's 
                                                 name, sex, age, height, weight, country, and medal, and the event's name, sport, games, year, and city. The outputs will exemine the simple 
                                                 distribuiton from all parameters include BMI, categorize athlete by BMI chart shown above, and the correlation BMI with earning medals.",tags$b 
                                                 ("Lets gets started!")))
                                  )
                                ),
                                fluidPage(
                                  h3(tags$b("Body Mass Index Amongst Olympians")),
                                  br(),
                                  box(width = 12,column(width = 6,
                                      pickerInput(inputId = "Select_region" ,
                                                  label = "Country :",
                                                  choices = as.character(unique(sort(olympic$region))),
                                                  options = list(`actions-box` = TRUE),
                                                  selected = "USA",
                                                  multiple = T, width = 675)),
                                  column(width = 6,
                                      pickerInput(inputId = "Select_Sport" ,
                                                  label = "Sport :",
                                                  choices = as.character(unique(sort(olympic$Sport))),
                                                  options = list(`actions-box` = TRUE),
                                                  selected = "Athletics",
                                                  multiple = T, width = 675)),
                                      plotlyOutput(outputId = "plot_4"))
                                ),
                                fluidPage(
                                  h3(tags$b("Highest BMI by Country & Olympians")),
                                  br(),
                                  box(width = 12,
                                      plotlyOutput(outputId = "plot_4")
                                    
                                    
                                  )
                                  
                                  
                                )
                        ),
                      tabItem(tabName = "page3",
                              fluidPage(
                                box(width = 4, title = "BMI Calculator", status = "danger", solidHeader = T, 
                                  column(width = 12, title = "What's your BMI ?",
                                         numericInput(inputId = "Weight",
                                                      label = "Weigth (kg) :",
                                                      value = 60),
                                         numericInput(inputId = "Height",
                                                      label = "Heigth (cm) :",
                                                      value = 160),
                                         actionButton(inputId = "Calculate", label = "Submit"),
                                         br(),
                                         valueBoxOutput("value_4", width = 12))),
                                box(width = 12))
                              
                              )
                      
                    )
                    )
)

  
                                      
                                  
    