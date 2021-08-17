

server <- function(input, output) {
    
    output$plot_1 <- renderPlotly({
        
        olympic_raw$Year <- as.Date(ISOdate(olympic_raw$Year, 1, 1))
        
        olympic_time <- olympic_raw %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            group_by(Year) %>%
            summarise(Olympians = n_distinct(ID))  %>%
            
            ggplot(mapping = aes(x = Year , y = Olympians)) +
            geom_area(alpha=0.3 , size=.5, colour="#14b89f", fill="#18dec0") +
            geom_point(col = "#11a68f") +
            scale_x_date(date_labels = "%Y") +
            labs(y = "Participants", x ="Year") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"))
        
        olympic_time
        
    })
    
    output$value_1 <- renderValueBox({
        
        olympians_freq <- olympic_raw %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            group_by(ID) %>%
            summarise(Olympians = n_distinct(ID)) %>%
            count()
        
        valueBox(
            olympians_freq$n, "Olympians", icon = icon("user-friends"),
            color = "blue", width = 2)
        
        
    })
    
    output$value_2 <- renderValueBox({
        
        olympians_avg_BMI <- olympic %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            group_by(ID) %>%
            summarise(avg_BMI = mean(BMI))
        
        olympians_avg_BMI <- round(mean(olympians_avg_BMI$avg_BMI),2)
        
        olympians_avg_BMI_cat <- if (olympians_avg_BMI < 18.5){
            print("Underweight")
        } else if (olympians_avg_BMI <= 24.9){
            print("Normal")
        } else if (olympians_avg_BMI <= 29.9){
            print("Overweight")
        } else if (olympians_avg_BMI <= 39.9){
            print("Obese")
        } else {
            print("Morbidly Obese")
        }
        
        valueBox(
            olympians_avg_BMI, olympians_avg_BMI_cat, icon = icon("heart"),
            color = "red", width = 2)
        
        
    })
    
    output$value_3 <- renderValueBox({
        
        olympians_medal <- olympic %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            select(Bronze, Silver, Gold) %>%
            pivot_longer(cols = c(Bronze,Silver,Gold)) %>%
            group_by(name) %>%
            summarise(Freq = sum(value))
        
        olympians_total_medal <- sum(olympians_medal$Freq)
        
        valueBox(
            olympians_total_medal, "Medals", icon = icon("medal"),
            color = "yellow", width = 2)
        
        
        
    })
    
    
    output$plot_2 <- renderGirafe({
        
        olympians_gender <- olympic_raw %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            group_by(Sex) %>%
            summarise(Freq = n_distinct(ID))
        
        olympians_gender
        
        BMI_perc <- olympians_gender$Freq / sum(olympians_gender$Freq)
        
        # Compute the cumulative percentages (top of each rectangle)
        ymax <- cumsum(BMI_perc)
        
        # Compute the bottom of each rectangle
        ymin <- c(0, head(ymax, n=-1))
        
        # Compute label position
        labelPosition <- (ymax + ymin) / 2
        
        # Compute a good label
        labelData <- paste0(olympians_gender$Freq)
        
        # Make the plot
        olympians_gender <- ggplot(olympians_gender, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sex, tooltip = Freq)) +
            geom_rect_interactive() +
            #geom_label( x=3.8, aes(y=labelPosition, label=labelData), size=3) +
            scale_fill_brewer(palette= "Paired") +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            labs(fill = "Gender") +
            theme_void() +
            theme(legend.position = "bottom")
        
        girafe(ggobj = olympians_gender, 
               options = list(
                   opts_tooltip(offx = 30, offy = 30)) )
        
        girafe(ggobj = olympians_gender)
        
    })
    
    output$plot_3 <- renderGirafe({
        
        olympians_BMI_cat <- olympic %>%
            pivot_wider(
                names_from = BMI_category,
                values_from = BMI_category,
                values_fn = length,
                values_fill = 0)
        
        olympians_BMI_cat <- olympians_BMI_cat %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            select("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese") %>%
            pivot_longer(cols = c("Underweight"| "Normal" | "Overweight" | "Obese" | "Morbidly Obese")) %>%
            group_by(name) %>%
            summarise(Freq = sum(value))
        
        BMI_perc <- olympians_BMI_cat$Freq / sum(olympians_BMI_cat$Freq)
        
        # Compute the cumulative percentages (top of each rectangle)
        ymax <- cumsum(BMI_perc)
        
        # Compute the bottom of each rectangle
        ymin <- c(0, head(ymax, n=-1))
        
        # Compute label position
        labelPosition <- (ymax + ymin) / 2
        
        # Compute a good label
        labelData <- paste0(olympians_BMI_cat$Freq)
        
        # Make the plot
        olympians_BMI_cat <- ggplot(olympians_BMI_cat, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name, tooltip = Freq)) +
            geom_rect_interactive() +
            #geom_label(x=3.5, aes(y=labelPosition, label=labelData), size=4) +
            scale_fill_brewer(palette=8) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            labs(fill = "BMI Category") +
            theme_void() +
            theme(legend.position = "bottom")
        
        girafe(ggobj = olympians_BMI_cat, 
               options = list(
                   opts_tooltip(offx = 30, offy = 30)) )
        
        girafe(ggobj = olympians_BMI_cat)
        
    })
    
    output$plot_4 <- renderGirafe({
        
        olympians_medal <- olympic %>%
            filter(region %in% input$Select_region, Sport %in% input$Select_Sport) %>%
            select(Bronze, Silver, Gold) %>%
            pivot_longer(cols = c(Bronze,Silver,Gold)) %>%
            group_by(name) %>%
            summarise(Freq = sum(value))
        
        medal_perc <- olympians_medal$Freq / sum(olympians_medal$Freq)
        
        # Compute the cumulative percentages (top of each rectangle)
        ymax <- cumsum(medal_perc)
        
        # Compute the bottom of each rectangle
        ymin <- c(0, head(ymax, n=-1))
        
        # Compute label position
        labelPosition <- (ymax + ymin) / 2
        
        # Compute a good label
        labelData <- paste0(olympians_medal$Freq)
        
        # Make the plot
        olympians_medal <- ggplot(olympians_medal, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name, tooltip = Freq)) +
            geom_rect_interactive() +
            #geom_label(x=3.5, aes(y=labelPosition, label=labelData), size=4) +
            scale_fill_manual(values=c("#cd7f32",
                                       "#C0C0C0",
                                       "#FFD700")) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            labs(fill = "Medal") +
            theme_void() +
            theme(legend.position = "bottom")
        
        girafe(ggobj = olympians_medal, 
               options = list(
                   opts_tooltip(offx = 30, offy = 30)) )
        
        girafe(ggobj = olympians_medal)
        
    })
    
    observe({
        print(c(input$Select_region))
        
    })
    
    
    v <- reactiveValues(data = NULL)
    
    observeEvent(input$Calculate, {
        v$data <- c(input$Calculate)
        
        
    })     
    
    
    output$value_4 <- renderValueBox({
        
        #BMI_calculator <- round(input$Weight/(input$Height/100)^2,2)
        
        
        if (v$data == FALSE) return()
        
        isolate({
            data <- round(input$Weight/(input$Height/100)^2,2)
            BMI_category <- if (data < 18.5){
                print("Underweight")
            } else if (data <= 24.9){
                print("Normal")
            } else if (data <= 29.9){
                print("Overweight")
            } else if (data <= 39.9){
                print("Obese")
            } else {
                print("Morbidly Obese")
            }
            
            
            valueBox(
                data, BMI_category, icon = icon("heart"),
                color = "red")
            
        })
    })
    
    output$plot_5 <- renderPlotly({
        
        olympic_history_M <- olympic %>%
            filter(region %in% input$Select_region_p2, Sex == "M") %>%
            group_by(Year, ID) %>%
            summarise(BMI = mean(BMI))  %>%
            mutate(BMI_cat = case_when(BMI < 18.5 ~ "Underweight",
                                       BMI <= 24.9 ~ "Normal",
                                       BMI <= 29.9 ~ "Overweight",
                                       BMI <= 39.9 ~ "Obese",
                                       BMI >39.9 ~ "Morbidly Obese")) %>%
            
            ggplot(mapping = aes(y = BMI , x = as.factor(Year))) +
            geom_jitter(aes(col = BMI_cat), size = 1.5) +
            geom_boxplot(fill = "Gray", alpha = 0.3) +
            labs(title = "Male Olympians BMI by Year",
                 subtitle = "Male Athlete",
                 y = "BMI", x ="Year",
                 col = "BMI Category")
        
        olympic_history_M
        
        
    })
    
    output$plot_6 <- renderPlotly({
        
        olympic_history_F <- olympic %>%
            filter(region %in% input$Select_region_p2, Sex == "F") %>%
            group_by(Year, ID) %>%
            summarise(BMI = mean(BMI))  %>%
            mutate(BMI_cat = case_when(BMI < 18.5 ~ "Underweight",
                                       BMI <= 24.9 ~ "Normal",
                                       BMI <= 29.9 ~ "Overweight",
                                       BMI <= 39.9 ~ "Obese",
                                       BMI >39.9 ~ "Morbidly Obese")) %>%
            
            ggplot(mapping = aes(y = BMI , x = as.factor(Year))) +
            geom_jitter(aes(col = BMI_cat), size = 1.5) +
            geom_boxplot(fill = "Gray", alpha = 0.3) +
            labs(title = "Female Olympians BMI by Year",
                 subtitle = "Male Athlete",
                 y = "BMI", x ="Year",
                 col = "BMI Category")
        
        olympic_history_F
        
        
    })
    
    output$plot_7 <- renderPlotly({
        
        olympic_high_country <- olympic %>%
            group_by(region) %>% 
            summarise(avg_BMI = mean(BMI)) %>%
            mutate(BMI_cat_country = case_when(avg_BMI < 18.5 ~ "Underweight",
                                               avg_BMI <= 24.9 ~ "Normal",
                                               avg_BMI <= 29.9 ~ "Overweight",
                                               avg_BMI <= 39.9 ~ "Obese",
                                               avg_BMI >39.9 ~ "Morbidly Obese")) %>%
            arrange(-avg_BMI) %>%
            head(30) %>%
            ggplot(aes(y= reorder(region,avg_BMI), x = avg_BMI)) +
            geom_col(aes(fill = BMI_cat_country)) +
            scale_fill_brewer(palette= 14) +
            labs(x = "Country", y = "BMI") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"))
        
        olympic_high_country
        
        
    })
    
    output$plot_8 <- renderPlotly({
        
        olympic_gender <- olympic_raw %>%
            group_by(Year, Sex) %>%
            summarise(Olympians = n_distinct(ID))  %>%
            pivot_wider(
                names_from = Sex,
                values_from = Olympians,
                values_fill = 0) %>%
            mutate(total_olympians = M + F) %>%
            pivot_longer(cols =c(M,F,total_olympians))
        
        
        olympic_gender <- ggplot(olympic_gender, mapping = aes(x = Year , y = value, col = name)) +
            geom_line() +
            geom_point() +
            labs(y = "Participants", x ="Year", col = "Athlete Gender" ) +
            scale_color_brewer(palette= "Blues") +
            theme(legend.position = "bottom") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white")) 
        
        olympic_gender
        
        
    })
    
    output$plot_9 <- renderPlotly({
        
        athlete_BMI <-olympic %>%
            group_by(ID, Name) %>% 
            summarise(avg_BMI = mean(BMI)) %>%
            mutate(BMI_cat_country = case_when(avg_BMI < 18.5 ~ "Underweight",
                                               avg_BMI <= 24.9 ~ "Normal",
                                               avg_BMI <= 29.9 ~ "Overweight",
                                               avg_BMI <= 39.9 ~ "Obese",
                                               avg_BMI >39.9 ~ "Morbidly Obese")) %>%
            
            arrange(-avg_BMI) %>%
            head(30) %>%
            ggplot(aes(y= reorder(Name,avg_BMI), x = avg_BMI)) +
            geom_col(aes(fill = BMI_cat_country)) +
            scale_fill_brewer(palette= 14) +
            labs(x = "Athlete", y = "BMI") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"))
        
        athlete_BMI
        
        
    })
    
    
    
    
}