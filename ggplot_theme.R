### create custom ggplot theme

library(showtext)
font_add_google(name = "Inter", family = "inter")

# turn on showtext
showtext_auto()



theme_woodland <- function(){ 
  font <- "inter"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major.y = element_line(linetype = 'dashed'),    #strip major gridlines
      panel.grid.major.x = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        hjust = 0.5,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14,                #font size
        vjust = -0.5),            # lower slightly   
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 11,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 12),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 12),                #font size
      
      axis.text.x = element_text( 
        hjust = 0.5, # center text 
        margin=margin(5, b = 11)), #margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5
      ),
      
      aspect.ratio = 1 #make plot square
      
    )
}


windows() # run this line of code to view your graph with the proper fonts

test_plot = ggplot(woodland_drug_crime) +
                  geom_histogram(aes(x = year), alpha = 0.5, binwidth = 0.5) +
                  labs(title = 'Drug Crimes on Woodland Ave',
                       subtitle = '2018 through 2022',) +
                  theme_woodland() +
                  theme(axis.title = element_blank())

test_plot

# this chunk creates a variable for today's date and formats it properly for the file name
today = lubridate::ymd(Sys.Date()) |>
          str_replace_all("-", '_')

# this line saves the plot as a pdf with the proper formatting based on today's date
ggsave(paste0(today, '_test_plot.pdf'), test_plot, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf')




