# For a whale or dolphin to reside under human care, it must have been captured from the ocean, born in captivity, or rescued. This dataset provides aggregate counts of whales and dolphins that joined the US captive animal population, along with their acquisition methods, between 1938 and May 7, 2017.

# Loading libraries
library(dplyr)
library(ggplot2)
library(ggtext)
library(tidyr)
library(readr)
library(cowplot)
library(png)
library(extrafont)

# dataviz years vs born, capture, rescue
data <- read_csv('acquisitions.csv') %>% select(-X1, -Total)
data_new <- data %>% gather(acquisition,count, 2:4)

# creating labels first
label_data2 <- data_new %>% group_by(AcqYear) %>% 
  summarise(sum=sum(count))

label_data2$id <- c(1:nrow(label_data2))

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data2)
angle <-  90 - 360 * ( label_data2$id-.5 )  /number_of_bar    

label_data2$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
label_data2$angle<-ifelse(angle < -90, angle+180, angle)

# find order of countries to use for scale_x, turn into factor
order_df <- data_new %>%
  select(AcqYear)  %>% 
  arrange(desc(AcqYear)) %>% 
  select(AcqYear)
order <- order_df$AcqYear
order <- c(order)

# Content to be put on the centre circle
# df <- data.frame(label = c("**Total cetaceans**<br> <span style = 'color:#2AAA8A'>**born**</span>,<br>
#           <span style = 'color:#e31c25'>**captured**,</span><br> **&**
#           <span style = 'color:#efefe8'>**rescued**</span><br>"),
#                  hjust = c(0.5),
#                  vjust = c(-0.5),
#                  angle = c(0))

# pal <- c("#efefe8" , "#e76a6a" , "#6dac4f")
pal <- c( "#2AAA8A" , "#e31c25" , "#b175ff")
# the order is alphabetical as per levels of acquisition
# born | capture | rescue

  # Viz
  rbz_plot <- data_new %>% 
    ggplot( aes(fill=as.factor(acquisition), y=count, x=AcqYear)) + 
    
    #add bars, order, color
    geom_bar(position="stack", stat="identity") +
    scale_x_discrete(limits=order)+
    scale_fill_manual(values=pal,guide=F)+
    
    # add ylim to keep empty circle in middle
    ylim(-150,250) + # first value decides the size of inner circle
    # second value should be greater than the max value of y-axis
    # remove axes and turn polar
    theme_void() +
    coord_polar(direction = 1,
                clip = "off") +
    
    # add country labels
    # control y to shift the circle
    geom_text(data=label_data2, aes(x=AcqYear, y=150, label=AcqYear,
                                    hjust=hjust), 
              color="#FFD700", fontface="bold",alpha=1,size=4,#golden yellow 
              angle= label_data2$angle, inherit.aes = FALSE )  +
    
    geom_text(data=label_data2, aes(x=AcqYear, y=100, label=sum,
                                    hjust=hjust), 
              color="#D8A7B1", fontface="bold",alpha=1, size=4, 
              angle= label_data2$angle, inherit.aes = FALSE )  +

  # add annotation in middle with colored words
  # geom_richtext(inherit.aes = F, data=df,
  #               aes(x= -Inf,y =-200,# this value can't exceed ylim first value
  #                   label=label,hjust=hjust),
  #               fill=NA, label.color=NA,size=5,
  #               family="Helvetica Neue",color="white")
    
  labs(title = '<br>For a whale or dolphin to reside under human care, it must have<br> been <b style="color:#e31c25;">captured</b> from the ocean, <b style="color:#2AAA8A;">born</b> in captivity, or <b style="color:#b175ff;">rescued</b> <br><br>
       This dataset provides <b style="color:#D8A7B1;">aggregate counts</b> of whales and dolphins<br> that joined the US captive animal population, along with their<br> acquisition methods, between <b style="color:#FFD700;">1938</b> and <b style="color:#FFD700;">2017</b>',
       
       caption = '**Data:** The Pudding article Free Willy and Flipper by the Numbers | **Visualisation:** Rahul Venugopal') +
    
    # adding a textbox for a quote
    
    
    #background fill
    theme(plot.title = ggtext::element_markdown(size=14,
                                                face = "bold",
                                                color = "white",
                                                family = "Noto Mono",
                                                lineheight = 1.4),
          plot.caption = ggtext::element_markdown(size = 12,
                                                  color = "white",
                                                  margin = margin(t = 15)),
          plot.title.position = "plot",
          plot.margin = margin(t = 25, r = 25, b = 10, l = 25),
          plot.background = element_rect(fill="#002e51"))
    
  # add quote via annotation
  ggdraw(rbz_plot) + 
    geom_text(
      data = data.frame(x = 1.1, y = 0,
                        label = "I’ve got to look out for Willy and I've got to do what’s best for him ~Free Willy"),
      aes(x, y, label = label),
      fontface = "italic",
      family = "Noto Mono",
      hjust = 1.14, vjust = -7, angle = 0, size = 4,
      # vjust negative values pushes textup
      # hjust larger values pushes towards left
      color = "white",
      inherit.aes = FALSE) + 

    draw_image("https://cdn.pixabay.com/photo/2014/04/03/10/11/dolphin-310084_960_720.png",
               x=1,y=1,
               hjust=3,vjust = 3.4,
               width = 0.2, height = 0.2)
  # larger values of vjust pulls image down and viceversa
  # larger values of hjust pulls image to left
  #save
  ggsave("cetacean.png",
         dpi=300,
         width=8,
         height=10,
         units="in",
         type = 'cairo') #hjust = 1.13 for pdf