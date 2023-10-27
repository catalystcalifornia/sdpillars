#### Interactive chart functions ####

#### Load libraries ####
library(dplyr)
library(highcharter)
library(extrafont)
library(showtext)

options(scipen=999)

#### Set themes and options ####
# set global options to ensure that comma separator is a comma for highchart graphs
lang <- getOption("highcharter.lang")
lang$thousandsSep <- ","
options(highcharter.lang = lang)

## COLORS ##
orange <- "#F25922"
peridot <- "#CEEA01"
lavender <- "#CEC4E2"
meteorite <- "#3A207D"
ccblue <- "#0860BC"

black <- "#000000"
alabaster<-"#FBFBFB"
gainsboro <- "#DEDEDE"

divergent_color_ramp <- c("#372278", "#5F3B63","#A8683C", "#D58424", "#FF9900")
grouped_color_ramp <-c(meteorite, orange, peridot, "#BDAFE9", "#E2D9FF")

## FONTS ##
main_font <- "Inter"
regular_font_weight <- 400
black_font_weight <- 800
semi_bold_font_weight <- 600

# highchart theme 
cc_theme <- hc_theme(
  colors = c(meteorite, lavender, orange, peridot
  ),
  chart = list(
    backgroundColor = alabaster,
    style = list(
      fontFamily = main_font, # font_subtitle
      color=alabaster
    ) 
  ),
  title = list(widthAdjust = -50,
               style = list(
                 color = meteorite,
                 fontFamily = main_font, # font_title
                 fontWeight = black_font_weight,
                 textAlign="left",
                 fontSize='3vmin'),
               align = "left"
  ),
  subtitle = list(
    style = list(
      color = meteorite, 
      fontFamily = main_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='2vmin'),
    align='left'
  ),
  caption = list(
    style = list(
      color = meteorite,
      fontFamily = main_font, # font_caption
      fontWeight = regular_font_weight,
      fontSize = "1.25vmin",
      textAlign = "left",
      width = 50),
    useHTML = TRUE,
    floating = FALSE
  ),
  
  xAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_x_label
        fontWeight = semi_bold_font_weight,
        width=120,  #argument to modify the width of the labels
        min=0,
        # spacingLeft = "150px",
        fontSize="1.5vmin")),
    lineColor=gainsboro
  ),
  
  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        fontSize="1.5vmin",
        margin = 50)),
    gridLineWidth=0, # removes vertical grid lines
    visible=TRUE, # makes axis line visible
    lineWidth=1,
    lineColor=gainsboro,
    min=0,
    tickAmount=6,
    tickWidth=1
  ),
  
  legend = list(
    itemStyle = list(
      fontFamily = main_font, # font_axis_label
      fontWeight = regular_font_weight,
      color = black,
      fontSize = '1.5vmin'
    ),
    
    itemHoverStyle = list(
      fontFamily = main_font, # font_table_text
      fontWeight = regular_font_weight,
      color = black
    ),
    plotLines=list(color=gainsboro)
  )
)


#### Standard notes ####
sourcenote<-paste0("Catalyst California's calculations based on City of San Diego's Police Stop Data (2022), catalystcalifornia.org, 2023.")
racenote<-paste0("Race/ethnicity: AIAN=American Indian or Alaska Native, NHPI=Native Hawaiian or Pacific Islander, SSWANA=South Asian, Southwest <br>Asian, or North African.")

#### Bubblepop Chart - Combined Bar and Bubble Charts ####

fx_bubblepopchart <- function(
    df, # name of dataframe
    x, # x or independent variables - will appear on y-axis
    y, # y or dependent variable - will appear on x-axis
    z, # variable storing count - will determine bubble size
    top_finding="",
    subtitle= "",
    tooltip_text="",
    caption = "",
    yaxis_label = "''",
    export_data_label="") {
  
  yaxis_label_JS <- paste0("function() {
        	return this.value +", yaxis_label, "}")
  
  # add line breaks to tooltip_text
  tooltip_text <- sapply(strwrap(tooltip_text, 110, simplify=FALSE), paste, collapse="<br>" )
  
  df <-  df %>%
    arrange(desc(y))
  
  highchart() %>%
    
    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text 
    
    hc_add_series(df, "bar", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y)), 
                  showInLegend=FALSE, 
                  enableMouseTracking=FALSE)%>% # disables tooltip from popping up when mouse moves over bars
    
    hc_add_series(df, "bubble", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y), size=!!rlang::ensym(z)), 
                  maxSize="15%", tooltip =  list(pointFormat = tooltip_text), showInLegend=FALSE,
                  clip=FALSE) %>%
    
    hc_xAxis(title = list(text = ""),
             type="category",
             categories=df$x) %>%
    
    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS(yaxis_label_JS))
    )  %>%
    
    hc_legend(enabled = TRUE, 
              width = '15%',
              align = "right", 
              verticalAlign="bottom",
              y=10,
              layout="vertical",
              floating=TRUE,
              borderColor=gainsboro,
              borderWidth=1,
              borderRadius=5,
              itemWidth=120,
              padding=4,
              itemMarginBottom=20,
              bubbleLegend =  list(enabled = TRUE,
                                   connectorDistance=20,
                                   borderColor=meteorite,
                                   connectorColor=meteorite,
                                   labels = list(
                                     format="{value:,.0f} persons",
                                     style=list(fontSize='1.25vmin')),
                                   marker = list(
                                     fillColor = lavender))) %>%
    
    # Sets bar width 
    hc_plotOptions(series=list(pointWidth=2,
                               marker=list(fillColor=lavender, 
                                           lineColor=meteorite,
                                           fillOpacity=1),
                               states=list(inactive=list(opacity=1))), # disables transparency of bars when hovering over bubbles
                   spacingLeft=200) %>%
    
    # title elements
    hc_title(
      text = paste0(top_finding),
      style = list(useHTML = TRUE)) %>%
    
    hc_subtitle(text = paste0(subtitle)) %>%
    
    hc_caption(
      text = paste0(caption),
      margin=30
    ) %>%
    
    hc_add_theme(cc_theme)%>%
    
    hc_chart(inverted = T) %>%
    
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600,
      chartOptions=list(plotOptions=list(
        series=list(
          dataLabels=list(
            enabled=TRUE, format=paste0(export_data_label))))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023.")
    )}



#### Stacked Bar Chart ####
fx_stackedbarchart <- function(
    df,
    x, # independent variable
    y, # dependent variable
    group_var, # variable to group by
    group_colors, # vector for colors to use
    top_finding, # chart title
    subtitle,
    tooltip_text,
    caption) {
  
  hchart(df, 
         "bar", hcaes(x = !!rlang::ensym(x), y = !!rlang::ensym(y), group = !!rlang::ensym(group_var)),
         stacking = "percent",
         tooltip =  list(headerFormat='',pointFormat=tooltip_text)) %>%
    
    hc_title(
      text = top_finding) %>%
    
    hc_subtitle(
      text = subtitle) %>%
    
    hc_caption(
      text = caption) %>%
    
    hc_yAxis(title = list(text = paste0(""))) %>%
    
    hc_xAxis(title = list(text = paste0(""),
                          labels=list(position="bottom"))) %>%
    
    hc_legend(enabled = TRUE, 
              reversed =  TRUE,
              x=50)%>% 
    
    hc_add_theme(cc_theme)%>%
    
    hc_colors(group_colors) %>%
    
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600, 
      chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE,format='{point.rate:.1f}%')))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV'))))
}

#### test stacked bar chart ####
library(RPostgreSQL)
source("W:\\RDA Team\\R\\credentials_source.R")
pillars_conn <- connect_to_db("rjs_pillars")

stoprates_age_race_ficard_person <- dbGetQuery(pillars_conn, "SELECT * FROM data.report_stoprates_age_race_ficard_person")

race_levels<-c("latinx","nh_asian","nh_black","nh_nhpi","nh_white", "aian", "nhpi", "sswana", "nh_twoormor")
race_labels<-c("latinx","nh_asian","nh_black","nh_nhpi","nh_white", "aian", "nhpi", "sswana", "nh_twoormor")
stoprates_age_race_ficard_person$race<-factor(stoprates_age_race_ficard_person$race, ordered=TRUE, 
                                               levels=race_levels, labels=race_labels)

tooltip_text <- "Across all field interviews of folks <b> ages {point.age_bracket:.1f} </b>, SDPD stopped <b>{point.race:.1f} people {point.rate:.1f}% of the time.</b>"

fx_stackedbarchart(  
  df = stoprates_age_race_ficard_person,
  x = 'age_bracket',
  y = 'rate',
  group_var = 'race',
  group_colors = c(meteorite, lavender, orange, peridot, ccblue, gainsboro, "#211447",
                         "#FF9E0D", "#A8683C"),
  top_finding = 'A top level finding about systemic impact',
  subtitle = 'stop rates for field interviews for every race across age brackets',
  tooltip = tooltip_text,
  caption = paste0(racenote,"<br><br>", sourcenote, "<br>","Analysis for all officer-initiated stops.","<br>"))

#### Item Chart ####
fx_itemchart <- function(
    df, # name of dataframe
    x, # category - will appear on y-axis
    y, # rate - will appear on x-axis
    group_colors, # vector of color values
    row_nums, # integer
    top_finding="",
    subtitle= "",
    tooltip_text="",
    legend_text="", # states what 1 dot represents
    caption = "") {

  
  # add line breaks to tooltip_text
  tooltip_text <- sapply(strwrap(tooltip_text, 110, simplify=FALSE), paste, collapse="<br>" )
  
  highchart() %>%
    hc_add_series(df, 
                  "item",
                  rows=row_nums,
                  hcaes(name=!!rlang::ensym(x), 
                        y=!!rlang::ensym(y),
                        label=!!rlang::ensym(x)),
                  name="I DON'T KNOW WHAT THIS IS",
                  showInLegend=TRUE)%>% # disables tooltip from popping up when mouse moves over bars
    hc_colors(group_colors) %>%
    hc_title(text=top_finding) %>%
    hc_subtitle(text=subtitle) %>%
    hc_caption(text=caption) %>%
    hc_add_theme(cc_theme)%>%
    hc_legend(title=list(text=paste0('<span style="color: #000000; font-weight: bold">', legend_text, '</span><br/><span style="color: #666; font-style: italic">Click to hide</span>')),
              enable = TRUE,
              labelFormat = paste0('{name} <span style="opacity: 0.4">{', y, ':.1f}</span>'))  %>%
    hc_tooltip(headerFormat="",
               pointFormat = tooltip_text) %>%
    hc_exporting(enabled = TRUE, sourceWidth=900, sourceHeight=600,
               chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE,                                                           
                                                                              format=paste0(list(pointFormat=paste0('{point.', y,':.1f}')))))),
               filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."))) %>%
    hc_chart(
      marginRight=100,
      marginLeft=50)
}
  

#### testing item chart ####  
pillars_conn <- connect_to_db("rjs_pillars")
timespent_ficard_race_person <- dbGetQuery(pillars_conn, "SELECT * FROM data.report_timespent_ficard_race_person") %>%
  filter(race != "overall")

fx_itemchart(
    df=timespent_ficard_race_person, 
    x="race",
    y="duration_rate", 
    group_colors=c(meteorite, lavender, orange, peridot, ccblue, gainsboro, "#211447",
    "#FF9E0D", "#A8683C"),
    row_nums=7, # integer
    top_finding="The top finding related to systemic factors",
    subtitle= "The number of hours SDPD spent conducting field interviews by race",
    tooltip_text="Out of 100 hours, LBPD spent <b>{point.duration_rate:.1f}</b> hours conducting field interviews on <b>{point.race} people</b>",
    legend_text="Single dot represents 1 hour out of 100 hours",
    caption = paste0(sourcenote," Analysis for all officer-initiated stops RESULTING IN FIELD INTERVIEWS ONLY."))    

  dbDisconnect(pillars_conn)
  




  
