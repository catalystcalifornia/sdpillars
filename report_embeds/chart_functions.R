#### Interactive chart functions ####

#### Load libraries ####
library(tidyverse)
library(sf)
library(RPostgreSQL)
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

black <- "#000000"
alabaster<-"#FBFBFB"
gainsboro <- "#DEDEDE"

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
  plotOptions = 
    list(
      line = 
        list(
          marker = 
            list(
            )
        )
    ),
  title = list(widthAdjust = -50,
    style = list(
      color = meteorite,
      fontFamily = main_font, # font_title
      fontWeight = black_font_weight,
      textAlign="left",
      fontSize='21px'
    )
  ),
  subtitle = list(
    style = list(
      color = meteorite, 
      fontFamily = main_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='14px'
    )
  ),
  caption = list(
    style = list(
      color = meteorite,
      fontFamily = main_font, # font_caption
      fontWeight = regular_font_weight,
      textAlign = "left",
      fontSize="10px"
    ),
    useHTML = TRUE
  ),
  axis = list(
    style = list(
      color = gainsboro,
      fontFamily = main_font, # font_axis_label
      fontWeight = semi_bold_font_weight,
      fontSize='12px'
    )
  ),
  
  xAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_x_label
        fontWeight = semi_bold_font_weight,
        width=120,  #argument to modify the width of the labels,
        spacingLeft = "150px",
        fontSize="12px")),
    lineColor=gainsboro
  ),
  
  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        fontSize="12px",
        margin = 50)),
    gridLineWidth=0, # removes vertical grid lines
    visible=TRUE, # makes axis line visible
    lineWidth=1,
    lineColor=gainsboro
  ),
  
  
  legend = list(
    itemStyle = list(
      fontFamily = main_font, # font_axis_label
      fontWeight = regular_font_weight,
      color = black
    ),
    itemHoverStyle = list(
      fontFamily = main_font, # font_table_text
      fontWeight = regular_font_weight,
      color = black
    ),
    # tooltip=list(headerFormat=""),
    plotLines=list(color=gainsboro)
  )
)


#### Standard notes ####
sourcenote<-paste0("Catalyst California's calculations based on City of Long Beach's Police Stop Data (2019), catalystcalifornia.org, 2023.")
racenote<-paste0("Race/ethnicity: AIAN=American Indian or Alaska Native, NHPI=Native Hawaiian or Pacific Islander, SSWANA=South Asian, Southwest <br>Asian, or North African.")

#### Combined Bar and Bubble Chart - Bubblepop chart ####

fx_barbubblechart <- function(
    db,
    order_var,
    x,
    y,
    z,
    top_finding="",
    chart_title= "",
    tooltip_text="",
    chart_caption = "",
    yaxis_label = "''",
    export_data_label="") {
  
  yaxis_label_JS <- paste0("function() {
        	return this.value +", yaxis_label, "}")
  
  # add line breaks to tooltip_text
  tooltip_text <- sapply(strwrap(tooltip_text, 110, simplify=FALSE), paste, collapse="<br>" )
  
  db <-  db %>%
    arrange(desc(y))
  
  highchart() %>%
    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text 
    
    hc_add_series(db, "bar", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y)), 
                  showInLegend=FALSE, 
                  enableMouseTracking=FALSE)%>% # disables tooltip from popping up when mouse moves over bars
    hc_add_series(db, "bubble", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y), size=!!rlang::ensym(z)), 
                  maxSize="10%", tooltip =  list(pointFormat = tooltip_text), showInLegend=FALSE,
                  clip=FALSE) %>%
    
    hc_xAxis(title = list(text = ""),
             type="category",
             categories=db$x,
             min=0) %>%
    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS(yaxis_label_JS)),
             min=0,
             tickAmount=6,
             tickWidth=1)  %>%
    hc_legend(enabled = TRUE, 
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
                                   labels = list(format="{value:,.0f} persons"))) %>%
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
      align = "left",
      widthAdjust = -50,
      style = list(useHTML = TRUE)) %>%
    hc_subtitle(text = paste0(chart_title), 
                align="left") %>%
    hc_caption(
      text = paste0(chart_caption),
      margin=30,
      floating=FALSE,
      style=list(fontSize='8px')
    ) %>%
    hc_add_theme(cc_theme)%>%
    hc_chart(inverted = T) %>%
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600,
      chartOptions=list(plotOptions=list(
        series=list(
          dataLabels=list(
            enabled=TRUE, format=paste0(export_data_label))))),
      filename = paste0(chart_title,"_Catalyst California, catalystcalifornia.org, 2023.")
    )}