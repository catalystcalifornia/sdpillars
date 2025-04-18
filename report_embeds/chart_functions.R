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
papaya <- "#F25922"
peridot <- "#CEEA01"
lavender <- "#CEC4E2"
meteorite <- "#3A207D"
ccblue <- "#0860BC"

black <- "#000000"
alabaster<-"#FBFBFB"
gainsboro <- "#DEDEDE"

divergent_color_ramp <- c("#372278", "#5F3B63","#A8683C", "#D58424", "#FF9900") 
grouped_color_ramp <-c(meteorite, papaya, peridot, "#BDAFE9", "#E2D9FF")
group_colors_item <- c(meteorite, papaya, peridot, lavender, ccblue, "#5F3B63","#A8683C", "#D58424", "#FF9900")
## FONTS ##
main_font <- "Inter"
regular_font_weight <- 400
black_font_weight <- 800
semi_bold_font_weight <- 600

##### highchart theme #####
cc_theme <- hc_theme(
  colors = c(meteorite, lavender, papaya, peridot
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
                 color = black,
                 fontFamily = main_font, # font_title
                 fontWeight = black_font_weight,
                 textAlign="left",
                 fontSize='21px'),
               align = "left"
  ),
  subtitle = list(
    style = list(
      color = black, 
      fontFamily = main_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='14px'),
    align='left'
  ),
  caption = list(
    style = list(
      color = black,
      fontFamily = main_font, # font_caption
      fontWeight = regular_font_weight,
      fontSize = "10px",
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
        fontSize="10px")),
    lineColor=gainsboro
  ),
  
  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        fontSize="10px",
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
      fontSize = '12px'
    ),
    
    itemHoverStyle = list(
      fontFamily = main_font, # font_table_text
      fontWeight = regular_font_weight,
      color = black
    ),
    plotLines=list(color=gainsboro)
  )
)

##### donut hc theme #####

cc_theme_donut <- hc_theme(
  colors = c(meteorite, lavender, papaya, peridot),
  chart = list(
    backgroundColor = alabaster,
    style = list(
      fontFamily = main_font, # font_subtitle
      color=alabaster
    ) 
  ),
  title = list(widthAdjust = -50,
               style = list(
                 color = black,
                 fontFamily = main_font, # font_title
                 fontWeight = black_font_weight,
                 textAlign="left",
                 fontSize='21px'),
               align = "left"
  ),
  subtitle = list(
    style = list(
      color = black, 
      fontFamily = main_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='14px'),
    align='left'
  ),
  caption = list(
    style = list(
      color = black,
      fontFamily = main_font, # font_caption
      fontWeight = regular_font_weight,
      fontSize = "10px",
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
        fontSize="10px")),
    lineColor=gainsboro
  ),
  
  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        fontSize="10px",
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
      fontSize = '12px'
    ),
    
    itemHoverStyle = list(
      fontFamily = main_font, # font_table_text
      fontWeight = regular_font_weight,
      color = black
    ),
    plotLines=list(color=gainsboro)
  )
)


##### Standard notes #####
sourcenote <- "Catalyst California's calculations based on City of San Diego's Police Stop Data (2022), catalystcalifornia.org, 2023."
racenote <- "Race/ethnicity: AIAN=American Indian or Alaska Native, NHPI=Native Hawaiian or Pacific Islander, SWANA/SA=Southwest Asian (Middle Eastern) or North African, or South Asian."

##### Bubblepop Chart - Combined Bar and Bubble Charts #####

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
  
  df <-  df %>%
    arrange(desc(y))
  
  highchart() %>%
    
    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text 
    
    hc_add_series(df, "bar", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y)), 
                  showInLegend=FALSE, 
                  enableMouseTracking=FALSE)%>% # disables tooltip from popping up when mouse moves over bars
    
    hc_add_series(df, "bubble", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), 
                        y=!!rlang::ensym(y), 
                        size=!!rlang::ensym(z)), 
                  maxSize="15%", 
                  # tooltip =  list(pointFormat = formatted_tooltip), 
                  showInLegend=FALSE,
                  clip=FALSE) %>%
    
    hc_xAxis(title = list(text = ""),
             type="category",
             categories=df$x,
             labels=list(style=list(fontSize="12px"))) %>%
    
    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS(yaxis_label_JS),
                           style=list(fontSize="12px")))  %>%
    
    hc_legend(title=list(text=paste0('<span style="color: #000000; font-weight: 400; font-size:10px;"><b>Line shows<br>    <i>the rate.</i></b><br><b>Bubble shows<br>    <i>total people.</i></b></span>')),
              enable = TRUE,
              align = "right", 
              verticalAlign="middle",
              width="12%",
              layout="vertical",
              borderColor=gainsboro,
              borderWidth=1,
              borderRadius=5,
              itemMarginBottom=10,
              bubbleLegend =  list(enabled = TRUE,
                                   connectorDistance=20,
                                   borderColor=meteorite,
                                   connectorColor=meteorite,
                                   labels = list(
                                     format="{value:,.0f}",
                                     style=list(fontSize='10px')),
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
      text = caption,
      useHTML=TRUE
    ) %>%
    
    hc_add_theme(cc_theme)%>%
    
    hc_chart(inverted = T,
             height = 480) %>%
    
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600,
      chartOptions=list(plotOptions=list(
        series=list(
          dataLabels=list(
            enabled=TRUE, 
            format=paste0(export_data_label))))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV')))
    )}



##### Stacked Bar Chart #####
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
         stacking = "normal",
         tooltip =  list(headerFormat='', pointFormat=tooltip_text)) %>%
    
    hc_title(
      text = top_finding) %>%
    
    hc_subtitle(
      text = subtitle) %>%
    
    hc_caption(
      text = caption) %>%
    
    hc_yAxis(title = list(text = paste0(""),
                          labels = list(style=list(fontSize="12px")))) %>%
    
    hc_xAxis(title = list(text = paste0(""),
                          labels=list(position="bottom",
                                      style=list(fontSize="12px")))) %>%
    
    hc_legend(enabled = TRUE, 
              reversed =  TRUE,
              x=20)%>% 
    
    hc_add_theme(cc_theme)%>%
    
    hc_colors(group_colors) %>%
    hc_chart(
      marginRight=120) %>%
    
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600, 
      chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE,format='{point.rate:.1f}%')))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV'))))
}


##### Item Chart #####
fx_itemchart <- function(
    df, # name of dataframe
    x, # category - will appear on y-axis
    y, # rate - will appear on x-axis
    group_colors, # vector of color values
    row_nums=0, # integer
    top_finding="",
    subtitle= "",
    tooltip_text="",
    legend_text="", # states what 1 dot represents
    caption = "") {

  
  highchart() %>%
    hc_add_series(df, 
                  "item",
                  rows=row_nums,
                  hcaes(name=!!rlang::ensym(x), 
                        y=!!rlang::ensym(y),
                        label=!!rlang::ensym(x)),
                  innerSize='96%',
                  showInLegend=TRUE)%>% # disables tooltip from popping up when mouse moves over bars
    hc_colors(group_colors) %>%
    hc_xAxis(labels=list(
      style=list(color=black,
                 fontFamily = main_font, # font_x_label
                 fontWeight = semi_bold_font_weight,
                 fontSize="12px")),
      lineColor=gainsboro) %>%
    hc_title(text=top_finding) %>%
    hc_subtitle(text=subtitle) %>%
    hc_caption(text=caption) %>%
    hc_add_theme(cc_theme)%>%
    hc_legend(title=list(text=paste0('<span style="color: #000000; font-weight: bold">', legend_text, '</span><br/><span style="color: #666; font-style: italic">Click to hide</span>')),
              enable = TRUE,
              labelFormat = paste0('{name} <span style="opacity: 0.4">{', y, ':.1f}</span>'))  %>%
    hc_tooltip(headerFormat="",
               pointFormat = tooltip_text) %>%
    hc_chart(
      marginRight=50,
      height = 480) %>%
    hc_exporting(enabled = TRUE, sourceWidth=900, sourceHeight=600,
               chartOptions=list(
                 plotOptions=list(
                   series=list(
                     dataLabels=list(enabled=TRUE,
                                     format=paste0(list(pointFormat=paste0('{point.', y,':.1f}'))))))),
                 filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
                 buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                              'downloadXLS', 'downloadCSV')))) 
}
  
