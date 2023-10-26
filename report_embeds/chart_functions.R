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

black <- "#000000"
alabaster<-"#FBFBFB"
gainsboro <- "#DEDEDE"

divergent_color_ramp <- c("#372278", "#5F3B63","#A8683C", "#D58424", "#FF9900")

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
                 fontSize='3vw'
               )
  ),
  subtitle = list(
    style = list(
      color = meteorite, 
      fontFamily = main_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='2vw'
    )
  ),
  caption = list(
    style = list(
      color = meteorite,
      fontFamily = main_font, # font_caption
      fontWeight = regular_font_weight,
      fontSize = "1.25vw",
      textAlign = "left",
      width = 50
    ),
    useHTML = TRUE
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
        fontSize="1.5vw")),
    lineColor=gainsboro
  ),
  
  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        fontSize="1.5vw",
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
      fontSize = '1vw'
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
                                     style=list(fontSize='1.25vw')),
                                   marker = list(
                                     fillColor = lavender
                                   )
              )) %>%
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
    
    hc_subtitle(text = paste0(subtitle), 
                align="left") %>%
    
    hc_caption(
      text = paste0(caption),
      margin=30,
      floating=FALSE
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

#### Testing bubblepop ####
library(RPostgreSQL)
source("W:\\RDA Team\\R\\credentials_source.R")
pillars_conn <- connect_to_db("rjs_pillars")

## Load in RJS Pillars data: Bubble Pop test ##
report_stoprates_race_person <- dbGetQuery(pillars_conn, "SELECT * FROM data.report_gang_stoprates_race_person")  %>% filter(race!="total" & !is.na(stop_count))

fx_bubblepopchart(
  df=report_stoprates_race_person, 
  x='race', 
  y='stop_per1k_rate', 
  z='stop_count',
  top_finding=paste0("TOP LEVEL FINDING ABOUT SYSTEMIC IMPACT."), # this is the top finding from the chart that gets used like a title
  subtitle= paste0("Number of Officer-initiated, Gang-related Stops per 1K of Same Population by Race and Ethnicity"), # this is the more standard chart title that gets used as a subtitle
  tooltip_text="For every 1K <b>{point.race_label_long:.1f}</b> people in San Diego, SDPD stopped <b>{point.stop_rate_per1k:.1f}</b> people perceived as <b>{point.race_label_long:.1f}</b>, a total of <b>{point.stop_count:,0f}</b> people.", # customize your tooltip as a statement, calling out rate and count
  caption = paste0(racenote,"<br><br>", sourcenote, "<br>","Analysis for all officer-initiated stops.","<br>"), # if necessary add a method note between the racenote and sourcenote with a line break, use racenote if graph includes data by race, and always use sourcenote. Each of these are standard objects across all charts
  yaxis_label = "''", #format for your y axis labels
  export_data_label=list(pointFormat='{point.stop_rate_per1k:.1f} per 1K')
)

#### Stacked Bar Chart ####
fx_stack <- function(
    df,
    x, # independent variable
    y, # dependent variable
    group_var, # variable to group by
    top_finding, # chart title
    subtitle,
    tooltip_text,
    caption) {
  
  hchart(df, 
         "bar", hcaes(x = !!rlang::ensym(chart_x), y = !!rlang::ensym(chart_y), group = !!rlang::ensym(chart_group)),
         stacking = "percent",
         tooltip =  list(headerFormat='',pointFormat=chart_tooltip)) %>%
    
    hc_title(
      text = chart_title,
      align = "left",
      widthAdjust = -50) %>%
    
    hc_subtitle(
      text = chart_subtitle,
      align = "left") %>%
    
    hc_caption(
      text = chart_caption,
      align = "left") %>%
    
    hc_yAxis(title = list(text = paste0(""))) %>%
    
    hc_xAxis(title = list(text = paste0(""),
                          labels=list(position="bottom"))) %>%
    
    hc_legend(enabled = TRUE, 
              reversed =  TRUE)%>% 
    
    hc_add_theme(cc_theme)%>%
    
    # hc_size(height=480, 
    #         width=830) %>%
    
    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600, 
      chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE,format='{point.rate:.1f}%')))),
      filename = paste0(chart_subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV'))))
}


#### Item Chart ####

# 
# 
# # for now filter just traffic and reasonable suspicion as separate dfs
# 
# df_traffic<-df%>%
#   filter(grepl('Traffic Violation',stop_reason_short))%>%
#   arrange(-hours_rate)


# mutate(hours_rate= round(hours_rate, 0))

# col <-c(meteorite, lavender, orange, peridot, "#177FEB", "#733256", "#9B9A9A", "#211447",
#         "#FF9E0D")
# 
# # mobile_screen_opts <- list(layout="vertical")
# 
# hc<-hchart(
#   df_traffic,
#   "item",
#   rows=7,
#   hcaes(
#     name = stop_result_short,
#     y = hours_rate,
#     label = stop_result_short,
#     color = col),
#   name = "Total Hours",
#   showInLegend = TRUE) %>%
#   hc_title(text = "The majority of hours LBPD spends on traffic stops only result in citations, warnings, or no action.", 
#            widthAdjust = -50,
#            align="left" )%>%
#   hc_subtitle(text = "Hours Spent on Traffic Stops by Result", 
#               align="left" ) %>%
#   hc_size(height=450) %>%
#   hc_caption(text = paste0(sourcenote," Analysis for all officer-initiated stops for traffic violations.")) %>%
#   hc_add_theme(cc_theme)%>%
#   hc_exporting(enabled = TRUE, sourceWidth=900, sourceHeight=600,
#                chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE,                                                           format=paste0(list(pointFormat='{point.hours_rate:.1f}')))))),
#                filename = paste0("Hours Spent on Traffic Stops by Result",
#                                  "_Catalyst California, catalystcalifornia.org, 2023."))%>%
#   
#   hc_legend(title=list(text='<span style="color: #000000; font-weight: bold">Single dot represents 1 hour out of 100 hours</span><br/><span style="color: #666; font-style: italic">Click to hide</span>'),
#             enable = TRUE,
#             labelFormat = '{name} <span style="opacity: 0.4">{hours_rate:.1f}</span>')  %>%
#   hc_tooltip(headerFormat="", 
#              pointFormat = "Out of 100 hours, LBPD spent <b>{point.hours_rate:.1f}</b> hours on traffic stops that resulted in <b>{point.stop_result_short}</b>") %>%
#   
#   # add in margins so the graph isn't cut off
#   hc_chart(
#     marginRight = 20,
#     marginLeft=20
#   )
# 
# hc


#### Testing Functions ####
library(RPostgreSQL)
source("W:\\RDA Team\\R\\credentials_source.R")
pillars_conn <- connect_to_db("rjs_pillars")
ourla_conn <- connect_to_db("pv_lareform")

## Load in RJS Pillars data: Bubble Pop test ##
report_stoprates_race_person <- dbGetQuery(pillars_conn, "SELECT * FROM data.report_gang_stoprates_race_person")  %>% filter(race!="total" & !is.na(stop_count))

## Load in Our LA stacked data: Stacked Bar Chart test ##
library(stringr)
source("W:/Project/Political Voice/LA Reform/Phase 1/R/lareform_chart_prep/lareform_chart_prep/chart_styling.R")

# Step 2a: pull df from postgres
df <-dbGetQuery(ourla_conn, "SELECT * FROM report_effect_total") %>% 
  filter(question %in% c("irc_10a", "irc_10b", "irc_10c")) %>% #filter for the question needed 
  filter(value != "Unsure/Don't Know")  #filter out response options not of interest

# Step 2b: pull data dictionary                     
dict <-dbGetQuery(ourla_conn, "SELECT * FROM data_dictionary") %>% 
  filter(variable %in% c("irc_10a", "irc_10b", "irc_10c")) #filter for the question needed 

# Step 2c: merge databases for labels
df_merge <- merge(df, dict, by.x = c("question"), by.y = c("variable"), all.x = TRUE)
df_merge<-df_merge%>%rename(question_label=sub_question) #pull out question label for pop-up

# Step 2d: The following levels set the order for the response options so they display correctly. Use the one that corresponds to the question you're visualizing for. 
effect_levels<-c("Very positive effect","Slight positive effect","No effect","Slight negative effect","Very negative effect")
effect_labels<-c("Very positive effect","Slight positive effect","No effect","Slight negative effect","Very negative effect")
df_merge$value<-factor(df_merge$value, ordered=TRUE, levels=effect_levels, labels=effect_labels)

# STEP 3: INSERT the text needed in the visual and follow the standards in this template. 

# TITLE 
title_text <- paste0("The majority of people believe an IRC will have a positive effect on their representation.")

# SUBTITLE
subtitle_text <- paste0("Percent of people by how they feel an IRC would impact their representation")

# N CALCULATION REVISION
n<-df%>%
  group_by(question)%>%
  mutate(n=min(total))%>%
  slice(1)%>%
  select(question, n)%>%
  left_join(dict, by=c("question"="variable"))%>%
  select("question", "n", "sub_question")

# CAPTION 
# take the question text from the data dictionary 
question <- dict[1, "question"] #may need to use sub_question depending on what you're visualizing for

# take the sample size from the data frame

subquestion<-paste0(n[1,3]," (n=",n[1,2],")/",n[2,3]," (n=",n[2,2],")/",n[3,3]," (n=",n[3,2],")")

caption_text <- paste0("<br>Survey Question: ", question," ",subquestion,"<br>",cc_footnote)

# EXTRA TOOLTIP PREP 

df_merge<-df_merge%>%
  mutate(value_tooltip=ifelse(value!="No effect", paste0("a ","<b>",str_to_title(value),"</b>"),
                              paste0("<b>",str_to_title(value),"</b>")))
# TOOLTIP
tooltip_text <- "<b>{point.rate:.1f}%</b> of people believe an IRC would have {point.value_tooltip} on <br><b>'{point.question_label}'</b>"


## Load in RJS Pillars Data: Item Chart test ##
timespent_ficard_race_person <- dbGetQuery(pillars_conn, "SELECT * FROM data.report_timespent_ficard_race_person") %>%
  filter(race != "overall")
col <-c(meteorite, lavender, orange, peridot, ccblue, gainsboro, "#211447",
        "#FF9E0D", "#A8683C")




## Test Stacked Bar Chart ##
fx_stack(  
  df = df_merge,
  chart_x = 'question_label',
  chart_y = 'rate',
  chart_group = 'value',
  chart_title = title_text,
  chart_subtitle = subtitle_text,
  chart_tooltip = tooltip_text,
  chart_caption = caption_text)

## Test Item Chart ##
hchart(
  timespent_ficard_race_person,
  "item",
  rows=7,
  hcaes(
    name = race,
    y = duration_rate,
    label = race,
    color = col),
  name = "Total Hours",
  showInLegend = TRUE) %>%
  hc_title(text = "TOP SYSTEMIC FINDING STATEMENT", 
           widthAdjust = -50,
           align="left" )%>%
  hc_subtitle(text = "Hours Spent on Field Interview Cards by Race", 
              align="left" ) %>%
  hc_size(height=450) %>%
  hc_caption(text = paste0(sourcenote," Analysis for all officer-initiated stops RESULTING IN FIELD INTERVIEWS ONLY.")) %>%
  hc_add_theme(cc_theme)%>%
  hc_exporting(enabled = TRUE, sourceWidth=900, sourceHeight=600,
               chartOptions=list(plotOptions=list(series=list(dataLabels=list(enabled=TRUE, format=paste0(list(pointFormat='{point.duration_rate:.1f}')))))),
               filename = paste0("Hours Spent CONDUCTING FIELD INTERVIEWS BY RACE",
                                 "_Catalyst California, catalystcalifornia.org, 2023."))%>%
  
  hc_legend(title=list(text='<span style="color: #000000; font-weight: bold">Single dot represents 1 hour out of 100 hours</span><br/><span style="color: #666; font-style: italic">Click to hide</span>'),
            enable = TRUE,
            labelFormat = '{name} <span style="opacity: 0.4">{duration_rate:.1f}</span>')  %>%
  hc_tooltip(headerFormat="", 
             pointFormat = "Out of 100 hours, SDPD spent <b>{point.duration_rate:.1f}</b> hours ON STOPS CONDUCTING FIELD INTERVIWS OF <b>{point.race} PEOPLE</b>") %>%
  
  # add in margins so the graph isn't cut off
  hc_chart(
    marginRight = 20,
    marginLeft=20
  )
