# ==============================================================================
#
# Create visualizations for inclusive data communications
#
# Status: In Progress
#
# Notes: 
# ==============================================================================
#
# Author: Brieanne Forbes; brieanne.forbes@pnnl.gov
# 27 June 2022
#
# ==============================================================================

library(tidyverse)
library(fuzzyjoin)
library(lubridate)

# ================================= User inputs ================================

file <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/RC2_DIC_NPOC_TN_TSS_Ions_2021-2022.csv'

metadata_file <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/RC2_Field_Metadata.csv'

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/data_time_series.png'

# ================================= read file =================================

data <- read_csv(file, skip = 2) %>%
  slice(12:609) %>%
  # select(-Field_Name)
  select(Sample_Name, '00691_DIC_mg_per_L_as_C','00602_TN_mg_per_L_as_N')%>%
  rename(DIC = '00691_DIC_mg_per_L_as_C',
         TN = '00602_TN_mg_per_L_as_N')

metadata <- read_csv(metadata_file)%>%
  select(Sample_Name, Date) 

merge <- regex_left_join(data, metadata) %>%
  mutate(Date = mdy(Date))

  # filter(str_detect("00691_DIC_mg_per_L_as_C", regex('_', ignore_case = TRUE)))

pivot <- merge %>%
  pivot_longer(cols = 2:3, names_to = 'variable', values_to = 'value') %>%
  filter(!is.na(Date))

# ================================= set theme ================================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    line = element_line(size = 0.05),
    axis.line = element_line(size = 0.5),
    panel.background = element_rect(color = 'white'),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 0.5
    ),
    plot.title = element_text(size = 25, face = 'bold'),
    axis.ticks.length = unit(.25, 'cm')
  )
)

# ================================= plot ================================

brks <- pivot$Date[seq(1, length(pivot$Date), 12)]
lbls <- lubridate::year(brks)

# dic_tn <- ggplot(pivot %>% arrange(Date), aes(x=Date)) +
#   geom_line(data = pivot %>% arrange(Date), aes(y=value))+
#   scale_x_date(labels = lbls, breaks = brks) +
#   facet_wrap(~variable)

dic_tn <- ggplot(merge%>% arrange(Date), aes(x=Date)) +
  geom_line(data = merge%>% arrange(Date), aes(y=DIC))+
  scale_x_date('"Date', date_breaks = "1 year") + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.5), 
        panel.grid.minor = element_blank())

