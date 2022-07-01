# ==============================================================================
#
# Create visualizations for inclusive data communications
#
# Status: In Progress
#
# Notes: remove white backgrounds, remove white lines
# 
# ==============================================================================
#
# Author: Brieanne Forbes; brieanne.forbes@pnnl.gov
# 27 June 2022
#
# ==============================================================================

library(tidyverse)
library(fuzzyjoin)
library(lubridate)
library(viridis)
library(ggpubr)

# ================================= User inputs ================================

file <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/RC2_DIC_NPOC_TN_TSS_Ions_2021-2022.csv'

metadata_file <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/RC2_Field_Metadata.csv'

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Documents/GitHub/Inclusive-Data-Communication/Inclusive-Data-Communication-Visualizations/data_time_series.pdf'

# ================================= read file =================================

data <- read_csv(file, skip = 2) %>%
  slice(12:609) %>%
  select(Sample_Name, '00691_DIC_mg_per_L_as_C','00681_NPOC_mg_per_L_as_C', '00602_TN_mg_per_L_as_N')%>%
  rename(DIC = '00691_DIC_mg_per_L_as_C',
         NPOC = '00681_NPOC_mg_per_L_as_C',
         TN = '00602_TN_mg_per_L_as_N')

metadata <- read_csv(metadata_file)%>%
  select(Site_ID,Site_Name,Sample_Name, Date) 

merge <- regex_left_join(data, metadata) %>%
  mutate(Date = mdy(Date),
         DIC = as.numeric(DIC),
         TN = as.numeric(TN),
         NPOC = as.numeric(NPOC))

merge$DIC[which(merge$DIC == "-9999")] <- NA
merge$TN[which(merge$TN == "-9999")] <- NA
merge$NPOC[which(merge$NPOC == "-9999")] <- NA

tidy <- merge %>% filter(Site_Name == c('Yakima at Kiona', 'Yakima at Mabton', 'Yakima at Union Gap')) %>%
  mutate(River_Location = case_when(Site_Name == 'Yakima at Union Gap' ~ 'Upstream',
                                    Site_Name == 'Yakima at Kiona' | Site_Name == 'Yakima at Mabton'~ 'Downstream'),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"))

# ================================= set theme ================================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.text = element_text(size = 9),
    line = element_line(size = 0.05),
    axis.line = element_line(size = 0.5),
    panel.background = element_rect(color = 'white'),
    panel.border = element_blank(),
    plot.title = element_text(size = 25, face = 'bold'),
    axis.ticks.length = unit(.25, 'cm'),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(size = 11, angle = 90, vjust = 3)
  )
)

# ================================= plot ================================

colors <- mako(2, begin = 0.3,end = 0.7)


dic <- ggplot(tidy, aes(x=Date)) +
  geom_smooth(data = tidy, aes(y=DIC, color = River_Location, fill = River_Location), se = T, alpha = 0.2)+
  scale_x_date('Date', date_breaks = "1 month", date_labels = "%b") +
  theme(axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Dissolved Inorganic\nCarbon (mg/L)')+
  scale_color_manual(values = colors, name = 'Site Location')+
  scale_fill_manual(values = colors, name = 'Site Location') 

tn <- ggplot(tidy, aes(x=Date)) +
  geom_smooth(data = tidy, aes(y=TN, color = River_Location, fill = River_Location), se = T, alpha = 0.2)+
  scale_x_date('Date', date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Total Nitrogen\n(mg/L)')+
  scale_color_manual(values = colors, name = 'Site Location')+
  scale_fill_manual(values = colors, name = 'Site Location')

npoc <- ggplot(tidy, aes(x=Date)) +
  geom_smooth(data = tidy, aes(y=NPOC, color = River_Location, fill = River_Location), se = T, alpha = 0.2)+
  scale_x_date('Date', date_breaks = "1 month", date_labels = "%B") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(angle = 180)) +
  ylab('Dissolved Organic\nCarbon (mg/L)')+
  scale_color_manual(values = colors, name = 'Site Location',
                     guide = guide_legend(direction = "vertical", title.position = "left", title.theme = element_text(angle = 90, hjust = 0.5, family = 'serif'),
                                          label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, label.theme = element_text(angle = 90, family = 'serif')))+
  scale_fill_manual(values = colors, name = 'Site Location') 

# , name = '                                                2021                                                               2022'

legend <- get_legend(npoc)

npoc <- npoc + theme(legend.position = 'none')


year1 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "bold(2021)"), 
            parse = TRUE, size = 6, hjust = -1, 
            family = 'serif'
            ) +
  theme_void()

year2 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "bold(2022)"), 
            parse = TRUE, size = 6, hjust = -1,
            family = 'serif') +
  theme_void()


smoosh <- ggarrange(
  tn, 
  dic, 
  npoc,
  ncol = 1,
  nrow = 3,
  widths = c(8),
  heights = c(2, 2, 2.5),
  common.legend = T, 
  legend = 'right',
  legend.grob = legend
)

smoosh <- smoosh + theme(plot.margin = unit(c(3, 3, 3, 3), "cm"),
        panel.background = element_rect(color = 'white', fill = 'white'))

# smoosh <-
#   annotate_figure(smoosh,
#                   bottom = text_grob(
#                     c('2021', '2022'),
#                     size = 14,
#                     family = 'serif',
#                     face = 'bold'
#                   )) +
#   theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
#         panel.background = element_rect(color = 'white', fill = 'white'))

ggsave(
  outdir,
  smoosh,
  device = 'pdf',
  width = 9.5,
  height = 8,
  units = 'in',
  dpi = 300
)

