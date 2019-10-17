library(tidyverse)
library(xml2)
library(httr)
library(reshape2)
library(gganimate)
library(magick)

yield_curve <- read_xml("https://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData")

entry_positions <- yield_curve %>% 
  xml_children() %>% 
  xml_name() 

entries <- yield_curve %>% 
  xml_children() %>% 
  .[which(entry_positions == "entry")] %>% 
  as_list() 

yield_content <- list()
for(i in 1:length(entries)){
  yield_content[[i]] <- entries[[i]] %>% 
    lapply(unlist) %>% 
    .$content %>% 
    enframe() %>% dcast("key"~name) %>% select(-`"key"`)
}

yields <- yield_content %>% bind_rows()

names(yields) <- gsub("properties.", "", names(yields))

yields <- yields %>% select(NEW_DATE, BC_3MONTH, BC_5YEAR, BC_10YEAR, BC_30YEAR)
names(yields)[1] <- "Date"

yields <- sapply(yields[,-1], as.numeric) %>% as_tibble() %>% bind_cols(yields["Date"])


yields <- yields %>% group_by(Date) %>% summarize(`Five Year Yield Curve` = BC_5YEAR - BC_3MONTH,
                                         `Ten Year Yield Curve` = BC_10YEAR - BC_3MONTH,
                                         `Thirty Year Yield Curve` = BC_30YEAR - BC_3MONTH)


yields$Date <- as.Date(yields$Date)

g <- yields %>% melt(id.vars = "Date") %>% 
  ggplot(aes(x=Date, y=value, color = variable)) + 
  geom_line(alpha = .5) + geom_point() + 
  coord_cartesian(clip = 'off') +
  theme_minimal() + 
  labs(color = "", y = "Yield Curve (%)") + 
  geom_segment(aes(xend = as.Date("2020-01-01"), yend = value, color = variable), linetype = 2)  + 
  geom_text(aes(x = as.Date("2020-01-02"), label = paste0(round(value,1),"%")), size = 12, alpha = 1, hjust = 0) + 
  theme(legend.title = element_blank(), text = element_text(size = 20)) + 
  ggtitle("Yield Curves (minus 3 Month yield)")


anim <- g + transition_reveal(Date) 


anim <- animate(anim, width = 1920, height = 1080, fps = 60)

anim_save("yield_curve.gif", anim)







