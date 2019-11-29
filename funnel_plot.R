#Visualizing MMR coverage in Toronto elementary and secondary schools

#package setup
library(dplyr)
library(ggplot2)
library(opendatatoronto)
library(janitor)
library(ggdark)
library(showtext)

#get data using opendatatoronto
imm_package <- search_packages("Immunization")

imm_resources <- imm_package %>%
  list_package_resources()

imm_data <- imm_resources %>%
  filter(name == "immunization-coverage-2018-2019") %>%
  get_resource() 

#Clean and manage data
imm_data_cleaned = imm_data %>%
  clean_names() %>%
  mutate(n=round((mmr_coverage_rate_percent/100) * enrolled_population),
         d=enrolled_population,
         r=n/d,
         theta=sum(n)/sum(d),
         z=(r - theta)/sqrt((theta * (1 - theta))/d),
         score = cut(z,      breaks = c(-Inf, 
                                        qnorm((1-0.998)/2), #lower limit
                                        qnorm((1-0.95)/2),  #lower limit
                                        -qnorm((1-0.95)/2),  #upper limit
                                        -qnorm((1-0.998)/2),  #upper limit
                                        Inf),
                     labels=c('Very Low',
                              'Low',
                              'Average',
                              'High',
                              'Very High'))
         
  ) %>%
  select(id, n, d, r, theta, z, score)


#Limits
dff = funnelR::fundata(input=imm_data_cleaned, alpha = 0.95, alpha2 = 0.998, method = "approximate")


#Fonts
font_add_google("Raleway", "rale")
showtext_auto()

#Plot
#Plot Points
fp <- ggplot(data=imm_data_cleaned, aes(y=r, x=d)) +
  geom_point(size=4, aes(fill=imm_data_cleaned$score), shape=21) +
  scale_fill_viridis_d("MMR Coverage") +
  #Plot Average + Control Limits
  geom_hline(data = dff, aes(yintercept = dff$benchmark), color="red", size=1.2)  +
  geom_line(data=dff, aes(x=d, y=up), color="snow4", size=1.1, linetype='dotted') +
  geom_line(data=dff, aes(x=d, y=lo), color="snow4", size=1.1, linetype='dotted') +
  geom_line(data=dff, aes(x=d, y=up2), color="snow3", size=1.1, linetype='dashed') +
  geom_line(data=dff, aes(x=d, y=lo2), color="snow3", size=1.1, linetype='dashed') +
  #Plot General Aesthetics
  scale_y_continuous(breaks = seq(0.7, 1, 0.05), limits=c(0.7,1) ,labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0,1750, 250), limits=c(0,1750)) +
  dark_theme_minimal(base_family = "rale", base_size = 14) +
  theme(plot.title = element_text(family = "rale"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(), 
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27)) +
  #Plot Titles 
  labs(caption="Source: https://open.toronto.ca",
       x="\n\nNumber of Enrolled Students\n",
       y="\nMMR Coverage, %\n",
       title='MMR Immunization Coverage Rates for School pupils (7-17 year olds) in Toronto',
       subtitle='Years 2018/2019\n\n')
fp

pdf( "funnel.pdf", width=12, height=8)
fp
dev.off()
