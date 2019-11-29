#Visualizing MMR coverage in Toronto elementary and secondary schools

#package setup
library(opendatatoronto)
library(dplyr)
library(janitor)
library(mapdeck)


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
         `MMR Coverage` = cut(z,      breaks = c(-Inf, 
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
  select(id, n, d, r, theta, z, `MMR Coverage`, lat, lng, school_name)

#tool tip summary
imm_data_cleaned$tooltip = paste(imm_data_cleaned$school_name, ":", paste0(round(imm_data_cleaned$r,2)*100, "%"))
imm_data_cleaned$r2 <- imm_data_cleaned$r * 1000

key="insertyourmapdeckkeyhere"


#plot the map
mapdeck(token = key, style = mapdeck_style("dark"),  pitch = 45) %>%
  add_pointcloud(
    data = imm_data_cleaned
    , lon = 'lng'
    , lat = 'lat'
    , elevation = 'r2'
    , layer_id = 'point'
    , fill_colour = "MMR Coverage"
    , tooltip = 'tooltip'
    , legend = TRUE
  )