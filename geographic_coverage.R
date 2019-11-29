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
         MMR_Coverage = cut(z,      breaks = c(-Inf, 
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
  select(id, n, d, r, theta, z, MMR_Coverage, lat, lng, school_name)

#legend summary
imm_data_cleaned$tooltip = paste(paste0(imm_data_cleaned$school_name, ":"), paste0(round(imm_data_cleaned$r,2)*100, "%"))
imm_data_cleaned$r2 <- imm_data_cleaned$r * 1000

key="pk.eyJ1IjoibWF0dGt1bWFyIiwiYSI6ImNrMzdmc3gxNzBjNDEzb3FzdHFybjJ3ZHcifQ.rGj37g7Vumg1lbeAegUasA"



mapdeck(token = key, style = mapdeck_style("dark"),  pitch = 45) %>%
  add_pointcloud(
    data = imm_data_cleaned
    , lon = 'lng'
    , lat = 'lat'
    , elevation = 'r2'
    , layer_id = 'point'
    , fill_colour = "MMR_Coverage"
    , tooltip = 'tooltip'
    , legend = TRUE
  )
