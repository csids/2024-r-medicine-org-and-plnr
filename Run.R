project <- org::initialize_project(
  env     = .GlobalEnv,
  home    = c(
    "~/csids/2024-r-medicine-org-and-plnr/",
    "/project1/2024-r-medicine-org-and-plnr"
  ),
  data    = c(
    "~/csids/2024-r-medicine-org-and-plnr/data",
    "/project1/2024-r-medicine-org-and-plnr/data"
  ),
  results = c(
    "~/csids/2024-r-medicine-org-and-plnr/results",
    "/project1/2024-r-medicine-org-and-plnr/results"
  ),
  folders_to_be_sourced = "R"
)

library(data.table)
library(ggplot2)
library(magrittr)

org::project$data
org::project$results_today

# We begin by defining a new plan
p <- plnr::Plan$new()

# We add sources of data
# We can add data directly
p$add_data(
  name = "gastro",
  direct = readRDS(file.path(org::project$data, "gastroenteritis.RDS"))
)

# Add the argsets
for(i in c(
  "nation_nor",
  "county_nor03",
  "county_nor11",
  "county_nor15",
  "county_nor18",
  "county_nor31",
  "county_nor32",
  "county_nor33",
  "county_nor34",
  "county_nor39",
  "county_nor40",
  "county_nor42",
  "county_nor46",
  "county_nor50",
  "county_nor55",
  "county_nor56"
)) for(j in c(2016:2022)){
  p$add_argset(
    location_code = i,
    estimate_isoyear = j
  )
}

# Observe the argsets as a data.table
p$get_argsets_as_dt()

# Create an action function
# (takes two arguments -- data and argset)
fn_estimate_baseline <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(1)
  }

  d <- data$gastro[
    location_code==argset$location_code &
      isoyear %in% c((argset$estimate_isoyear-5):argset$estimate_isoyear)
  ]

  training_data <- d[isoyear < argset$estimate_isoyear]
  prediction_data <- d[isoyear == argset$estimate_isoyear]

  fit <- lm(gastro_n ~ isoyear, data = training_data)
  prediction_data <- cbind(
    prediction_data,
    predict(fit, prediction_data, interval = "prediction")
  )

  return(prediction_data)
}

# Apply the analysis function to all argsets
p$apply_action_fn_to_all_argsets(fn_name = "fn_estimate_baseline")

# Run all analyses
retval <- p$run_all()

# Bind the results together
res <- rbindlist(retval)

# Create a plot
q <- ggplot(res, aes(x = isoyear))
q <- q + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4)
q <- q + geom_line(aes(y = fit))
q <- q + geom_point(aes(y = gastro_n))
q <- q + facet_wrap(~location_code, scales = "free_y")
q

# Save the plot to results
ggplot2::ggsave(file.path(org::project$results_today, "figure_1.png"), q, width = 12, height = 12)
