require(rgdal)
require(tmap) # install tmap before tidyverse for plotting fns to work
require(tidyverse)
require(naniar)
require(summarytools)
require(corrplot)

source("src/garden_plot_functions.R")

#########################
# Manual cleaning steps #
#########################

# Renamed columns with shorter names
# Made sure there were no spaces between plot number & direction (e.g. 5NE)
# Made sure there were commas between multiple plots on same line
# Replaced '30W' with '30NW, 30SW'
# Replaced '74N' with '74' (for now- need to divide in QGIS)
# Replaced '18SW' from Paolo with '28SE', need to check- Manuela has
# also reported having 18SW and it seems like there might be a typo

###############################
# First cleaning steps with R #
###############################

# Set both 'Didn't grow' and 'None' to NA for fruit/veg because
# respondents didn't answer consistently and we can't use.
# Convert 'Yes' and 'No' to 1 and 0
# Remove unwanted columns, add 'responded' col to track who answered

data.file <- "data/survey_data/processed/01_Gardening Club Data 2018 Manually Adjusted.csv"

df.start <- read_csv(data.file)
df.start <- df.start %>%
  replace_with_na_all(condition = ~ .x == "Didn't grow") %>%
  replace_with_na_at(
    .vars = names(df.start)[17:61],
    condition = ~ .x == "None"
  ) %>%
  mutate(
    Fertilizer = if_else(Fertilizer == "Yes", 1, 0),
    Manure = if_else(Manure == "Yes", 1, 0),
    Nematodes = if_else(Nematodes == "Yes", 1, 0)
  ) %>%
  select(-Timestamp, -Name) %>%
  mutate("Responded" = T)

write_csv(
  df.start,
  "data/survey_data/processed/02_Gardening Club Data 2018 Partially Cleaned.csv"
)

################################
# Second cleaning steps with R #
################################

# Read data with specified column types
# One line for each plot number

pest.levels <- c("None", "Low", "Medium", "High")
yield.levels <- c("Low", "Medium", "High")

data.file <- "data/survey_data/processed/02_Gardening Club Data 2018 Partially Cleaned.csv"
df.partial <- read_csv(data.file,
  col_types = cols(
    Plot = col_character(),
    Mole_cricket = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Mice = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Birds = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Slugs = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Couch_grass = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Nettles = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Bindweed = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Horsetail = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Ground_elder = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Other_weed = col_factor(
      levels = pest.levels,
      ordered = T
    ),
    Other_weed_detail = col_character(),
    Fertilizer = col_logical(),
    Manure = col_logical(),
    Nematodes = col_logical(),
    Comments = col_character(),
    Responded = col_logical(),
    .default = col_factor(
      levels = yield.levels,
      ordered = T
    )
  )
)
df <- df.partial %>%
  mutate(Plot = strsplit(Plot, ",")) %>%
  unnest(Plot) %>%
  mutate(Plot = gsub(" ", "", Plot))

######################################
# A bit of exploratory data analysis #
######################################

df.nontext <- df %>%
  select(-Other_weed_detail, -Comments, -Responded, -Plot)

view(dfSummary(df.nontext, valid.col = FALSE))

# Look at correlation for mole crickets, weeds, treatments
cor.vars <- df %>%
  select(names(df)[1:14]) %>%
  select(-Other_weed_detail, -Birds, -Mice, -Slugs, -Other_weed) %>%
  data.matrix()

corS <- cor(cor.vars, method = "spearman")
corSsig <- cor.mtest(cor.vars, method = "spearman", exact = F)

# corr plot where insignificant correlations show p-values
pdf(file = "output/plots/Corr_plot_Spearman_siglevel_0.05_insig_pvalue.pdf")
corrplot(corS,
  order = "hclust", tl.col = "black", tl.srt = 45,
  p.mat = corSsig$p, sig.level = .05, insig = "p-value"
)

dev.off()

# corr plot where insignificant correlations are left blank
pdf(file = "output/plots/Corr_plot_Spearman_siglevel_0.05_insig_blank.pdf")
corrplot(corS,
  order = "hclust", tl.col = "black", tl.srt = 45,
  p.mat = corSsig$p, sig.level = .05, insig = "blank"
)
dev.off()

###########################################
# Load shapefiles & join with survey data #
###########################################

plot.shapes <- readOGR(
  dsn = "data/map_data",
  layer = "Plots"
)

df2 <- left_join(df, plot.shapes@data, by = c("Plot" = "Name")) # for bar charts

plot.shapes@data <- left_join(plot.shapes@data,
  df,
  by = c("Name" = "Plot")
)

############################################
# PLOTS                                    #
#                                          #
# Automatically generate maps and stacked  #
# barplots and save them                   #
############################################

### MAPS ###

# Columns to map
pests <- c("Mole_cricket", "Mice", "Birds", "Slugs")
weeds <- c(
  "Couch_grass", "Nettles", "Bindweed", "Horsetail", "Ground_elder",
  "Other_weed"
)
treatments <- c("Fertilizer", "Manure", "Nematodes")
fruits <- names(plot.shapes@data)[c(18:30)]
fruits.minus.figs <- names(plot.shapes@data)[c(18:22, 24:30)] # no one grew figs
veg <- names(plot.shapes@data)[31:62]
veg1 <- names(plot.shapes@data)[31:42]
veg2 <- names(plot.shapes@data)[43:54]
veg3 <- names(plot.shapes@data)[55:62]

# Which plots responded
makeMap(
  shapefile = plot.shapes, fill.vars = c("Responded"),
  fill.palette = "Set2", ncol = 1, show.text = T,
  text = "Name", save.map = T, save.name = "output/plots/Responded.pdf"
)

# Pest distribution
makeMap(
  shapefile = plot.shapes, fill.vars = pests,
  fill.palette = "Reds", ncol = 2, show.text = F,
  save.map = T, save.name = "output/plots/Pests.pdf"
)
makeMapLoop(
  shapefile = plot.shapes, var.list = pests, var.group = "Pests",
  fill.palette = "Reds", show.text = T, save.map = T
)

# Weed distribution
makeMap(
  shapefile = plot.shapes, fill.vars = weeds,
  fill.palette = "Blues", ncol = 3, show.text = F,
  save.map = T, save.name = "output/plots/Weeds.pdf"
)
makeMapLoop(
  shapefile = plot.shapes, var.list = weeds, var.group = "Weeds",
  fill.palette = "Blues", show.text = T, save.map = T
)

# Treatment distribution
makeMap(
  shapefile = plot.shapes, fill.vars = treatments,
  fill.palette = "Oranges", ncol = 2, show.text = F,
  save.map = T, save.name = "output/plots/Treatments.pdf"
)
makeMapLoop(
  shapefile = plot.shapes, var.list = treatments, var.group = "Treatments",
  fill.palette = "Oranges", show.text = T, save.map = T
)

# Fruit distribution
makeMap(
  shapefile = plot.shapes, fill.vars = fruits.minus.figs,
  fill.palette = "Purples", ncol = 4, show.text = F,
  save.map = T, save.name = "output/plots/Fruit.pdf"
)
makeMapLoop(
  shapefile = plot.shapes, var.list = fruits, var.group = "Fruit",
  fill.palette = "Purples", show.text = T, save.map = T
)

# Veg distribution
makeMap(
  shapefile = plot.shapes, fill.vars = veg1,
  fill.palette = "Greens", ncol = 4, show.text = F,
  save.map = T, save.name = "output/plots/Veg1.pdf"
)
makeMap(
  shapefile = plot.shapes, fill.vars = veg2,
  fill.palette = "Greens", ncol = 4, show.text = F,
  save.map = T, save.name = "output/plots/Veg2.pdf"
)
makeMap(
  shapefile = plot.shapes, fill.vars = veg3,
  fill.palette = "Greens", ncol = 3, show.text = F,
  save.map = T, save.name = "output/plots/Veg3.pdf"
)
makeMapLoop(
  shapefile = plot.shapes, var.list = veg, var.group = "Veg",
  fill.palette = "Greens", show.text = T, save.map = T
)

### STACKED BARPLOTS ###

# Pests
df.pests <- df2 %>%
  select(pests, Area) %>%
  gather(pests, key = "Pest", value = "Level") %>%
  group_by(Pest, Level) %>%
  summarize(Count = n(), Total_Area = sum(Area)) %>%
  mutate(Level = factor(Level,
    levels = c("High", "Medium", "Low", "None"),
    ordered = T
  ))


makeStackedBarplot(
  plot.data = df.pests, fill.var = "Level", x.var = "Pest",
  y.var = "Total_Area",
  plot.title = "Pest levels by area",
  x.axis = "Pest type",
  y.axis = bquote("Total area affected" ~ (m ^ 2)),
  text.angle = 90, save.plot = T,
  plot.width = 4, plot.height = 5.5,
  save.name = "output/plots/Barplot_Pests_by_Area.pdf"
)

# Pests by respondent
df.pests2 <- df.partial %>%
  select(pests) %>%
  gather(pests, key = "Pest", value = "Level") %>%
  group_by(Pest, Level) %>%
  summarize(Count = n()) %>%
  mutate(Level = factor(Level,
    levels = c("High", "Medium", "Low", "None"),
    ordered = T
  ))


makeStackedBarplot(
  plot.data = df.pests2, fill.var = "Level", x.var = "Pest",
  y.var = "Count",
  plot.title = "Pest levels by respondent",
  x.axis = "Pest type", y.axis = "Number respondents",
  text.angle = 90, save.plot = T, plot.width = 4,
  plot.height = 5.5,
  save.name = "output/plots/Barplot_Pests_by_respondent.pdf"
)

# Weeds by area
df.weeds <- df2 %>%
  select(weeds, Area) %>%
  gather(weeds, key = "Weed", value = "Level") %>%
  group_by(Weed, Level) %>%
  summarize(Count = n(), Total_Area = sum(Area)) %>%
  mutate(Level = factor(Level,
    levels = c("High", "Medium", "Low", "None"),
    ordered = T
  ))

makeStackedBarplot(
  plot.data = df.weeds, fill.var = "Level", x.var = "Weed",
  y.var = "Total_Area",
  plot.title = "Weed levels by area",
  x.axis = "Weed type", y.axis = bquote("Total area affected" ~ (m ^ 2)),
  text.angle = 90, save.plot = T,
  plot.width = 4.5, plot.height = 5.5,
  save.name = "output/plots/Barplot_Weeds_by_Area.pdf"
)

# Weeds by respondent
df.weeds2 <- df.partial %>%
  select(weeds) %>%
  gather(weeds, key = "Weed", value = "Level") %>%
  group_by(Weed, Level) %>%
  summarize(Count = n()) %>%
  mutate(Level = factor(Level,
    levels = c("High", "Medium", "Low", "None"),
    ordered = T
  ))


makeStackedBarplot(
  plot.data = df.weeds2, fill.var = "Level", x.var = "Weed",
  y.var = "Count",
  plot.title = "Weed levels by respondent",
  x.axis = "Weed type", y.axis = "Number respondents",
  text.angle = 90, save.plot = T, plot.width = 4.5,
  plot.height = 5.5,
  save.name = "output/plots/Barplot_Weeds_by_respondent.pdf"
)

# Treatments
df.treatments <- df.partial %>%
  select(treatments) %>%
  gather(treatments, key = "Treatment", value = "Used") %>%
  group_by(Treatment, Used) %>%
  summarize(Count = n()) %>%
  mutate(Level = factor(Used, levels = c("No", "Yes"), ordered = T))


makeStackedBarplot(
  plot.data = df.treatments, fill.var = "Used", x.var = "Treatment",
  y.var = "Count", scale_values = c("tomato", "royalblue"),
  plot.title = "Treatment use by gardening club members",
  x.axis = "Treatment type", y.axis = "Number of respondents",
  text.angle = 90, save.plot = T, plot.width = 5, plot.height = 3.5,
  save.name = "output/plots/Barplot_Treatmentss.pdf"
)

# Fruits
df.fruits <- df.partial %>%
  select(fruits) %>%
  gather(fruits,
    key = "Fruit", value = "Yield"
  ) %>%
  group_by(Fruit, Yield) %>%
  summarize(Count = n()) %>%
  filter(!(is.na(Yield))) %>%
  mutate(Yield = factor(Yield,
    levels = c("High", "Medium", "Low"),
    ordered = T
  ))

makeStackedBarplot(
  plot.data = df.fruits, fill.var = "Yield", x.var = "Fruit",
  y.var = "Count", scale_values = c("red4", "tomato2", "lightcoral"),
  plot.title = "Fruit yields per respondent",
  x.axis = "Fruit type", y.axis = "Number of respondents",
  text.angle = 90, save.plot = T, plot.width = 4.5, plot.height = 5,
  save.name = "output/plots/Barplot_Fruits.pdf"
)

# Vegetables
df.veg <- df.partial %>%
  select(veg) %>%
  gather(veg,
    key = "Vegetable", value = "Yield"
  ) %>%
  group_by(Vegetable, Yield) %>%
  summarize(Count = n()) %>%
  filter(!(is.na(Yield))) %>%
  mutate(Yield = factor(Yield,
    levels = c("High", "Medium", "Low"),
    ordered = T
  ))

makeStackedBarplot(
  plot.data = df.veg, fill.var = "Yield", x.var = "Vegetable",
  y.var = "Count", scale_values = c("red4", "tomato2", "lightcoral"),
  plot.title = "Vegetable yields per respondent",
  x.axis = "Vegetable type", y.axis = "Number of respondents",
  text.angle = 90, save.plot = T, plot.width = 8, plot.height = 6.2,
  save.name = "output/plots/Barplot_Vegetables.pdf"
)
