library(tidyverse)

d_wide <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/d_wide.csv")
d_long <- read_csv("https://raw.githubusercontent.com/rtotbagi/ClimateXSupernatural/refs/heads/main/d_long.csv")

# //////////////////////////
# Szűrés létező adatokra
# //////////////////////////

analysis_data <- d_wide %>%
  filter(
    !is.na(EA034),
    !is.na(SCCS649),
    !is.na(SCCS652),
    !is.na(SCCS653),
    !is.na(SCCS654),
    !is.na(SCCS655),
    !is.na(SCCS656),
    !is.na(AnnualTemperatureVariance),
    !is.na(AnnualPrecipitationVariance),
    !is.na(TemperaturePredictability),
    !is.na(PrecipitationPredictability)
  )

nrow(analysis_data)


ggplot(
  analysis_data,
  aes(x = AnnualTemperatureVariance)
) +
  geom_histogram()


ggplot(
  analysis_data,
  aes(x = AnnualTemperatureVariance)
) +
  geom_histogram(bins = 20)

ggplot(
  analysis_data,
  aes(
    x = AnnualTemperatureVariance,
    y = AnnualPrecipitationVariance
  )
) +
  geom_point()


# //////////////////////////
# Éghajlat
# //////////////////////////

analysis_data <- analysis_data %>%
  mutate(
    TemperatureUnpredictability = 1 - TemperaturePredictability,
    PrecipitationUnpredictability = 1 - PrecipitationPredictability,
    
    AnnualTemperatureVariance_z = as.numeric(scale(AnnualTemperatureVariance)),
    AnnualPrecipitationVariance_z = as.numeric(scale(AnnualPrecipitationVariance)),
    TemperatureUnpredictability_z = as.numeric(scale(TemperatureUnpredictability)),
    PrecipitationUnpredictability_z = as.numeric(scale(PrecipitationUnpredictability)),
    
    ClimateIndex = rowMeans(
      cbind(
        AnnualTemperatureVariance_z,
        AnnualPrecipitationVariance_z,
        TemperatureUnpredictability_z,
        PrecipitationUnpredictability_z
      ),
      na.rm = TRUE
    )
  )


summary(analysis_data$ClimateIndex)


ggplot(analysis_data, aes(x = ClimateIndex)) +
  geom_histogram(bins = 20) +
  labs(
    x = "ClimateIndex",
    y = "Kultúrák száma"
  )


analysis_data %>%
  select(
    AnnualTemperatureVariance_z,
    AnnualPrecipitationVariance_z,
    TemperatureUnpredictability_z,
    PrecipitationUnpredictability_z
  ) %>%
  cor(use = "pairwise.complete.obs")

analysis_data %>%
  select(
    ClimateIndex,
    AnnualTemperatureVariance,
    AnnualPrecipitationVariance,
    TemperaturePredictability,
    PrecipitationPredictability
  ) %>%
  cor(use = "pairwise.complete.obs")


analysis_data %>%
  select(
    AnnualTemperatureVariance_z,
    AnnualPrecipitationVariance_z,
    TemperatureUnpredictability_z,
    PrecipitationUnpredictability_z
  ) %>%
  cor(use = "pairwise.complete.obs")

climate_component_correlations <- analysis_data %>%
  select(
    ClimateIndex,
    AnnualTemperatureVariance,
    AnnualPrecipitationVariance,
    TemperaturePredictability,
    PrecipitationPredictability
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  select(Variable, ClimateIndex) %>%
  filter(Variable != "ClimateIndex") %>%
  mutate(
    AbsCorrelation = abs(ClimateIndex)
  ) %>%
  arrange(desc(AbsCorrelation))

climate_component_correlations

# //////////////////////////
# Természetfeletti hiedelmek
# //////////////////////////

ggplot(
  analysis_data,
  aes(x = factor(EA034))
) +
  geom_bar() +
  labs(
    x = "EA034",
    y = "Kultúrák száma"
  )


ggplot(
  analysis_data,
  aes(
    x = factor(EA034),
    y = TemperaturePredictability
  )
) +
  geom_boxplot()


analysis_data <- analysis_data %>%
  mutate(
    ConservativeIndex = EA034
  )

ggplot(
  analysis_data,
  aes(x = factor(ConservativeIndex))
) +
  geom_bar() +
  labs(
    title = "A konzervatív természetfeletti beavatkozás index eloszlása",
    x = "EA034 kategória",
    y = "Kultúrák száma"
  )


analysis_data <- analysis_data %>%
  mutate(
    InterventionIndex =
      rowMeans(
        cbind(
          SCCS649,
          SCCS652,
          SCCS653,
          SCCS654,
          SCCS655,
          SCCS656
        ),
        na.rm = TRUE
      )
  )

summary(analysis_data$InterventionIndex)

ggplot(
  analysis_data,
  aes(x = InterventionIndex)
) +
  geom_histogram(
    bins = 15
  ) +
  labs(
    title = "Az Intervention Index eloszlása",
    x = "Intervention Index",
    y = "Kultúrák száma"
  )


intervention_cor <- analysis_data %>%
  select(
    SCCS649,
    SCCS652,
    SCCS653,
    SCCS654,
    SCCS655,
    SCCS656
  ) %>%
  cor(use = "pairwise.complete.obs")

intervention_cor




# //////////////////////////
# Elemzés
# //////////////////////////

summary(
  analysis_data[,c(
    "ClimateIndex",
    "ConservativeIndex",
    "InterventionIndex"
  )]
)

hist(analysis_data$ClimateIndex)

hist(analysis_data$ConservativeIndex)

hist(analysis_data$InterventionIndex)

analysis_data %>% 
  ggplot(aes(ClimateIndex, ConservativeIndex)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  xlab('Éghajlat szélsőségesség') +
  ylab('Létező természetfeletti')

analysis_data %>% 
  ggplot(aes(ClimateIndex, InterventionIndex)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  xlab('Éghajlat szélsőségesség') +
  ylab('Beavatkozó természetfeletti')


cor(
  analysis_data$ClimateIndex,
  analysis_data$ConservativeIndex,
  use = "pairwise.complete.obs"
)

cor(
  analysis_data$ClimateIndex,
  analysis_data$InterventionIndex,
  use = "pairwise.complete.obs"
)

library(Hmisc)

rcorr(
  as.matrix(
    analysis_data[,c(
      "ClimateIndex",
      "ConservativeIndex",
      "InterventionIndex"
    )]
  ),
  type="spearman"
)

model1 <- lm(
  InterventionIndex ~ ClimateIndex,
  data = analysis_data
)

summary(model1)


model2 <- lm(
  InterventionIndex ~
    ClimateIndex +
    ConservativeIndex,
  data = analysis_data
)

summary(model2)



ggplot(
  analysis_data,
  aes(
    ClimateIndex,
    ConservativeIndex
  )
) +
  geom_point() +
  geom_smooth() +
  xlab('Éghajlat szélsőségesség') +
  ylab('Létező természetfeletti')


ggplot(
  analysis_data,
  aes(
    ClimateIndex,
    InterventionIndex
  )
) +
  geom_point() +
  geom_smooth() +
  xlab('Éghajlat szélsőségesség') +
  ylab('Beavatkozó természetfeletti')

