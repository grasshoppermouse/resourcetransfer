#+ messages=F, warnings=F

source("functions.R")

library(resourcetransferdata2012)
library(lavaan)
library(qgraph)
library(GENLIB)
library(ggcorrplot)
library(ggfortify)
library(patchwork)
library(scico)
library(hagenutils)


# MTurk -------------------------------------------------------------------

## MTurk rescaling ---------------------------------------------------------

mturk2011b <-
  mturk2011 %>%
  relocate(condition, .after = office_experience) %>%
  mutate(
    across(competition:give_benefit, ~scale(.x)[,1]),
    population = 'MTurk'
  )

## MTurk SEM ---------------------------------------------------------------

m_mturk <-
'# regressions
give_benefit ~ impression
give_benefit ~ work_gossip
give_benefit ~ family_gossip
impression ~ work_gossip
impression ~ family_gossip
'

mturkSEM <- sem(m_mturk, data = mturk2011b, group = 'condition')
summary(mturkSEM, standardized = TRUE)

## MTurk domain specificity ------------------------------------------------

m_mturkdomain <- lm(impression ~ condition*work_gossip + condition*family_gossip, mturk2011b)
summary(m_mturkdomain)

plot_mturk_work <- gossip_domain_plot(m_mturkdomain, 'work', mturk2011b)
plot_mturk_work

plot_mturk_family <- gossip_domain_plot(m_mturkdomain, 'family', mturk2011b)
plot_mturk_family

## Compare coefficients

m_mturk_family <- lm(impression ~ work_gossip + family_gossip, data = scale_subset(mturk2011, 'family'))
m_mturk_family_z <- signif(z_test(m_mturk_family, 'family_gossip', 'work_gossip'), 2)

m_mturk_work <- lm(impression ~ work_gossip + family_gossip, data = scale_subset(mturk2011, 'work'))
m_mturk_work_z <- signif(z_test(m_mturk_work, 'work_gossip', 'family_gossip'), 2)

mturk_congruent <-
  mturk2011b %>%
  pivot_longer(c(work_gossip, family_gossip), names_to = 'gossip_type', values_to = 'gossip_value') %>%
  mutate(
    congruent = str_detect(gossip_type, condition)
  ) %>%
  dplyr::select(-gossip_type) %>%
  pivot_wider(
    names_from = congruent,
    values_from = gossip_value,
    names_prefix = 'congruent'
  ) %>%
  rename(
    congruent = congruentTRUE,
    incongruent = congruentFALSE
  ) %>%
  mutate(
    impression = scale(impression)[,1],
    congruent = scale(congruent)[,1],
    incongruent = scale(incongruent)[,1],
    total_gossip = scale(congruent + incongruent)[,1]
  )

m_mturk_congruent <- lm(impression ~ congruent + incongruent, mturk_congruent)
summary(m_mturk_congruent)
m_mturk_congruent_z <- signif(z_test(m_mturk_congruent, 'congruent', 'incongruent'), 2)

## MTurk exploratory -------------------------------------------------------

# Variables to explore

mturk_vars <-
  c(
    'impression',
    'afraid',
    'competition',
    'criticize',
    'interference',
    'friendly',
    'get_your_job',
    'total_gossip',
    'give_benefit'
  )

## Age and sex effects -------------------------------------------------------

m_mturk_age <- lm(impression ~ age*congruent + age*incongruent, mturk_congruent)
summary(m_mturk_age)

## Interactions  -------------------------------------------------------

# Test for interaction between work_gossip and family_gossip on impression
m_mturk_wfinteract <- lm(impression ~ condition*work_gossip + condition*family_gossip + work_gossip*family_gossip, mturk2011b)
summary(m_mturk_wfinteract)

# Investigate interaction between congruent and incongruent gossip on impression
m_mturk_ci_interact <- lm(impression ~ congruent*incongruent, mturk_congruent)
summary(m_mturk_ci_interact)

## Correlation matrix -------------------------------------------------------
mturk_cor <- cor(mturk_congruent[mturk_vars])

plot_cor_mturk <-
  ggcorrplot(
  mturk_cor,
  hc.order = T,
  hc.method = 'ward.D',
  show.diag = F,
  lab = T,
  colors = scico(3, palette = 'bam')
)
plot_cor_mturk

## PCA -------------------------------------------------------
mturk_pca <- prcomp(mturk_congruent[mturk_vars], scale. = T)

## Gaussian graphical model -------------------------------------------------------
mturk_qgraph <- EBICglasso(mturk_cor, nrow(mturk_congruent), threshold = T, gamma = 0.5, lambda.min.ratio = 0.01)

## MTurk India vs US -------------------------------------------------------

mturk_india_us <-
  mturk_congruent %>%
  dplyr::filter(nation_home == 'India' | nation_home == 'United States')

m_mturk_india_us <- lm(impression ~ nation_home*congruent + nation_home*incongruent, mturk_india_us)
summary(m_mturk_india_us)

# Ngandu  -----------------------------------------

## Ngandu gossip statement ratings -----------------------------------------

# 1-8: family positive
# 9-16: family negative
# 17-24: work positive
# 25-32: work negative

ratings_tests <-
  ngandu_ratings %>%
  group_by(
    gossip_statement, gossip_valence, gossip_type
  ) %>%
  summarise(
    mean = mean(rating),
    test = t.test(rating)$p.value
  )

rating_summary <-
  ngandu_ratings %>%
  group_by(gossip_text, gossip_valence, gossip_type) %>%
  summarise(mean_rating = mean(rating)) %>%
  ungroup() %>%
  mutate(gossip_text = fct_reorder(gossip_text, mean_rating))

plot_ngandu_ratings <-
  ggplot(rating_summary, aes(mean_rating, gossip_text, colour = gossip_valence, shape = gossip_type)) +
  geom_point(size=3) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = '\nMean gossip rating', y = '') +
  theme_minimal(15)
plot_ngandu_ratings

## Ngandu SEM --------------------------------------------------------------

ngandu2012b <-
  ngandu2012 %>%
  mutate(
    id = as.character(id),
    across(family_gossip:believe_negative, ~scale(.x)[,1]),
    population = 'Ngandu'
  )

m_ngandu <-
'# regressions
give_benefit ~ impression
give_benefit ~ family_gossip
give_benefit ~ work_gossip
impression ~ family_gossip
impression ~ work_gossip
'

nganduSEM <- sem(m_ngandu, data=ngandu2012b, group = 'condition')
summary(nganduSEM, standardize = TRUE, rsq=T)

## Ngandu domain specificity -----------------------------------------------

m_nganduRep <- lm(impression ~ condition*work_gossip + condition*family_gossip, data = ngandu2012b)
summary(m_nganduRep)

plot_ngandu_work <- gossip_domain_plot(m_nganduRep, 'work', ngandu2012b) + ylim(-1.5,2)
plot_ngandu_work

plot_ngandu_family <- gossip_domain_plot(m_nganduRep, 'family', ngandu2012b) + ylim(-1.5,2)
plot_ngandu_family

# Compare coefficients

m_ngandu_family <- lm(impression ~ work_gossip + family_gossip, data=scale_subset(ngandu2012, 'family'))
m_ngandu_family_z <- signif(z_test(m_ngandu_family, 'family_gossip', 'work_gossip'), 2)

m_ngandu_work <- lm(impression ~ work_gossip + family_gossip, data=scale_subset(ngandu2012, 'work'))
m_ngandu_work_z <- signif(z_test(m_ngandu_work, 'work_gossip', 'family_gossip'), 2)

# Effects of belief in gossip

m_ngandu_belief <- lm(impression ~ work_gossip + family_gossip + believe_positive + believe_negative, ngandu2012b)
summary(m_ngandu_belief)

ngandu_congruent <-
  ngandu2012b %>%
  pivot_longer(c(family_gossip, work_gossip), names_to = 'gossip_type', values_to = 'gossip_value') %>%
  mutate(
    congruent = str_detect(gossip_type, condition)
  ) %>%
  dplyr::select(-gossip_type) %>%
  pivot_wider(
    names_from = congruent,
    values_from = gossip_value,
    names_prefix = 'congruent'
  ) %>%
  rename(
    congruent = congruentTRUE,
    incongruent = congruentFALSE
  ) %>%
  mutate(
    impression = scale(impression)[,1],
    congruent = scale(congruent)[,1],
    incongruent = scale(incongruent)[,1],
    total_gossip = scale(congruent + incongruent)[,1]
  )

m_ngandu_congruent <- lm(impression ~ congruent + incongruent, ngandu_congruent)
summary(m_ngandu_congruent)
m_ngandu_congruent_z <- signif(z_test(m_ngandu_congruent, 'congruent', 'incongruent'), 2)

# Investigate interaction between congruent and incongruent gossip on impression
m_ngandu_ci_interact <- lm(impression ~ congruent*incongruent, ngandu_congruent)
summary(m_ngandu_ci_interact)

ngandu_vars <- c(
  'impression',
  'friendly', # Same as in mturk analyses
  'afraid',
  'argue',
  'criticize',
  'total_gossip',
  'give_benefit'
)

ngandu_cor <- cor(ngandu_congruent[ngandu_vars])

plot_cor_ngandu <-
  ggcorrplot(
    ngandu_cor,
    hc.order = T,
    hc.method = 'ward.D',
    show.diag = F,
    lab = T,
    colors = scico(3, palette = 'bam')
  )
plot_cor_ngandu

ngandu_pca <- prcomp(ngandu_congruent[ngandu_vars], scale. = T)

ngandu_qgraph <- EBICglasso(ngandu_cor, nrow(ngandu_congruent), threshold = F, gamma = 0.5, lambda.min.ratio = 0.01)
qgraph(ngandu_qgraph, labels = ngandu_vars)

# Combo mturk ngandu ------------------------------------------------------

# Variables in common
vars <- c('id', 'condition', 'impression', 'work_gossip', 'family_gossip', 'friendly', 'afraid', 'criticize', 'population', 'give_benefit')

combo <- bind_rows(mturk2011b[vars], ngandu2012b[vars])

m_combo1 <- lm(impression ~ condition*work_gossip + condition*family_gossip, combo)
summary(m_combo1)

m_combo2 <- lm(impression ~ condition*population*work_gossip + condition*population*family_gossip, combo)
summary(m_combo2)

m_combo3 <- lm(impression ~ population*condition + population*work_gossip + population*family_gossip, combo)
summary(m_combo3)

combo_congruent <-
  combo %>%
  pivot_longer(c(family_gossip, work_gossip), names_to = 'gossip_type', values_to = 'gossip_value') %>%
  mutate(
    congruent = str_detect(gossip_type, condition)
  ) %>%
  dplyr::select(-gossip_type) %>%
  pivot_wider(
    names_from = congruent,
    values_from = gossip_value,
    names_prefix = 'congruent'
  ) %>%
  rename(
    congruent = congruentTRUE,
    incongruent = congruentFALSE
  ) %>%
  mutate(
    impression = scale(impression)[,1],
    congruent = scale(congruent)[,1],
    incongruent = scale(incongruent)[,1],
    total_gossip = congruent + incongruent
  )

m_combo_congruent <- lm(impression ~ population*congruent + population*incongruent, combo_congruent)
summary(m_combo_congruent)
m_combo_z <- signif(z_test(m_combo_congruent, 'congruent', 'incongruent'), 2)

# Aka ---------------------------------------------------------------------

## Aka relatedness ---------------------------------------------------------

rmat <- gen.phi(ped, akagenealogy$egoid)
rmat <- rmat/diag(rmat)
ids <- as.character(aka_ratings$id)
rmat2 <- rmat[ids, ids]
diag(rmat2) <- NA

num_kin <- function(v, r){
  sum(v==r, na.rm = T)
}

total_kin <- apply(rmat2, 1, function(v) sum(v>0, na.rm = T))
kin_r <- apply(rmat2, 1, function(x) mean(x[x>0], na.rm=T))

akagenealogy$participant <- akagenealogy$egoid %in% ids

## Aka GGM -----------------------------------------------------------------

# Kombeti: 1625
# Nganga: 1627

aka_ratings2 <-
  aka_ratings %>%
  left_join(akagenealogy[c('egoid', 'age', 'sex', 'avg_relatedness')], by = c('id' = 'egoid'))

aka_ratings3 <-
  aka_ratings2 %>%
  mutate(sex = ifelse(sex == 'f', 0, 1)) %>%
  dplyr::filter(id != 1625) # Remove Kombeti outlier

aka_vars <- c(
  'sex',
  'age',
  'reputation',
  'give',
  'receive',
  'workhard',
  'parenting',
  'angry',
  'avg_relatedness'
)

aka_cor <- cor(aka_ratings3[aka_vars])
m_aka_qgraph <- EBICglasso(aka_cor, nrow(aka_ratings3), gamma = 0.1, lambda.min.ratio = 0.1)

# Manually reorder circular layout of graph
aka_reorder_qgraph <- c('receive', 'reputation', 'workhard', 'angry', 'age', 'avg_relatedness', 'give', 'parenting', 'sex')
qgraph(
  m_aka_qgraph[aka_reorder_qgraph, aka_reorder_qgraph],
  labels = aka_reorder_qgraph,
  edge.labels=T,
  edge.label.color='black',
  edge.label.bg=F,
  minimum = 0.1,
  label.scale = F,
  layout = "circle",
  filetype = "pdf",
  filename = "Figures/aka_qgraph"
)

plot_cor_aka <-
  ggcorrplot(
    aka_cor,
    hc.order = T,
    hc.method = 'ward.D',
    show.diag = F,
    lab = T,
    colors = scico(3, palette = 'bam')
  )
plot_cor_aka


## Aka PCA -----------------------------------------------------------------

aka_pca <- prcomp(aka_ratings3[aka_vars[-1]], scale. = T) # all vars except sex

# kombeti coordinates
newdata <- aka_ratings2[aka_ratings$id==1625,]
kombeti <- -predict(aka_pca, newdata=newdata)[1:2]

# Flip PC1, PC2
aka_pca$rotation[,1:3] <- -aka_pca$rotation[,1:3]
aka_pca$x[,1:3] <- -aka_pca$x[,1:3]

aka_ratings3$sex <- ifelse(aka_ratings3$sex == 0, 'Female', 'Male')
aka_biplot <-
  autoplot(aka_pca, loadings=T, loadings.label=T, scale = 0, data = aka_ratings3, colour = 'sex', size = 4) +
  geom_hline(yintercept = 0, size = 0.2) +
  geom_vline(xintercept = 0, size = 0.2) +
  scale_color_binary(direction = -1) +
  annotate('point', kombeti[1], kombeti[2], colour='#641A80FF', size = 3) +
  annotate('text', kombeti[1], kombeti[2], label="Kombeti\n(camp leader)") +
  theme_minimal(15)
aka_biplot

aka_loadings_plot <- pca_loadings_plot(aka_pca, components = c(2,1,3)) + guides(colour = guide_none())
aka_loadings_plot

aka_biplot_loadings <- aka_loadings_plot + aka_biplot + plot_layout(ncol=1, heights = c(1,2))

ggsave("Figures/aka_biplot_loadings.pdf", plot = aka_biplot_loadings, width = 10, height = 10)
