library(tidyverse)
library(broom)
library(visreg)

# Z-test ------------------------------------------------------------------

z_test <- function(m, term1, term2){
  stats <- tidy(m)
  b1 <- stats$estimate[stats$term == term1]
  SEb1 <- stats$std.error[stats$term == term1]
  b2 <- stats$estimate[stats$term == term2]
  SEb2 <- stats$std.error[stats$term == term2]
  z <- (b1 - b2)/sqrt(SEb1^2 + SEb2^2)
  p <- 2 * pnorm(-abs(z))
  c(b1=b1, b2=b2, Z=z, p=p)
}

# Gossip domain plot ------------------------------------------------------

gossip_domain_plot <- function(m, condition, d){
  if (condition == 'work'){
    xvar = 'work_gossip'
    xlab = '\nWork gossip score\n(Z-score)'
    ylab = 'Impression (Z-score)'
  } else {
    xvar = 'family_gossip'
    xlab = '\nFamily gossip score\n(Z-score)'
    ylab = ''
  }
  visreg(m, xvar = xvar, by = 'condition', plot = F)$fit %>%
    ggplot(aes_string(xvar, 'visregFit', ymin = 'visregLwr', ymax = 'visregUpr', colour = 'condition', fill = 'condition')) +
    geom_line() +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_jitter(data=d, aes_string(x=xvar, y='impression', colour='condition'), inherit.aes = F, width = 0.05, height = 0.05, alpha = 0.5) +
    scale_colour_binary() +
    scale_fill_binary() +
    ylim(-2, 2) +
    labs(x = xlab, y = ylab) +
    theme_minimal(15)
  # theme(axis.title.y = element_text(angle = 0))
}

# Standardize subset ------------------------------------------------------

scale_subset <- function(d, cond){
  d %>%
    dplyr::filter(condition==cond) %>%
    dplyr::select(impression, family_gossip, work_gossip) %>%
    scale() %>%
    as_tibble()
}
