# ============================================================================
# workshop_part3.R
# Part 3: From Description to Inference on a Figure
# ============================================================================

library(palmerpenguins)
library(tidyverse)
library(ggdist)
library(emmeans)

penguins_clean <- penguins |> drop_na(body_mass_g)


# =========================================================================
# STAGE 1: THE DESCRIPTIVE FIGURE
# =========================================================================

p <- penguins_clean |>
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  stat_halfeye(
    adjust = 0.5, width = 0.6, .width = 0,
    justification = -0.2, point_colour = NA
  ) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5) +
  geom_jitter(aes(shape = species), width = 0.1, alpha = 0.3, size = 1.2) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(x = "Species", y = "Body mass (g)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

p

# This figure has no model, no annotations, no statistics.
# It shows what the data look like.
#
# Write a figure legend for it AS IT IS NOW.
# Make it complete enough that someone who has never seen this
# dataset could understand everything on the figure.
#
# YOUR LEGEND:
#
#
#
#


# =========================================================================
# STAGE 2: FIT THE MODEL — THEN STOP AND THINK
# =========================================================================

mass_model <- lm(body_mass_g ~ species, data = penguins_clean)

summary(mass_model)

emm <- emmeans(mass_model, specs = pairwise ~ "species")

emm_means <- emm$emmeans

emm_pairs <- emm$contrasts


# STOP HERE. Do not scroll down yet.
#
# Look at the three pairwise comparisons above.
# Look at your figure.
#
# You are preparing this figure for a journal-style lab report.
#
# Write down:
#
# a) Which of these three comparisons would you show ON the figure
#    as an annotation? Why those?
#
# b) Which would you report IN THE LEGEND only? Why?
#
# c) Which, if any, would you leave out entirely? Why?
#
# d) What does the Adelie–Chinstrap comparison tell you biologically,
#    and does the figure already communicate that visually without
#    an annotation?
#
# Compare your answers with the person next to you before continuing.


# =========================================================================
# STAGE 3: TIDY THE CONTRASTS — YOU DECIDE WHAT TO KEEP
# =========================================================================

# This code tidies the emmeans output into a format ggplot2 can use.

annotation_df <- emm_pairs |>
  as_tibble() |>
  separate(contrast, into = c("group1", "group2"), sep = " - ") |>
  mutate(
    stars = case_when(
      p.value < 0.0001 ~ "****",
      p.value < 0.001  ~ "***",
      p.value < 0.01   ~ "**",
      p.value < 0.05   ~ "*",
      TRUE             ~ "ns"
    ),
    p_label = case_when(
      p.value < 0.0001 ~ "p < 0.0001",
      TRUE ~ paste0("p = ", round(p.value, 3))
    )
  )

annotation_df

# All three comparisons are in this table.
# Based on the decision you made in Stage 2, write a filter line
# to keep only the comparisons you want to annotate.

# plot_annotations <- annotation_df |>
#  filter(___)

# Now add bracket positions above the data:

y_max <- max(penguins_clean$body_mass_g)

plot_annotations <- plot_annotations |>
  mutate(y_position = y_max + 300 * row_number())


# =========================================================================
# STAGE 4: ADD YOUR ANNOTATIONS
# =========================================================================

# This code adds brackets and labels for whatever rows are in
# plot_annotations. The figure will reflect the decision you made.

p +
  geom_segment(
    data = plot_annotations,
    aes(x = group1, xend = group2,
        y = y_position, yend = y_position),
    inherit.aes = FALSE, linewidth = 0.4, colour = "grey30"
  ) +
  geom_text(
    data = plot_annotations,
    aes(
      x = (as.numeric(factor(group1, levels = levels(penguins_clean$species))) +
           as.numeric(factor(group2, levels = levels(penguins_clean$species)))) / 2,
      y = y_position + 80,
      label = stars # change this to p.value or p_label
    ),
    inherit.aes = FALSE, size = 4, colour = "grey30"
  )

# =========================================================================
# EXTENSION EXERCISE
# =========================================================================

# The above plot now combines descriptive visuals with inferential statistics
# You could consider re-making this figure with means and 95%CI using geom_pointrange() and the emm_means data


# =========================================================================
# STAGE 5: REWRITE YOUR LEGEND
# =========================================================================

# Your figure now has annotations on it.
# Your Stage 1 legend is no longer complete.
#
# Rewrite it so that a reader encountering this figure in isolation
# can understand everything on it.
#
# YOUR UPDATED LEGEND:
#
#
#
#

# Now swap your legend with the person next to you.
# They read your legend WITHOUT seeing your figure.
#
# Ask them:
#   Can you sketch what the figure looks like from this legend alone?
#
# Where they get stuck is where your legend is incomplete.


# =========================================================================
# STAGE 6: WHAT CHANGED?
# =========================================================================

# Put your two legends side by side.
#
# a) What did you have to ADD to the legend when the annotations
#    appeared?
#
# b) What stayed exactly the same?
#
# c) What is the minimum set of information the annotations forced
#    you to include that was not needed before?
#
# d) If your peer reviewer could not sketch the figure from your
#    legend, what was missing?

# The elements you have just discovered through this process are
# the core of a complete figure legend:
#
#   1. A descriptive sentence — what the figure shows
#   2. The data source and sample sizes
#   3. The statistical model and its result (added in Stage 5)
#   4. The meaning of every annotation (added in Stage 5)
#   5. A definition of every visual component
#
# You arrived at this structure by noticing what was missing.
# That is more reliable than following a checklist — because in
# your own work, there will be no checklist. There will only be
# the question: could someone understand this figure without me?
