# ============================================================================
# graph_crimes.R
# Workshop Part 1: Graph Crimes Tribunal
#
# Eight figures, each committing one or more identifiable crimes.
# Run this script and save each figure to host on a webpage.
# Uncomment the ggsave lines to save individual PNGs.
#
# All figures use the Palmer Penguins dataset.
# Crime 8 uses lterdatasampler (install if needed).
# ============================================================================

library(palmerpenguins)
library(tidyverse)

penguins_clean <- penguins |> drop_na(body_mass_g, flipper_length_mm,
                                       bill_length_mm, bill_depth_mm)


# =========================================================================
# CRIME 1: Truncated bar chart axis (Distortion)
# Y-axis starts at 3000. A modest difference looks enormous.
# =========================================================================

crime_1 <- penguins_clean |>
  group_by(species) |>
  summarise(mean_mass = mean(body_mass_g), .groups = "drop") |>
  ggplot(aes(x = species, y = mean_mass, fill = species)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(3000, 5500), oob = scales::squish) +
  labs(title = "Mean body mass by species", x = "Species", y = "Body mass (g)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

crime_1
# ggsave("crime_01_truncated_axis.png", crime_1, width = 7, height = 6, dpi = 300)


# =========================================================================
# CRIME 2: Pie chart for comparison (Distortion)
# Angle is a poor encoding for comparing quantities.
# =========================================================================

crime_2 <- penguins_clean |>
  count(species) |>
  ggplot(aes(x = "", y = n, fill = species)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  labs(title = "Number of penguins per species", fill = "Species") +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

crime_2
# ggsave("crime_02_pie_chart.png", crime_2, width = 7, height = 6, dpi = 300)


# =========================================================================
# CRIME 3: Dynamite plot (Omission)
# Bar chart of means with SEM error bars. Conceals distributions.
# =========================================================================

crime_3_summary <- penguins_clean |>
  group_by(species) |>
  summarise(mean_fl = mean(flipper_length_mm),
            se = sd(flipper_length_mm) / sqrt(n()), .groups = "drop")

crime_3 <- ggplot(crime_3_summary, aes(x = species, y = mean_fl, fill = species)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_fl - se, ymax = mean_fl + se), width = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(limits = c(0, 230), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Mean flipper length by species",
       x = "Species", y = "Flipper length (mm)") +
  theme_gray(base_size = 14) +
  theme(legend.position = "none")

crime_3
# ggsave("crime_03_dynamite_plot.png", crime_3, width = 7, height = 6, dpi = 300)


# =========================================================================
# CRIME 4: Missing units and incomplete labels (Omission)
# Axes use variable names, no units, no context.
# =========================================================================

crime_4 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(x = "bill_length", y = "depth") +
  theme_minimal(base_size = 14)

crime_4
# ggsave("crime_04_missing_units.png", crime_4, width = 7, height = 6, dpi = 300)


# =========================================================================
# CRIME 5: Dual axes (Distortion)
# Two unrelated variables with independent y-axes.
# The apparent relationship is an artefact of scaling.
# =========================================================================

crime_5_data <- penguins_clean |>
  group_by(species) |>
  summarise(mean_mass = mean(body_mass_g),
            mean_bill = mean(bill_length_mm), .groups = "drop")

sf <- max(crime_5_data$mean_mass) / max(crime_5_data$mean_bill)

crime_5 <- ggplot(crime_5_data, aes(x = species)) +
  geom_col(aes(y = mean_mass), fill = "#457B9D", width = 0.4,
           position = position_nudge(x = -0.2)) +
  geom_col(aes(y = mean_bill * sf), fill = "#E63946", width = 0.4,
           position = position_nudge(x = 0.2)) +
  scale_y_continuous(name = "Body mass (g)",
                     sec.axis = sec_axis(~ . / sf, name = "Bill length (mm)")) +
  labs(title = "Body mass and bill length by species", x = "Species") +
  theme_minimal(base_size = 14) +
  theme(axis.title.y.left = element_text(colour = "#457B9D"),
        axis.title.y.right = element_text(colour = "#E63946"))

crime_5
# ggsave("crime_05_dual_axes.png", crime_5, width = 8, height = 6, dpi = 300)


# =========================================================================
# CRIME 6: Red-green colour scheme, no redundant encoding (Omission)
# Inaccessible to ~8% of males.
# =========================================================================

crime_6 <- penguins_clean |>
  filter(species != "Chinstrap") |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_colour_manual(values = c("red", "green")) +
  labs(title = "Flipper length vs body mass",
       x = "Flipper length (mm)", y = "Body mass (g)", colour = "Species") +
  theme_minimal(base_size = 14)

crime_6
# ggsave("crime_06_red_green.png", crime_6, width = 8, height = 6, dpi = 300)


# =========================================================================
# CRIME 7: Chartjunk (Distortion)
# Grey background, heavy gridlines, garish fills, unnecessary border.
# Every non-data element fails the data-ink test.
# =========================================================================

crime_7 <- penguins_clean |>
  count(island) |>
  ggplot(aes(x = island, y = n, fill = island)) +
  geom_col(width = 0.8, colour = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "PENGUIN COUNTS PER ISLAND", x = "Island", y = "Count") +
  theme(
    plot.background = element_rect(fill = "grey80"),
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major = element_line(colour = "grey60", linewidth = 0.8),
    panel.grid.minor = element_line(colour = "grey70", linewidth = 0.4),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    plot.title = element_text(size = 18, face = "bold", colour = "navy"),
    legend.position = "right",
    legend.background = element_rect(fill = "grey85", colour = "black")
  )

crime_7
# ggsave("crime_07_chartjunk.png", crime_7, width = 8, height = 6, dpi = 300)


# =========================================================================
# CRIME 8: Cherry-picked time window (Distortion)
# Uses lterdatasampler ice phenology data.
# install.packages("lterdatasampler") if needed
# =========================================================================

# library(lterdatasampler)
#
# ice_full <- ntl_icecover |>
#   filter(lakeid == "Lake Mendota") |>
#   mutate(ice_off_doy = yday(ice_off))
#
# # Cherry-picked window with apparent strong trend
# ice_cherry <- ice_full |> filter(year >= 1980, year <= 1998)
#
# crime_8 <- ggplot(ice_cherry, aes(x = year, y = ice_off_doy)) +
#   geom_point(size = 2, alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE, colour = "#E63946") +
#   labs(title = "Ice-off date is getting earlier on Lake Mendota",
#        x = "Year", y = "Ice-off day of year") +
#   theme_minimal(base_size = 14)
#
# crime_8
# # ggsave("crime_08_cherry_picked.png", crime_8, width = 8, height = 6, dpi = 300)
#
# # The fix: full time series with cherry-picked window highlighted
# crime_8_fix <- ggplot(ice_full, aes(x = year, y = ice_off_doy)) +
#   geom_point(size = 1.5, alpha = 0.5) +
#   geom_smooth(method = "lm", se = TRUE, colour = "#457B9D") +
#   annotate("rect", xmin = 1980, xmax = 1998,
#            ymin = -Inf, ymax = Inf, fill = "#E63946", alpha = 0.1) +
#   annotate("text", x = 1989, y = 130, label = "Cherry-picked\nwindow",
#            colour = "#E63946", size = 3.5, fontface = "italic") +
#   labs(title = "Ice-off date, Lake Mendota (full record)",
#        x = "Year", y = "Ice-off day of year") +
#   theme_minimal(base_size = 14)
#
# crime_8_fix
# # ggsave("crime_08_fix.png", crime_8_fix, width = 10, height = 6, dpi = 300)
