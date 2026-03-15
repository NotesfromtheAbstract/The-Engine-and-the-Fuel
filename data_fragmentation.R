library(ggplot2)
library(dplyr)

theme_1950s <- function() {
  theme_minimal(base_family = "Arial", base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.grid.major = element_line(color = "#E6DECA", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "#3A3A3A"),
      axis.text = element_text(color = "#4A4A4A"),
      plot.title = element_text(face = "bold", size = 20, color = "#3A3A3A"),
      plot.subtitle = element_text(size = 13, color = "#5A5A5A"),
      plot.margin = margin(15, 15, 15, 15),
      legend.position = "none"
    )
}

# ── Layout ──
# Three horizontal swim lanes:
#   y=3: Clinical encounters (top)
#   y=2: What Copilot Health sees (middle)
#   y=1: Wearable data (bottom)
# x-axis: months (1-18)

# Colors
col_connected <- "#2471A3"   # data that made it through
col_orphaned  <- "#C0392B"   # data that got lost
col_wearable  <- "#D4A94B"   # wearable data (different evidentiary tier)
col_faint     <- "#D5CFC0"   # background lane lines

# ── Clinical encounters ──
clinical <- data.frame(
  month = c(1, 4, 7, 10, 13, 16),
  label = c("T2D diagnosis\nE11.9\nStamford Clinic",
            "A1C lab\n7.2%\nStamford Clinic",
            "Retinal screening\nDiabetic retinopathy\nAbilene ophthalmologist",
            "A1C lab\n7.8%\nHendrick, Abilene",
            "Telehealth visit\nSwitch to SGLT2i\nRegional telehealth",
            "A1C lab\n8.1%\nStamford Clinic"),
  system = c("Stamford", "Stamford", "Abilene", "Abilene", "Telehealth", "Stamford"),
  y = 3,
  stringsAsFactors = FALSE
)

# Which of these made it to Stamford's record?
clinical$reached_stamford <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)

# ── What Copilot Health sees (Stamford record) ──
copilot_sees <- data.frame(
  month = c(1, 4, 16),
  label = c("E11.9\nT2D, no complications", "A1C: 7.2%", "A1C: 8.1%"),
  y = 2,
  stringsAsFactors = FALSE
)

# ── What Copilot Health misses ──
copilot_misses <- data.frame(
  month = c(7, 10, 13),
  label = c("Retinopathy\nfinding", "A1C: 7.8%\n(different lab)", "Med change\nto SGLT2i"),
  y = 2,
  stringsAsFactors = FALSE
)

# ── Wearable data stream ──
wearable_events <- data.frame(
  month = c(3, 6, 9, 12, 14, 15),
  label = c("Sleep\ndeclining", "HRV\ndrop", "Sleep\npoor", "Irregular\nrhythm alert", "Temp\nelevated", "Irregular\nrhythm x2"),
  y = 1,
  stringsAsFactors = FALSE
)

# ── Build the plot ──
p <- ggplot() +
  # Swim lane backgrounds
  annotate("rect", xmin = 0.2, xmax = 17.8, ymin = 2.6, ymax = 3.4, fill = "#F3EDE0", alpha = 0.5) +
  annotate("rect", xmin = 0.2, xmax = 17.8, ymin = 1.6, ymax = 2.4, fill = "#EDE8DA", alpha = 0.5) +
  annotate("rect", xmin = 0.2, xmax = 17.8, ymin = 0.6, ymax = 1.4, fill = "#F3EDE0", alpha = 0.5) +

  # Swim lane labels on the left
  annotate("text", x = 0.1, y = 3, label = "Clinical\nencounters", hjust = 1, size = 4.0,
           fontface = "bold", color = "#3A3A3A", family = "Arial", lineheight = 0.9) +
  annotate("text", x = 0.1, y = 2, label = "What\nCopilot\nHealth sees", hjust = 1, size = 4.0,
           fontface = "bold", color = "#3A3A3A", family = "Arial", lineheight = 0.9) +
  annotate("text", x = 0.1, y = 1, label = "Wearable\ndata", hjust = 1, size = 4.0,
           fontface = "bold", color = "#3A3A3A", family = "Arial", lineheight = 0.9) +

  # ── Connections: clinical events that DID reach Stamford's record ──
  # Diagnosis (month 1) -> visible in Copilot
  geom_segment(aes(x = 1, y = 2.95, xend = 1, yend = 2.05),
               color = col_connected, linewidth = 0.6, linetype = "solid",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  # A1C month 4 -> visible
  geom_segment(aes(x = 4, y = 2.95, xend = 4, yend = 2.05),
               color = col_connected, linewidth = 0.6, linetype = "solid",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  # A1C month 16 -> visible
  geom_segment(aes(x = 16, y = 2.95, xend = 16, yend = 2.05),
               color = col_connected, linewidth = 0.6, linetype = "solid",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +

  # ── Orphaned connections: clinical events that did NOT reach Stamford ──
  geom_segment(aes(x = 7, y = 2.95, xend = 7, yend = 2.05),
               color = col_orphaned, linewidth = 0.6, linetype = "dashed",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  geom_segment(aes(x = 10, y = 2.95, xend = 10, yend = 2.05),
               color = col_orphaned, linewidth = 0.6, linetype = "dashed",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  geom_segment(aes(x = 13, y = 2.95, xend = 13, yend = 2.05),
               color = col_orphaned, linewidth = 0.6, linetype = "dashed",
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +

  # ── Wearable data: never connects to clinical lane ──
  # (no arrows from y=1 to y=2, that IS the point)

  # ── Clinical encounter points ──
  geom_point(data = clinical, aes(x = month, y = y),
             size = 5, shape = 16,
             color = ifelse(clinical$reached_stamford, col_connected, col_orphaned)) +
  # Clinical labels
  geom_text(data = clinical, aes(x = month, y = y + 0.32, label = label),
            size = 3.2, family = "Arial", lineheight = 0.85,
            color = ifelse(clinical$reached_stamford, col_connected, col_orphaned)) +

  # ── Copilot sees (blue) ──
  geom_point(data = copilot_sees, aes(x = month, y = y),
             size = 4, shape = 15, color = col_connected) +
  geom_text(data = copilot_sees, aes(x = month, y = y - 0.25, label = label),
            size = 3.0, family = "Arial", lineheight = 0.85, color = col_connected) +

  # ── Copilot misses (red, with X) ──
  geom_point(data = copilot_misses, aes(x = month, y = y),
             size = 4, shape = 4, color = col_orphaned, stroke = 1.5) +
  geom_text(data = copilot_misses, aes(x = month, y = y - 0.25, label = label),
            size = 3.0, family = "Arial", lineheight = 0.85, color = col_orphaned) +

  # ── Wearable events (gold diamonds) ──
  geom_point(data = wearable_events, aes(x = month, y = y),
             size = 3.5, shape = 18, color = col_wearable) +
  geom_text(data = wearable_events, aes(x = month, y = y - 0.25, label = label),
            size = 2.8, family = "Arial", lineheight = 0.85, color = col_wearable) +

  # ── Legend ──
  # Connected
  annotate("point", x = 1, y = 0.35, shape = 16, color = col_connected, size = 3) +
  annotate("text",  x = 1.3, y = 0.35, label = "Data reached Stamford record",
           hjust = 0, size = 3.5, color = "#3A3A3A", family = "Arial") +
  # Orphaned
  annotate("point", x = 7, y = 0.35, shape = 4, color = col_orphaned, size = 3, stroke = 1.5) +
  annotate("text",  x = 7.3, y = 0.35, label = "Data lost between systems",
           hjust = 0, size = 3.5, color = "#3A3A3A", family = "Arial") +
  # Wearable
  annotate("point", x = 12.5, y = 0.35, shape = 18, color = col_wearable, size = 3.5) +
  annotate("text",  x = 12.8, y = 0.35, label = "Wearable data (never entered clinical record)",
           hjust = 0, size = 3.5, color = "#3A3A3A", family = "Arial") +

  # Scales
  scale_x_continuous(
    breaks = c(1, 4, 7, 10, 13, 16),
    labels = c("Month 1", "Month 4", "Month 7", "Month 10", "Month 13", "Month 16"),
    limits = c(-1.8, 18.5)
  ) +
  scale_y_continuous(limits = c(0.1, 3.8)) +
  labs(
    title = "The Data You Don't Have",
    subtitle = "18 months of Type 2 diabetes management in Stamford, Texas. Three systems. One patient. Gaps everywhere.",
    x = NULL, y = NULL,
    caption = "Copyright (c) 2026 Andrew R. Crocker. All rights reserved.   |   Scenario is illustrative, based on documented interoperability patterns in rural Texas.\nThe AI reads what is there and treats gaps as absence rather than failure."
  ) +
  theme_1950s() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 11, color = "#5A5A5A"),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#8A8A8A", hjust = 0, margin = margin(t = 10), lineheight = 1.2),
    plot.subtitle = element_text(size = 11.5, color = "#5A5A5A", margin = margin(b = 12))
  )

ggsave("/home/claude/data_fragmentation.png", p, width = 1456/100, height = 900/100, dpi = 100, bg = "#FAF5E9")
cat("Chart 3 saved.\n")
