library(ggplot2)

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

# ── Data ──
# 92 Texas Critical Access Hospitals
# 86 lack HCAHPS star ratings (no public quality signal)
# 6 have HCAHPS star ratings

df <- data.frame(
  category = c("No HCAHPS\nstar rating", "Has HCAHPS\nstar rating"),
  count = c(86, 6),
  pct = c(86/92 * 100, 6/92 * 100),
  stringsAsFactors = FALSE
)

df$category <- factor(df$category, levels = c("No HCAHPS\nstar rating", "Has HCAHPS\nstar rating"))

col_no_rating <- "#C0392B"
col_has_rating <- "#2471A3"

p <- ggplot(df, aes(x = category, y = count)) +
  geom_col(
    fill = c(col_no_rating, col_has_rating),
    width = 0.55
  ) +
  # Count labels on bars
  geom_text(
    aes(label = paste0(count, " hospitals\n(", round(pct), "%)")),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    color = c(col_no_rating, col_has_rating),
    family = "Arial"
  ) +
  # Annotation explaining why this matters
  annotate(
    "text",
    x = 2.0, y = 55,
    label = "When Copilot Health pulls records\nfrom these facilities, neither the\npatient nor the AI can evaluate the\nquality of the documentation underneath.",
    size = 3.8, color = "#5A5A5A", family = "Arial",
    lineheight = 1.3, hjust = 0.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "The Quality Signal That Doesn't Exist",
    subtitle = "Of 92 Texas Critical Access Hospitals, 86 have no publicly available HCAHPS star rating",
    x = NULL,
    y = "Number of hospitals",
    caption = "Copyright (c) 2026 Andrew R. Crocker. All rights reserved.   |   Data: CMS Hospital General Information (2024)\nHCAHPS reporting requirements differ for CAHs. Many lack the volume or staffing to participate."
  ) +
  theme_1950s() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 13, face = "bold", color = "#3A3A3A"),
    plot.caption = element_text(size = 9, color = "#8A8A8A", hjust = 0, margin = margin(t = 10)),
    plot.subtitle = element_text(size = 12, color = "#5A5A5A", margin = margin(b = 15))
  )

ggsave("/home/claude/cah_quality_gap.png", p, width = 1456/100, height = 816/100, dpi = 100, bg = "#FAF5E9")
cat("Chart 1 saved.\n")
