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

# ── Build data explicitly row by row to avoid mapping errors ──
df <- data.frame(
  product = c(
    # Amazon Health AI
    "Amazon\nHealth AI", "Amazon\nHealth AI", "Amazon\nHealth AI", "Amazon\nHealth AI", "Amazon\nHealth AI",
    # ChatGPT Health
    "ChatGPT\nHealth", "ChatGPT\nHealth", "ChatGPT\nHealth", "ChatGPT\nHealth", "ChatGPT\nHealth",
    # Copilot Health
    "Microsoft\nCopilot Health", "Microsoft\nCopilot Health", "Microsoft\nCopilot Health", "Microsoft\nCopilot Health", "Microsoft\nCopilot Health",
    # Claude for Healthcare
    "Claude for\nHealthcare", "Claude for\nHealthcare", "Claude for\nHealthcare", "Claude for\nHealthcare", "Claude for\nHealthcare"
  ),
  dimension = rep(c(
    "HIPAA-compliant\nenvironment",
    "Clinical escalation\nto providers",
    "Excludes health data\nfrom model training",
    "Free tier\navailable",
    "FDA-reviewed\nmedical device"
  ), 4),
  value = c(
    # Amazon:   HIPAA=Yes, ClinEsc=Yes, NoTrain=Partial, Free=Yes, FDA=No
    "Yes", "Yes", "Partial", "Yes", "No",
    # ChatGPT:  HIPAA=No, ClinEsc=No, NoTrain=Yes, Free=Yes, FDA=No
    "No", "No", "Yes", "Yes", "No",
    # Copilot:  HIPAA=No, ClinEsc=No, NoTrain=Yes, Free=TBD, FDA=No
    "No", "No", "Yes", "TBD", "No",
    # Claude:   HIPAA=Partial(enterprise), ClinEsc=No, NoTrain=Yes, Free=No, FDA=No
    "Partial", "No", "Yes", "No", "No"
  ),
  stringsAsFactors = FALSE
)

# Set factor levels for display order
df$product <- factor(df$product, levels = c("ChatGPT\nHealth", "Microsoft\nCopilot Health", "Claude for\nHealthcare", "Amazon\nHealth AI"))
df$dimension <- factor(df$dimension, levels = c(
  "HIPAA-compliant\nenvironment",
  "Clinical escalation\nto providers",
  "Excludes health data\nfrom model training",
  "Free tier\navailable",
  "FDA-reviewed\nmedical device"
))

# Verify
cat("Data check:\n")
for (i in 1:nrow(df)) {
  cat(sprintf("  %-25s | %-30s | %s\n", gsub("\n"," ",df$product[i]), gsub("\n"," ",df$dimension[i]), df$value[i]))
}

# Colors
col_yes     <- "#2471A3"
col_partial <- "#D4A94B"
col_no      <- "#C0392B"
col_tbd     <- "#8A8A8A"

df$fill <- case_when(
  df$value == "Yes"     ~ col_yes,
  df$value == "Partial" ~ col_partial,
  df$value == "TBD"     ~ col_tbd,
  TRUE                  ~ col_no
)

df$symbol <- case_when(
  df$value == "Yes"     ~ "\u2713",
  df$value == "No"      ~ "\u2717",
  df$value == "TBD"     ~ "?",
  df$value == "Partial" ~ "~"
)

# Qualifier text for partial/TBD cells
df$qualifier <- case_when(
  df$product == "Amazon\nHealth AI" & df$dimension == "Excludes health data\nfrom model training" ~ "Trains on\nabstracted patterns",
  df$product == "Claude for\nHealthcare" & df$dimension == "HIPAA-compliant\nenvironment" ~ "Enterprise\nonly",
  df$product == "Microsoft\nCopilot Health" & df$dimension == "Free tier\navailable" ~ "Subscription\nplanned",
  TRUE ~ ""
)

p <- ggplot(df, aes(x = dimension, y = product)) +
  geom_tile(fill = "#FAF5E9", color = "#E6DECA", linewidth = 0.5) +
  geom_point(aes(color = fill), size = 14, shape = 16) +
  scale_color_identity() +
  geom_text(aes(label = symbol), size = 7, color = "white", fontface = "bold", family = "Arial") +
  # Qualifier labels
  geom_text(
    data = df %>% filter(qualifier != ""),
    aes(label = qualifier),
    vjust = 3.4, size = 3.0, color = "#5A5A5A", family = "Arial", lineheight = 0.85
  ) +
  # Legend
  annotate("point", x = 1.5, y = 0.28, shape = 16, color = col_yes, size = 4) +
  annotate("text",  x = 1.7, y = 0.28, label = "Yes", hjust = 0, size = 3, color = "#3A3A3A", family = "Arial") +
  annotate("point", x = 2.2, y = 0.28, shape = 16, color = col_partial, size = 4) +
  annotate("text",  x = 2.4, y = 0.28, label = "Partial / Conditional", hjust = 0, size = 3, color = "#3A3A3A", family = "Arial") +
  annotate("point", x = 3.5, y = 0.28, shape = 16, color = col_no, size = 4) +
  annotate("text",  x = 3.7, y = 0.28, label = "No", hjust = 0, size = 3, color = "#3A3A3A", family = "Arial") +
  annotate("point", x = 4.1, y = 0.28, shape = 16, color = col_tbd, size = 4) +
  annotate("text",  x = 4.3, y = 0.28, label = "TBD", hjust = 0, size = 3, color = "#3A3A3A", family = "Arial") +
  coord_cartesian(clip = "off", ylim = c(0.5, 4.5)) +
  labs(
    title = "The HIPAA Costume Party",
    subtitle = "What protections do AI health products actually provide? Only Amazon claims HIPAA compliance for consumers.",
    x = NULL, y = NULL,
    caption = "Copyright (c) 2026 Andrew R. Crocker. All rights reserved.   |   Sources: Company announcements and documentation as of March 2026\nAmazon trains Health AI on abstracted patterns without identifying information, per HIPAA permitted uses. Claude offers HIPAA-ready tools for enterprise clients."
  ) +
  theme_1950s() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold", color = "#3A3A3A", lineheight = 1.1),
    axis.text.y = element_text(size = 12, face = "bold", color = "#3A3A3A", lineheight = 1.1),
    plot.caption = element_text(size = 9, color = "#8A8A8A", hjust = 0, margin = margin(t = 15), lineheight = 1.3),
    plot.subtitle = element_text(size = 12, color = "#5A5A5A", margin = margin(b = 15)),
    plot.margin = margin(15, 20, 40, 15)
  )

ggsave("/home/claude/regulatory_comparison.png", p, width = 1456/100, height = 816/100, dpi = 100, bg = "#FAF5E9")
cat("Chart 2 saved.\n")
