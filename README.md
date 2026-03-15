# The Fuel and the Engine

**Notes from the Abstract** | Andrew R. Crocker | March 2026

## About

This piece examines the health data infrastructure that AI health products like Microsoft Copilot Health, ChatGPT Health, Amazon Health AI, and Claude for Healthcare are about to land on. Rather than reviewing the products themselves (none have been in the wild long enough to evaluate), it surveys the terrain: coding quality at rural Critical Access Hospitals, regulatory gaps in consumer health data protection, and the fragmentation of health records in communities where these tools are needed most.

The analysis is grounded in Stamford, Texas, a town of nearly 3,000 people whose hospital closed in July 2018, leaving Jones County without a hospital.

## Contents

```
article/
  copilot_health_draft.md   — Full article draft (~3,100 words)

charts/
  stamford_map.png          — Healthcare desert map around Stamford, TX
  cah_quality_gap.png       — Texas CAH hospitals without HCAHPS star ratings
  regulatory_comparison.png — AI health product regulatory comparison matrix
  data_fragmentation.png    — T2D data fragmentation timeline (illustrative)

R/
  stamford_map.R            — Schematic distance map using verified driving distances
  cah_quality_gap.R         — Bar chart of CAH quality visibility gap
  regulatory_comparison.R   — Matrix comparison of four AI health products
  data_fragmentation.R      — Swim lane timeline of data fragmentation
```

## Data Sources

- CMS Hospital General Information (2024)
- CMS HCAHPS Patient Survey Results
- Company announcements and documentation (Microsoft, OpenAI, Amazon, Anthropic) as of March 2026
- Driving distances from distance-cities.com and withinhours.com
- KTXS News (Stamford hospital closure, 2018)
- Oxford Internet Institute / Nature Medicine (AI chatbot medical advice study, February 2026)
- Microsoft Research: "How People Use Copilot for Health" (March 2026)

## R Dependencies

- ggplot2
- ggrepel
- dplyr
- scales

All charts use a custom `theme_1950s()` function defined within each script.
Charts sized at 1456px wide for Substack's maximum post width.

## License

Copyright (c) 2026 Andrew R. Crocker. All rights reserved.
See LICENSE file for details.
