#### mask ----------------------------------------------------------------------

# import packages
library(tidyverse)
library(readxl)
library(stringdist)

#### import data ---------------------------------------------------------------
# you can download the data from https://drive.google.com/file/d/18SeJTiM2-JXR9SOqEg22wdkvNL3OxG3u/view?usp=sharing

my_theme <- function(){
  theme_bw() +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black", size = 20),
      panel.grid = element_line(colour = "grey"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(size = 15),
      legend.position = "top",
      legend.box = "vertical",
      legend.background = element_rect(fill = "transparent"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 14),
      text = element_text(size = 25),
      legend.key = element_rect(colour = "grey"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )
}

dat <- read_xlsx("data/mask.xlsx") %>% 
  mutate(roman = as.logical(roman),
         romanisation = str_to_lower(romanisation),
         label = paste0(language, ": ", form, " (", romanisation, ")"),
         label_phon =  paste0(language, ": ", form,  " /", ipa_flat, "/"))  %>% 
  drop_na(form) %>% 
  filter(region %in% c("Europe", "Asia", "Africa", "Oceania"), # other regions only provide missing values
         language != "Hmong") %>% # outlier, makes any other scores seem too close to each other (see masks.xlsx)
  as_tibble()

#### orthographic distance -----------------------------------------------------
# generate all pairwise combinations of orthographic forms and compute their Levenshtein distance
expanded_ort <- expand_grid(label1 = dat$label,
                            label2 = dat$label) %>% 
  left_join(., {
    dat %>%
      select(label, romanisation) %>% 
      drop_na(romanisation) %>% 
      rename_all(function(x) paste0(x, "1"))
  }) %>% 
  left_join(., {
    dat %>%
      select(label, romanisation) %>% 
      drop_na(romanisation) %>% 
      rename_all(function(x) paste0(x, "2"))
  }) %>% 
  rowwise() %>% 
  mutate(n = ifelse(nchar(romanisation1) > nchar(romanisation2), nchar(romanisation1), nchar(romanisation2)),
         lv = stringdist(romanisation1, romanisation2, method = "lv"),
         lv_norm = lv/n) %>% 
  ungroup() 

# plot data
ggplot(expanded_ort, aes(fct_reorder(label1, lv_norm),
                         fct_reorder(label2, lv_norm),
                         fill = lv_norm)) +
  geom_tile() +
  labs(fill = "Levenshtein distance",
       title = "Orthographic distance between romanisations of translations of MASK",
       subtitle = paste0("N = ", sum(!is.na(dat$romanisation), na.rm = TRUE)),
       caption = "Levenshtein distance: Number of insertions, deletions, and replacements a character string\nhas to go trough to be identical to other | @gongcastro") +
  scale_fill_distiller(palette = "Blues") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(colour = "black"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, "cm")) +
  ggsave("static/img/masks_ort.png", height = 18, width = 15)

#### phonological distance -----------------------------------------------------
# generate all pairwise combinations of phonological forms and compute their Levenshtein distance
expanded_phon <- expand_grid(label_phon1 = dat$label_phon, label_phon2 = dat$label_phon) %>% 
  left_join(., {
    dat %>%
      select(label_phon, ipa_flat) %>% 
      drop_na(ipa_flat) %>% 
      rename_all(function(x) paste0(x, "1"))
  }) %>% 
  left_join(., {
    dat %>%
      select(label_phon, ipa_flat) %>% 
      drop_na(ipa_flat) %>% 
      rename_all(function(x) paste0(x, "2"))
  }) %>% 
  rowwise() %>% 
  mutate(n = ifelse(nchar(ipa_flat1) > nchar(ipa_flat2), nchar(ipa_flat1), nchar(ipa_flat2)),
         lv_phon = stringdist(ipa_flat1, ipa_flat2, method = "lv"),
         lv_phon_norm = lv_phon/n) %>% 
  ungroup() %>% 
  drop_na(lv_phon_norm)

# plot data
ggplot(expanded_phon, aes(fct_reorder(label_phon1, lv_phon_norm),
                          fct_reorder(label_phon2, lv_phon_norm),
                          fill = lv_phon_norm)) +
  geom_tile() +
  labs(fill = "Levenshtein distance",
       title = "Phonological distance between IPA transcriptions of translations of MASK",
       subtitle = paste0("N = ", sum(!is.na(dat$ipa_flat), na.rm = TRUE)),
       caption = "Levenshtein distance: Number of insertions, deletions, and replacements a character string\nhas to go trough to be identical to other | @gongcastro") +
  scale_fill_distiller(palette = "YlOrRd") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(colour = "black"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.5, "cm")) +
  ggsave("static/img/masks_phon.png", height = 18, width = 15)

#### onset ---------------------------------------------------------------------
# compute how many orthographic/phonological forms share onset
onsets <- list(
  Orthography = dat %>% 
    mutate(onset = as.factor(substr(romanisation, 1, 1))) %>% 
    count(onset) %>%  
    mutate(prop = n/sum(.$n)),
  Phonology =  dat %>% 
    drop_na(ipa_flat) %>% 
    mutate(onset = as.factor(substr(ipa_flat, 1, 1))) %>% 
    count(onset) %>%  
    mutate(prop = n/sum(.$n),
           onset = paste0("/", onset, "/"))
) %>% 
  bind_rows(.id = "type") %>% 
  mutate(type = ifelse(
    type=="Orthography",
    paste0(type, " (N = ", sum(!is.na(dat$romanisation), na.rm = TRUE), ")"),
    paste0(type, " (N = ", sum(!is.na(dat$ipa_flat), na.rm = TRUE), ")")
  )) %>% 
  group_by(type) %>% 
  arrange(desc(prop)) %>%  
  mutate(index = row_number()) %>% 
  ungroup()

# plot data
ggplot(onsets, aes(x = fct_reorder(onset, prop, .desc = TRUE),
                   y = prop,
                   order = -prop,
                   label = n,
                   fill = index)) +
  facet_wrap(~type, scales = "free", dir = "v") +
  geom_col(colour = "white", width = 1, show.legend = FALSE) +
  geom_text(nudge_y = 0.025) +
  scale_fill_distiller(palette = "YlOrRd") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Onset",
       y = "%",
       fill = "Region",
       title = "Shared onset",
       subtitle = "How often do translations of MASK start with the same letter/phoneme?",
       caption = "@gongcastro") +
  my_theme() +
  theme(axis.text.x = element_text(colour = "black", face = "bold", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "grey", colour = NA),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        plot.caption.position = "plot",
        plot.subtitle = element_text(size = 15),
        plot.title = element_text(face = "bold", size = 20),
        plot.caption = element_text(hjust = 0)) +
  ggsave("static/img/masks_onset.png", width = 6, height = 6)
