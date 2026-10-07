# R/10_figures.R
# Generates and saves all publication figures.
# ---------------------------------------------------------------

message("10 | Generating figures...")

fig1 <- plot_scatter(bothBaseQ_data, "baseThresh", "basegertAccuracy",
                     x_label = "Interoceptive Detection Threshold (BCAT)",
                     y_label = "Emotion Recognition Accuracy (GERT, baseline)",
                     color = "#2E4A7A", alpha = 0.60, point_style = "point")
ggsave(file.path(resultsPath, "Figure1_BCAT_threshold_vs_GERT_accuracy.png"),
       fig1, height = 4, width = 6, units = "in", dpi = 300)

fig2 <- plot_scatter(traitData, "BATtotal", "combinedArousal",
                     x_label = "Burnout Severity (BAT-C Total)",
                     y_label = "Perceived Arousal (Combined Task)",
                     color = "#2E4A7A", alpha = 0.60, point_style = "point")
ggsave(file.path(resultsPath, "Figure2_burnout_vs_combined_arousal.png"),
       fig2, height = 4, width = 6, units = "in", dpi = 300)

fig3 <- ggplot(fullData, aes(x = bcatAccuracy, y = Arousal, colour = DirectionLabel)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  scale_colour_manual(values = c("Acc" = "#2C7BB6", "Dec" = "#D7191C"),
                      labels = c("Acc" = "Acceleration", "Dec" = "Deceleration"),
                      name   = "Respiration") +
  labs(x = "BCAT Detection Accuracy", y = "Perceived Arousal") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank())
ggsave(file.path(resultsPath, "Figure3_BCAT_accuracy_x_direction_on_arousal.png"),
       fig3, height = 4, width = 6, units = "in", dpi = 300)

fig4 <- plot_scatter(fullData, "Arousal", "emoIntensity",
                     x_label = "Perceived Arousal (BCAT trial)",
                     y_label = "Perceived Emotion Intensity (GERT)",
                     color = "#2E4A7A", alpha = 0.08, point_style = "none")
ggsave(file.path(resultsPath, "Figure4_arousal_vs_GERT_intensity.png"),
       fig4, height = 4, width = 6, units = "in", dpi = 300)

cm <- build_confusion_matrix(
  dplyr::bind_rows(
    dplyr::select(gertBase_clean, emoResponse, vidCorrect),
    dplyr::select(combGERT_clean, emoResponse, vidCorrect)
  )
)
ggsave(file.path(resultsPath, "Figure5_GERT_confusion_matrix.png"),
       cm$plot, height = 7, width = 8, units = "in", dpi = 300)

png(file.path(resultsPath, "Supplement_correlation_matrix.png"),
    width = 3000, height = 3000, res = 300)
corrplot(cmat, method = "circle", order = "hclust",
         tl.cex = 0.5, tl.col = "black")
dev.off()

message("10 | Figures saved to: ", resultsPath)
