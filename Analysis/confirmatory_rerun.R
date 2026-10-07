# confirmatory_rerun.R
# Re-runs the preregistered hypotheses (R/08_hypotheses.R) on the cleaned,
# consistently excluded sample (R/06_exclusions.R, 2026-10-05) and sets each
# estimate beside the value reported in the thesis.
#
# Run after main.R's prep, exclusion and merge steps (01-04, 06, 07), with
# their objects in the session. Output: Results/confirmatory_rerun/.
# ---------------------------------------------------------------

outPath <- file.path(resultsPath, "confirmatory_rerun")
if (!dir.exists(outPath)) dir.create(outPath, recursive = TRUE)

tab_model <- function(...) invisible(NULL)   # 08 prints HTML tables; not needed here
source(file.path(analysisPath, "R", "08_hypotheses.R"))
rm(tab_model)

fe <- function(m, term) {
  s <- summary(m)$coefficients
  if (!term %in% rownames(s)) return(c(b = NA, lo = NA, hi = NA, p = NA))
  ci <- suppressMessages(confint(m, parm = term, method = "Wald"))
  c(b = s[term, 1], lo = ci[1], hi = ci[2], p = s[term, ncol(s)])
}
cr <- function(t) c(b = unname(t$estimate), lo = t$conf.int[1], hi = t$conf.int[2], p = t$p.value)

rows <- list(
  H1A = c(cr(H1a.test), n = unname(H1a.test$parameter) + 2, thesis = "r = -.008, p = .885"),
  H1B = c(cr(H1b.test), n = unname(H1b.test$parameter) + 2, thesis = "r = .043, p = .443"),
  H1E = c(cr(H1e.test), n = unname(H1e.test$parameter) + 2, thesis = "r = -.032, p = .566"),
  H2A = c(cr(H2a.test), n = unname(H2a.test$parameter) + 2, thesis = "r = .020, p = .730"),
  H2B = c(cr(H2b.test), n = unname(H2b.test$parameter) + 2, thesis = "r = -.173, p = .002"),
  H2C = c(cr(H2c.test), n = unname(H2c.test$parameter) + 2, thesis = "r = -.005, p = .930"),
  `H3A Acc+Aware -> accuracy`  = c(fe(H3A.model, "AccelAwareTRUE"), n = length(unique(combData_clean$id)), thesis = "b = -.006, p = .475"),
  `H3B Acc+Aware -> intensity` = c(fe(H3B.model, "AccelAwareTRUE"), n = length(unique(combData_clean$id)), thesis = "b = .033, p = .100"),
  `H3E Acc+Aware -> distance`  = c(fe(H3E.model, "AccelAwareTRUE"), n = length(unique(combData_clean$id)), thesis = "b = .012, p = .637"),
  `H3 expansion: aware -> intensity` = c(fe(H3B.expand, "AwareTRUE"), n = length(unique(combData_clean$id)), thesis = "b = .077, p = .016"),
  `H4A dir x burnout -> accuracy`  = c(fe(H4A.int, "DirectionLabelDec:BATtotal"), n = length(unique(hiSalData$id)), thesis = "b = -.00, p = .111"),
  `H4B dir x burnout -> intensity` = c(fe(H4B.int, "DirectionLabelDec:BATtotal"), n = length(unique(hiSalData$id)), thesis = "b = -.00, p = .900"),
  `Manipulation: salience (low vs high) -> detection, logit` = c(fe(manip_check, "SalienceLow"), n = length(unique(combBCAT_clean$id)), thesis = "salience robust (path model b = .18, p < .001)")
)
tab <- do.call(rbind, lapply(names(rows), function(k) {
  v <- rows[[k]]
  data.frame(test = k, estimate = round(as.numeric(v["b"]), 3),
             ci = sprintf("[%.3f, %.3f]", as.numeric(v["lo"]), as.numeric(v["hi"])),
             p = signif(as.numeric(v["p"]), 3), n = as.integer(as.numeric(v["n"])),
             thesis = v["thesis"], stringsAsFactors = FALSE)
}))
tab$p_BH_correlations <- NA
tab$p_BH_correlations[1:6] <- signif(corr_results$p_BH, 3)
write.csv(tab, file.path(outPath, "confirmatory_vs_thesis.csv"), row.names = FALSE)

flow <- read.csv(participantFlowFile, stringsAsFactors = FALSE)
md <- c("# Confirmatory hypotheses on the cleaned sample", "",
        sprintf("Run %s with the exclusions in `R/06_exclusions.R` (flow below). Thesis values for comparison.", format(Sys.Date())), "",
        "## Participant flow", "", "| Step | Excluded | Remaining | Note |", "|---|---|---|---|",
        sprintf("| %s | %d | %d | %s |", flow$step, flow$excluded, flow$remaining, flow$note), "",
        "## Results", "", "| Test | Estimate | 95% CI | p | p (BH, H1-H2) | n | Thesis |", "|---|---|---|---|---|---|---|",
        sprintf("| %s | %.3f | %s | %s | %s | %d | %s |", tab$test, tab$estimate, tab$ci, format(tab$p),
                ifelse(is.na(tab$p_BH_correlations), "", format(tab$p_BH_correlations)), tab$n, tab$thesis))
# The preregistration applies Benjamini-Hochberg to "all hypotheses (7 tests
# in total)". Seven tests matches H1A, H1B, H2A, H2B, H2C, H3A, H3B (H1E and
# H3E are the exploratory distance outcome; H4 is a moderation test). Shown
# under that reading, with the reading stated.
pre7 <- c("H1A", "H1B", "H2A", "H2B", "H2C", "H3A Acc+Aware -> accuracy", "H3B Acc+Aware -> intensity")
bh7 <- stats::p.adjust(tab$p[match(pre7, tab$test)], method = "BH")
md <- c(md, "", "## Multiplicity", "",
        "The preregistration applies Benjamini-Hochberg across \"all hypotheses (7 tests in total)\". Read as H1A, H1B, H2A, H2B, H2C, H3A and H3B (H1E and H3E use the exploratory distance outcome; H4 is a moderation test), the BH-adjusted p values are:", "",
        "| Test | p | p (BH, 7 tests) |", "|---|---|---|",
        sprintf("| %s | %s | %s |", pre7, format(signif(tab$p[match(pre7, tab$test)], 3)), format(signif(bh7, 3))))
writeLines(md, file.path(outPath, "report.md"))
message("Confirmatory rerun written to ", outPath)
