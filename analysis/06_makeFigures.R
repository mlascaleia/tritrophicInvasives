# this script makes the paper figures from the models in 0203 and 05
# goodness I love having fast-running models (trauma from chapter 2 will be revisited)

# sections:
#   0. initialize
#   1. helpers (split violin, p-value labels, panel data, panel plot, model curves)
#   2. bottom-up figure: growth efficiency, pupation, pupal weight -> figures/bigOne_bu.png
#   3. mortality figure: death from all causes -> figures/mortality.png
#   4. top-down figure: parasitism, pan traps, clay decoys -> figures/bigOne_td.png
#   5. network figure (bipartite)
#   6. caterpillar tables + checks -> figures/catTable.csv, figures/catTable2.csv
#   7. presentation versions (black backgrounds, all commented out)

# 0. initialize ####

rm(list = ls())
source("analysis/05_makePanTrapModels.R")

library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggpp)
library(RColorBrewer)
library(ggtext)
library(ggbeeswarm)
library(emmeans)

# family order for every panel (Caprifoliaceae is the baseline family in the models)
fam.levels <- c("Overall", "Caprifoliaceae", "Oleaceae", "Rosaceae")

# native, exotic
cols.ne <- c("#9CB380", "#586A6A")

# 1. helpers ####

# split violin ####

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# p-value labels ####

# "*p* < 0.001" or "*p* = 0.012"
fmt_p <- function(p) {
  ifelse(p < 0.001, "*p* < 0.001", paste0("*p* = ", round(p, 3)))
}

# overall native vs exotic p from emmeans
overall_p <- function(model) {
  as.data.frame(emmeans(model, pairwise ~ hostNative)$contrasts)$p.value
}

# per-family p's from glht, unadjusted
fam_p <- function(gl) {
  summary(gl, test = adjusted(type = "none"))$test$pvalues
}

# labels in fam.levels order; families blank when there's no gl (additive model won)
make_siglab <- function(model, gl = NULL) {
  fam <- if (is.null(gl)) rep(NA, 3) else fam_p(gl)
  p <- c(overall_p(model), fam)
  data.frame(hostFamily = factor(fam.levels, levels = fam.levels),
             pValue = ifelse(is.na(p), "", fmt_p(p)))
}

# panel data ####
# species-centered response (each value minus its caterpillar species' mean),
# stacked with an "Overall" copy, plus mean +/- 1.96 SE by family x origin,
# plus the p-value labels from the model
# gl = NULL for models where the additive version won

fig_data <- function(df, yvar, model, gl = NULL) {
  pts <- df %>%
    group_by(catSpecies) %>%
    mutate(resid = .data[[yvar]] - mean(.data[[yvar]])) %>%
    ungroup() %>%
    mutate(hostFamily = as.character(hostFamily))
  
  pts <- pts %>%
    mutate(hostFamily = "Overall") %>%
    rbind(pts) %>%
    mutate(hostFamily = factor(hostFamily, levels = fam.levels),
           hostNative = factor(hostNative, levels = c("native", "exotic")))
  
  summ <- pts %>%
    group_by(hostFamily, hostNative) %>%
    summarise(n = n(),
              rate = mean(resid),
              se = sd(resid)/sqrt(n()),
              .groups = "drop") %>%
    mutate(q95 = rate + (1.96 * se),
           q05 = rate - (1.96 * se))
  
  list(pts = pts, summ = summ, siglab = make_siglab(model, gl))
}

# panel plot ####
# violin = TRUE for continuous responses (split violins + raw points + black bars)
# violin = FALSE for binary responses (colored bars only)
# panel-specific tweaks (axis position, axis text, titles) get added after

plot_fig <- function(fd, ylab, violin = FALSE, violin.width = .9, lab.y = NULL) {
  
  # p-value labels sit just above the highest thing in the panel
  if (is.null(lab.y)) {
    yr <- if (violin) range(fd$pts$resid) else range(c(fd$summ$q05, fd$summ$q95))
    lab.y <- yr[2] + 0.12 * diff(yr)
  }
  fd$siglab$y <- lab.y
  
  p <- ggplot(data = fd$summ, aes(x = hostFamily, y = rate, group = hostNative)) +
    geom_hline(aes(yintercept = 0), color = "grey20", alpha = 1) +
    geom_vline(aes(xintercept = 1.5), color = "black",
               alpha = 1, linewidth = 1.5)
  
  if (violin) {
    p <- p +
      geom_split_violin(data = fd$pts,
                        aes(x = hostFamily, y = resid, fill = hostNative),
                        inherit.aes = F,
                        drop = F, width = violin.width, alpha = .8) +
      geom_point(data = fd$pts[fd$pts$hostNative == "native", ],
                 aes(x = hostFamily, y = resid), inherit.aes = F,
                 position = position_jitternudge(width = 0.1,
                                                 seed = 1234, x = -0.15,
                                                 nudge.from = "jittered"),
                 color = "darkgreen", alpha = .3, size = 1.2) +
      geom_point(data = fd$pts[fd$pts$hostNative == "exotic", ],
                 aes(x = hostFamily, y = resid), inherit.aes = F,
                 position = position_jitternudge(width = 0.1,
                                                 seed = 1234, x = 0.15,
                                                 nudge.from = "jittered"),
                 color = "darkblue", alpha = .3, size = 1.2) +
      geom_errorbar(aes(ymin = q05, ymax = q95),
                    width = .3, linewidth = 1.2,
                    position = position_dodge(width = .5)) +
      geom_point(size = 3,
                 position = position_dodge(width = .5))
  } else {
    p <- p +
      geom_errorbar(aes(ymin = q05, ymax = q95, color = hostNative),
                    width = .4, linewidth = 1.2,
                    position = position_dodge(width = .5)) +
      geom_point(position = position_dodge(width = .5),
                 aes(color = hostNative), size = 3)
  }
  
  p +
    geom_richtext(data = fd$siglab,
                  aes(y = y, x = hostFamily,
                      label = pValue),
                  inherit.aes = F, label.size = NA,
                  fill = NA, family = "serif", size = 5) +
    theme_tufte() +
    ylab(ylab) +
    scale_fill_manual(values = cols.ne,
                      name = "Hostplant\norigin",
                      labels = c("Native", "Exotic")) +
    scale_color_manual(values = cols.ne) +
    theme(text = element_text(size = 18),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title.x = element_blank())
}

# model curves ####
# population-level prediction across `grid` values of xvar, with every other
# predictor held at its mean (scaled ones at 0, clay color averaged over),
# and xvar's interaction terms evaluated at the other term's mean
# returns the linear predictor and its 95% CI; back-transform outside

curve_fit <- function(model, grid, xvar = "ratio", offset = 0) {
  fe <- fixef(model)$cond
  X <- model.matrix(model)[, names(fe), drop = F]
  V <- vcov(model)$cond[names(fe), names(fe)]
  ref <- colMeans(X)
  
  int.col <- grep(paste0("^", xvar, ":"), names(fe), value = T)
  other <- sub(paste0("^", xvar, ":"), "", int.col)
  
  Xg <- t(sapply(grid, function(r) {
    x <- ref
    x[xvar] <- r
    x[int.col] <- r * ref[other]
    x
  }))
  
  eta <- drop(Xg %*% fe) + offset
  se <- sqrt(rowSums((Xg %*% V) * Xg))
  
  data.frame(ratio = grid,
             fit = eta,
             lwr = eta - 1.96 * se,
             upr = eta + 1.96 * se)
}

# 2. bottom-up figure ####

# growth efficiency ####
# response is ge with the year and initial-weight effects removed
# (both are in m.ge and initial weight matters, so raw ge would mix them in)
# additive model won, so only an overall p

fe.ge <- fixef(m.ge)$cond
mm.ge <- model.matrix(m.ge)
covs.ge <- c("year2022", "log(initialWeight)")

geCats$ge_adj <- geCats$ge -
  drop(scale(mm.ge[, covs.ge], scale = F) %*% fe.ge[covs.ge])

fd.ge <- fig_data(geCats, "ge_adj", m.ge)

vio1 <- plot_fig(fd.ge, "Residual growth efficiency\n", violin = T) +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_markdown(face = c("bold",
                                                "plain", "plain", "plain")))

vio1

# pupation (unparasitized survival) ####

fd.pupa <- fig_data(pupest, "isPupal", m.pupa, gl.pupa)

bp2 <- plot_fig(fd.pupa, "Residual pupation rate\n") +
  theme(axis.text.x = element_blank())

bp2

# pupal weight ####
# raw grams on the plot; model is on log weight

fd.pw <- fig_data(pwp2, "pWeight", m.pw, gl.pw)

vio2 <- plot_fig(fd.pw, "Residual pupal weight (g)", violin = T, violin.width = 1.2) +
  theme(axis.text.x = element_blank())

vio2

# assemble and save ####

bu1 <- vio1 + theme(axis.text.x = element_markdown(size = 22))
bu2 <- vio2
bu3 <- bp2

bufig <- bu1/bu3/bu2

bufig

ggsave("figures/bigOne_bu.png", plot = bufig, height = 11, width = 8.5, unit = "in")

# 3. mortality figure ####

fd.death <- fig_data(cc[cc$isMissing == 0, ], "isDeceased", m.death, gl.death)

bp.death <- plot_fig(fd.death, "Residual total mortality rate\n") +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_markdown(face = c("bold",
                                                "plain", "plain", "plain"),
                                       size = 18))

bp.death

ggsave("figures/mortality.png", plot = bp.death, height = 4, width = 8.5, unit = "in")

# 4. top-down figure ####

# parasitism ####
# additive model won, so only an overall p

fd.toid <- fig_data(toidest, "isToid", m.toid)

bp1 <- plot_fig(fd.toid, "Residual Parasitoid Emergence Rate\n") +
  scale_x_discrete(position = "top") +
  ggtitle("Parasitoid emergence rate") +
  theme(plot.title = element_text(size = 18, face = "italic"),
        axis.text.x = element_markdown(face = c("bold",
                                                "plain", "plain", "plain"),
                                       size = 14))

bp1

# pan traps ####

rat <- seq(min(td.mush$ratio), max(td.mush$ratio), by = 0.01)
off.days <- log(mean(td.mush$daysOut))

curve.w <- curve_fit(m.tf.wasps, rat, offset = off.days) %>%
  mutate(across(c(fit, lwr, upr), exp))

curve.f <- curve_fit(m.tf.flies, rat, offset = off.days) %>%
  mutate(across(c(fit, lwr, upr), exp))

tf <- ggplot(data = td.mush, aes(x = ratio, y = flies)) +
  geom_jitter(color = "darkblue", shape = 19, 
              position = position_jitter(seed = 1234)) +
  geom_ribbon(data = curve.f,
              aes(ymin = lwr, ymax = upr, x = ratio),
              inherit.aes = F,
              alpha = .1, fill = "blue") +
  geom_line(data = curve.f, aes(x = ratio, y = fit), inherit.aes = F,
            color = "darkblue", linewidth = 1.1) +
  geom_jitter(aes(y = wasps), color = "gold3", 
              position = position_jitter(seed = 1234)) +
  geom_ribbon(data = curve.w,
              aes(ymin = lwr, ymax = upr, x = ratio),
              inherit.aes = F,
              alpha = .1, fill = "gold4") +
  geom_line(data = curve.w, aes(x = ratio, y = fit), inherit.aes = F,
            color = "gold4", linewidth = 1.1) +
  theme_tufte() +
  ylab("Parasitoids captured\n") +
  xlab("\nVegetation provenance") +
  ggtitle("Adult parasitoid abundance") +
  scale_x_continuous(breaks = c(-2.5, -.5, 1.5),
                     labels = c("more\nnative", "equal", "more\nexotic")) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 20)) +
  theme(legend.position = "none",
        text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "italic"),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

tf

# clay decoys ####
clay.fig <- clays %>%
  mutate(ratio = as.numeric(ratio)) %>%
  group_by(block, trap) %>%
  summarise(ratio = mean(ratio),
            checks = n(),
            struck = mean(strikesB),
            .groups = "drop")

rat.c <- seq(min(clay.fig$ratio), max(clay.fig$ratio), by = 0.01)

curve.c <- curve_fit(m.clay, rat.c) %>%
  mutate(across(c(fit, lwr, upr), plogis))

clayplot <- ggplot(data = clay.fig, aes(x = ratio, y = struck)) +
  geom_ribbon(data = curve.c,
              aes(ymin = lwr, ymax = upr, x = ratio),
              inherit.aes = F,
              alpha = .15, fill = "maroon") +
  geom_line(data = curve.c, aes(x = ratio, y = fit), inherit.aes = F,
            color = "red", linewidth = 1.1) +
  geom_point(alpha = .6) +
  theme_tufte() +
  ylab("Proportion attacked\n") +
  xlab("\nVegetation provenance") +
  scale_x_continuous(breaks = c(-2.25, -.5, 1.25),
                     labels = c("more\nnative", "equal", "more\nexotic")) +
  ggtitle("Bird attacks") +
  theme(text = element_text(size = 18),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "italic"),
        axis.line = element_line())

clayplot

# assemble and save ####

tdfig <- (bp1/plot_spacer())|(tf/clayplot)

tdfig

ggsave("figures/bigOne_td.png", plot = tdfig, height = 8, width = 11, unit = "in")

# 5. network figure ####

library(bipartite)

cc.bip <- cc %>%
  filter(!treeSpecies %in% c("BERTH", "EUOAL", "CELOR", "RHOSC")) %>%
  filter(catSpecies %in% cats.good$catSpecies) %>%
  filter(!catSpecies %in% "MICROX") %>%
  mutate(treeSpecies = ifelse(treeSpecies %in% c("MALFL", "MALPR", "MALPU", "MALSI"),
                              "MALXX", treeSpecies)) %>%
  mutate(treeSpecies = ifelse(treeSpecies %in% c("VIBOP", "SAMCA"),
                              "VIBDE", treeSpecies)) %>%
  group_by(catSpecies, treeSpecies) %>%
  summarise(total = n()) %>%
  pivot_wider(id_cols = catSpecies,
              names_from = treeSpecies,
              values_from = total,
              values_fill = 0) %>%
  column_to_rownames("catSpecies") %>%
  as.matrix() %>%
  log1p()

# plants: family (1 = Caprifoliaceae, 2 = Rosaceae, 3 = Oleaceae), native (1/0)
spec <- c("FRAAM", "LIGOB",
          "MALXX", "PRUSE", "ROSMU",
          "VIBAC", "VIBDI",
          "VIBLE", "CRAXX", "LONMO",
          "RUBXX", "VIBPL", "VIBDE",
          "AMEXX", "VIBSI")
fam <- c(3,3,
         2,2,2,
         1,1,
         1,2,1,
         2,1,1,
         2,1)
nat <- c(1,0,
         0,1,0,
         1,0,
         1,1,0,
         1,0,1,
         1,0)

spec.high <- data.frame(treeSpecies = spec,
                        hostFamily = fam,
                        hostNative = nat) %>%
  arrange(hostFamily, -hostNative, treeSpecies)

spec.high$color <- c(rep("#020887", 3),
                     rep("#585A87", 4),
                     rep("#840032", 4),
                     rep("#844D5F", 2),
                     rep("#FFA630", 1),
                     "#F1D7B5")

# caterpillars and their families
catso <- c("ACHADI", "ACROSU", "BISTBE", "CAMPPE", "CATOUL", "CROCNO",
           "ECTRCR", "ELAPVE", "ENNOSU", "EUPIMI", "EUPISW", "EUPSXX", "EUTRCL",
           "HETEGU", "HIMEFI", "HYPAUN", "HYPESC", "IRIDEP", "IRIDLA", "LAMBFI",
           "LITHXX", "LOMOVE", "MELACA", "MESORU", "MORRCO", "MORRLA", "NEMARE",
           "ORGYLE", "ORTHRU", "PALEVE", "PALTAN", "PROCLI", "PROTPO", "PSEUCY",
           "SCHIUN", "ZALEHO", "ZALEXX", "ZANCXX")

fam.cat <- c("n", "n", "g", "g", "e", "n",
             "g", "n", "g", "g", "g", "n", "g",
             "not", "n", "g", "e", "g", "g", "g",
             "n", "g", "g", "g", "n", "n", "g",
             "e", "n", "g", "n", "g", "g", "d",
             "not", "e", "e", "n")

spec.low <- data.frame(catSpecies = catso,
                       catFamily = fam.cat) %>%
  arrange(catFamily, catSpecies)

spec.low$colors <- c(
  "#152F15",
  rep("#31572C", 5),
  rep("#4F772D", 18,),
  rep("#90A955", 12,),
  rep("#ECF39E", 2)
)

cc.bip <- cc.bip[spec.low$catSpecies, spec.high$treeSpecies]

catlabs <- c("Pseu. cy.", "Cato. ul.", "Hype. sc.", "Orgy. le.",
             "Zale ho.", "Zale lu.", "Bist. be.", "Camp. pe.", "Ectr. cr.",
             "Enno. su.", "Eupi. mi.", "Eupi. sw.", "Eutr. cl.", "Hypa. un.",
             "Irid. ep.", "Irid. la.", "Lamb. fi.", "Lomo. ve.", "Mela. ca.",
             "Meso. ru.", "Nema. re.", "Pale. ve.", "Proc. li.", "Prot. po.",
             "Acha. di.", "Acro. su.", "Croc. no.", "Elap. ve.", "Eups.", "Hime. fi.",
             "Lith.", "Morr. co.", "Morr. la.", "Orth.", "Palt. an.",
             "Zanc.", "Hete. gu.", "Schi. un.")

row.names(cc.bip) <- catlabs

tlabs <- c("V. acerifolium", "V. dentatum", "V. lentago", "L. morrowii", "V. dillitatum",
           "V. plicatum", "V. sieboldii", "Amelanchier", "Crataegus", "P. serotina", "Rubus",
           "Malus", "R. multiflora", "F. americanum", "L. obtusifolium")

colnames(cc.bip) <- tlabs

# not saved automatically; wrap in png() ... dev.off() to save
par(font = 3)

plotweb(cc.bip,
        method = "normal",
        col.high = spec.high$color,
        col.low = spec.low$color,
        text.rot = 90,
        labsize =  1.8,
        text.high.col="grey99",
        text.low.col="grey99",
        bor.col.high="grey99",
        bor.col.interaction ="grey99",
        bor.col.low="grey99",
        ybig = 1.1)

# 6. caterpillar tables + checks ####

# caterpillars per caterpillar family in the network
yo <- cbind.data.frame(spec.low, tot = rowSums(exp(cc.bip) - 1))

yy <- yo %>%
  group_by(catFamily) %>%
  summarise(total = sum(tot))

table(cc$catSpecies, cc$hostNative)

write.csv(as.data.frame(table(cc$catSpecies)),
          file = "figures/catTable.csv",
          row.names = F)

write.csv(as.data.frame(table(cc$catSpecies, cc$hostNative)) %>%
            pivot_wider(id_cols = Var1,
                        names_from = Var2,
                        values_from = Freq,
                        values_fill = 0),
          file = "figures/catTable2.csv",
          row.names = T)

# 7. presentation versions ####
# black backgrounds for talks; not part of the paper. uncomment to use
# (column names updated to the fig_data() objects: pts$resid, summ$rate)

# pupal weight, overall only
# ggplot(data = fd.pw$pts[fd.pw$pts$hostFamily == "Overall", ], aes(x = 1, y = resid, fill = hostNative)) +
#   geom_hline(aes(yintercept = 0), color = "white", alpha = 1) +
#   geom_split_violin(drop = F, width = .9, alpha = .8, color = "white") +
#   geom_point(data = fd.pw$pts[fd.pw$pts$hostFamily == "Overall" & fd.pw$pts$hostNative == "native", ],
#              position = position_jitternudge(width = 0.1,
#                                              seed = 1234, x = -0.15,
#                                              nudge.from = "jittered"),
#              color = "lightgreen", alpha = .5, size = 1.5) +
#   geom_point(data = fd.pw$pts[fd.pw$pts$hostFamily == "Overall" & fd.pw$pts$hostNative == "exotic", ],
#              position = position_jitternudge(width = 0.1,
#                                              seed = 1234, x = 0.15,
#                                              nudge.from = "jittered"),
#              color = "lightblue", alpha = .5, size = 1.5) +
#   theme_tufte() +
#   scale_fill_manual(values = cols.ne) +
#   geom_errorbar(data = fd.pw$summ[fd.pw$summ$hostFamily == "Overall", ],
#                 aes(ymin = q05, ymax = q95, x = 1,
#                     group = hostNative, color = hostNative),
#                 width = .2, linewidth = 1.4,
#                 position = position_dodge(width = .5),
#                 inherit.aes = F) +
#   geom_point(data = fd.pw$summ[fd.pw$summ$hostFamily == "Overall", ],
#              aes(y = rate, group = hostNative, x = 1, color = hostNative),
#              size = 5,
#              position = position_dodge(width = .5),
#              inherit.aes = F) +
#   scale_color_manual(values = c("darkgreen", "#170C6A")) +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 18, color = "white"),
#         plot.background = element_rect(fill = "black"),
#         axis.ticks = element_blank(),
#         axis.title.x = element_blank())

# parasitism, by family
# ggplot(data = fd.toid$summ[fd.toid$summ$hostFamily != "Overall", ], aes(x = hostFamily, y = rate, group = hostNative)) +
#   geom_hline(aes(yintercept = 0),color = "white", alpha = 1) +
#   geom_errorbar(aes(ymin = q05, ymax = q95, color = hostNative),
#                 width = .25, linewidth = 1.3, alpha = .8,
#                 position = position_dodge(width = .5)) +
#   geom_point(position = position_dodge(width = .5),
#              aes(color = hostNative), size = 5, alpha = .9) +
#   theme_tufte() +
#   scale_color_manual(values = c("lightgreen", "lightblue")) +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 18, color = "white"),
#         plot.background = element_rect(fill = "black"),
#         axis.ticks = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(color = "white", size = 24, face = "bold"))

# pupation, overall only
# ggplot(data = fd.pupa$summ[fd.pupa$summ$hostFamily == "Overall", ], aes(x = hostFamily, y = rate, group = hostNative)) +
#   geom_hline(aes(yintercept = 0),color = "white", alpha = 1) +
#   geom_errorbar(aes(ymin = q05, ymax = q95, color = hostNative),
#                 width = .25, linewidth = 1.3, alpha = .8,
#                 position = position_dodge(width = .5)) +
#   geom_point(position = position_dodge(width = .5),
#              aes(color = hostNative), size = 5, alpha = .9) +
#   theme_tufte() +
#   scale_color_manual(values = c("lightgreen", "lightblue")) +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 18, color = "white"),
#         plot.background = element_rect(fill = "black"),
#         axis.ticks = element_blank(),
#         axis.title.x = element_blank())

# clay decoys
# ggplot(data = clay.fig, aes(x = ratio, y = struck)) +
#   geom_ribbon(data = curve.c, aes(ymin = lwr, ymax = upr, x = ratio),
#               inherit.aes = F, alpha = .5, fill = "pink") +
#   geom_line(data = curve.c, aes(x = ratio, y = fit), inherit.aes = F,
#             color = "maroon", linewidth = 1.1) +
#   geom_point(aes(size = checks), color = "pink") +
#   theme_tufte() +
#   ylab("Proportion of decoys attacked\n") +
#   xlab("\nVegetation provenance") +
#   scale_x_continuous(breaks = c(-2.25, -.5, 1.25),
#                      labels = c("more\nnative", "equal", "more\nexotic")) +
#   theme(text = element_text(size = 18, color = "white"),
#         legend.position = "none",
#         axis.text = element_text(color = "white", size = 18),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_line(color = "white"),
#         axis.line = element_line(color = "white"),
#         axis.title.x = element_blank(),
#         plot.background = element_rect(fill = "black"))