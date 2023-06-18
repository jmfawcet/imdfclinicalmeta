
# Set APA theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        #panel.spacing.x = unit(0,"line"),
        axis.line=element_line(),
        text=element_text(family='sans'),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7),
        legend.position='none')

plot_custom_colors <- c(
  'GoldenDream' = "#264653",
  'BostonBlue'  = "#2a9d8f",
  'Black'       = "#000000",
  'col4' = '#e9c46a',
  'col5' = '#f4a261',
  'col6' = '#e76f51')


# Figure 3 - Basic DF -----------------------------------------------------

temp1 = prepare_plot_data(neg_clin, label='Clinical (Neg)')
temp2 = prepare_plot_data(neu_clin, label='Clinical (Neu)')
temp3 = prepare_plot_data(neg_con, label='Control (Neg)')
temp4 = prepare_plot_data(neu_con, label='Control (Neu)')

temp = rbind(temp1, temp2, temp3, temp4)
temp$label = factor(temp$label, levels=c('Control (Neu)', 'Control (Neg)', 'Clinical (Neu)', 'Clinical (Neg)'))
temp$facet_labels = factor(temp$facet_labels, levels=c('Addiction', 'Anxiety', 'Depression', 'Psychosis', 'Mixed', 'Summary'))
temp[temp$plot_labels=='Overall',]$StudyID=999
temp[temp$points=='Anxiety+Depression',]$points = 'Overall'

# For ordering
temp[temp$plot_labels=='Addiction',]$Year = -70
temp[temp$plot_labels=='Anxiety',]$Year = -60
temp[temp$plot_labels=='Depression',]$Year = -50
temp[temp$plot_labels=='Psychosis',]$Year = -40
temp[temp$plot_labels=='Mixed',]$Year = -30
temp[temp$plot_labels=='Anxiety+Depression',]$Year = -20
temp[temp$plot_labels=='Overall',]$Year = -10

# Re-order
re_ordered_col = temp %>% select(plot_labels, StudyID, categorization, Year, es_id, yi) %>%
  arrange(desc(Year), desc(StudyID)) %>%
  mutate(for_ordering = 1:n())

temp = temp %>%
  left_join(re_ordered_col) %>%
  mutate(for_ordering = ifelse(is.na(for_ordering), 999, for_ordering))
temp$plot_labels2 = fct_reorder(temp$plot_labels, temp$for_ordering, mean)

temp[temp$plot_labels2 %in% c('Psychosis', 'Addiction') & temp$label %in% c('Control (Neg)', 'Clinical (Neg)'),c('yi', 'c_m', 'c_se', 'c_lb', 'c_ub', 'obs', 'pi_lb', 'pi_ub')] = NA

PCP = ggplot(temp, aes(y=plot_labels2, x=c_m, xmin=c_lb, xmax=c_ub, shape = points))+
  # Add CI bars
  geom_errorbarh(height=0, lwd=.55) +
  geom_errorbarh(data=subset(temp, facet_labels=='Summary'), height=0, lwd=.2, mapping=aes(xmin=pi_lb, xmax=pi_ub)) +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  # Add data points and color-code them; set point size based on the sample size of the study
  geom_point(data=subset(temp, points=='Addiction'), color=plot_custom_colors[3], fill=plot_custom_colors[1], shape=23, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Anxiety'), color=plot_custom_colors[3], fill=plot_custom_colors[2], shape=21, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Depression'), color=plot_custom_colors[3], fill=plot_custom_colors[4], shape=24, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Mixed'), color=plot_custom_colors[5], fill=plot_custom_colors[5], shape=8, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Psychosis'), color=plot_custom_colors[3], fill=plot_custom_colors[6], shape=25, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Overall'), color=plot_custom_colors[3], fill=plot_custom_colors[3], shape=22, aes(size=NControl))+
  # Specify the limits of the x-axis and relabel it
  scale_x_continuous(limits=c(-20,60), name='Directed Forgetting (%)', breaks=seq(-20,60,20), minor_breaks=seq(-10,50,20))+
  #scale_y_discrete(labels=roles) +
  geom_point(data=temp, x=temp$obs*100, pch=4)+
  # Give y-axis a meaningful label
  ylab('')+
  scale_size(range=c(.5, 3)) + 
  # Create sub-plots (i.e., facets) based on levels of instructions
  # Allow unique axes (references don't repeat)
  facet_grid(facet_labels~label, scales= 'free', space='free')+
  # Apply custom APA theme
  apatheme

# Apply different colors to instructions legend on the right
PCP <- ggplot_gtable(ggplot_build(PCP))
strip_both <- which(grepl('strip-', PCP$layout$name))
fills <- c(plot_custom_colors[1],plot_custom_colors[2],
           plot_custom_colors[4], plot_custom_colors[5],
           plot_custom_colors[6],"grey")
k <- 1

for (i in strip_both[1:4]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- 'black'
  j <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  k <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children[[k]]$gp$col <- 'white'
}

for (i in strip_both[-1:-4]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$alpha <- 0.6
  k <- k+1
}

plot(PCP)

# Display forest plot for clinical and sub-clinical groups

pdf('figures/Figure 2.pdf', width=8, height=8)
plot(PCP)
dev.off()

# Figure 4 - Comparing Negative and Neutral Items -------------------------

temp1 = prepare_plot_data(emodiff_clin, label='Clinical (Neg - Neu)')
temp2 = prepare_plot_data(emodiff_con, label='Control (Neg - Neu)')

temp = rbind(temp1, temp2)
temp$label = factor(temp$label, levels=c('Control (Neg - Neu)', 'Clinical (Neg - Neu)'))
temp$facet_labels = factor(temp$facet_labels, levels=c('Addiction', 'Anxiety', 'Depression', 'Psychosis', 'Mixed', 'Summary'))
temp[temp$plot_labels=='Overall',]$StudyID=999
temp[temp$points=='Anxiety+Depression',]$points = 'Overall'

# For ordering
temp[temp$plot_labels=='Addiction',]$Year = -70
temp[temp$plot_labels=='Anxiety',]$Year = -60
temp[temp$plot_labels=='Depression',]$Year = -50
temp[temp$plot_labels=='Psychosis',]$Year = -40
temp[temp$plot_labels=='Mixed',]$Year = -30
temp[temp$plot_labels=='Anxiety+Depression',]$Year = -20
temp[temp$plot_labels=='Overall',]$Year = -10

# Re-order
re_ordered_col = temp %>% select(plot_labels, StudyID, categorization, Year, es_id, yi) %>%
  arrange(desc(Year), desc(StudyID)) %>%
  mutate(for_ordering = 1:n())

temp = temp %>%
  left_join(re_ordered_col) %>%
  mutate(for_ordering = ifelse(is.na(for_ordering), 999, for_ordering))
temp$plot_labels2 = fct_reorder(temp$plot_labels, temp$for_ordering, mean)

temp[temp$plot_labels2 %in% c('Psychosis', 'Addiction'),c('yi', 'c_m', 'c_se', 'c_lb', 'c_ub', 'obs', 'pi_lb', 'pi_ub')] = NA

PCP = ggplot(temp, aes(y=plot_labels2, x=c_m, xmin=c_lb, xmax=c_ub, shape = points))+
  # Add CI bars
  geom_errorbarh(height=0, lwd=.55) +
  geom_errorbarh(data=subset(temp, facet_labels=='Summary'), height=0, lwd=.2, mapping=aes(xmin=pi_lb, xmax=pi_ub)) +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  # Add data points and color-code them; set point size based on the sample size of the study
  geom_point(data=subset(temp, points=='Addiction'), color=plot_custom_colors[3], fill=plot_custom_colors[1], shape=23, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Anxiety'), color=plot_custom_colors[3], fill=plot_custom_colors[2], shape=21, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Depression'), color=plot_custom_colors[3], fill=plot_custom_colors[4], shape=24, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Mixed'), color=plot_custom_colors[5], fill=plot_custom_colors[5], shape=8, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Psychosis'), color=plot_custom_colors[3], fill=plot_custom_colors[6], shape=25, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Overall'), color=plot_custom_colors[3], fill=plot_custom_colors[3], shape=22, aes(size=NControl))+
  # Specify the limits of the x-axis and relabel it
  scale_x_continuous(limits=c(-40,30), name='Directed Forgetting (%)', breaks=seq(-40,30,10), minor_breaks=seq(-10,50,20))+
  #scale_y_discrete(labels=roles) +
  geom_point(data=temp, x=temp$obs*100, pch=4)+
  # Give y-axis a meaningful label
  ylab('')+
  scale_size(range=c(.5, 3)) + 
  # Create sub-plots (i.e., facets) based on levels of instructions
  # Allow unique axes (references don't repeat)
  facet_grid(facet_labels~label, scales= 'free', space='free')+
  # Apply custom APA theme
  apatheme

# Apply different colors to instructions legend on the right
PCP <- ggplot_gtable(ggplot_build(PCP))
strip_both <- which(grepl('strip-', PCP$layout$name))
fills <- c(plot_custom_colors[1],plot_custom_colors[2],
           plot_custom_colors[4], plot_custom_colors[5],
           plot_custom_colors[6],"grey")
k <- 1

for (i in strip_both[1:2]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- 'black'
  j <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  k <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children[[k]]$gp$col <- 'white'
}

for (i in strip_both[-1:-2]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$alpha <- 0.6
  k <- k+1
}

plot(PCP)

# Display forest plot for clinical and sub-clinical groups

pdf('figures/Figure 3.pdf', width=8, height=8.5)
plot(PCP)
dev.off()

# Figure 5 - Comparing Clinical and Control -------------------------------

temp1 = prepare_plot_data(neu_comp, label='Neutral (Control - Clinical)')
temp2 = prepare_plot_data(neg_comp, label='Negative (Control - Clinical)')

temp = rbind(temp1, temp2)
temp$label = factor(temp$label, levels=c('Neutral (Control - Clinical)', 'Negative (Control - Clinical)'))
temp$facet_labels = factor(temp$facet_labels, levels=c('Addiction', 'Anxiety', 'Depression', 'Psychosis', 'Mixed', 'Summary'))
temp[temp$plot_labels=='Overall',]$StudyID=999
temp[temp$points=='Anxiety+Depression',]$points = 'Overall'

# For ordering
temp[temp$plot_labels=='Addiction',]$Year = -70
temp[temp$plot_labels=='Anxiety',]$Year = -60
temp[temp$plot_labels=='Depression',]$Year = -50
temp[temp$plot_labels=='Psychosis',]$Year = -40
temp[temp$plot_labels=='Mixed',]$Year = -30
temp[temp$plot_labels=='Anxiety+Depression',]$Year = -20
temp[temp$plot_labels=='Overall',]$Year = -10

# Re-order
re_ordered_col = temp %>% select(plot_labels, StudyID, categorization, Year, es_id, yi) %>%
  arrange(desc(Year), desc(StudyID)) %>%
  mutate(for_ordering = 1:n())

temp = temp %>%
  left_join(re_ordered_col) %>%
  mutate(for_ordering = ifelse(is.na(for_ordering), 999, for_ordering))
temp$plot_labels2 = fct_reorder(temp$plot_labels, temp$for_ordering, mean)

temp[temp$plot_labels2 %in% c('Psychosis', 'Addiction') & temp$label == 'Negative (Control - Clinical)',c('yi', 'c_m', 'c_se', 'c_lb', 'c_ub', 'obs', 'pi_lb', 'pi_ub')] = NA

PCP = ggplot(temp, aes(y=plot_labels2, x=c_m, xmin=c_lb, xmax=c_ub, shape = points))+
  # Add CI bars
  geom_errorbarh(height=0, lwd=.55) +
  geom_errorbarh(data=subset(temp, facet_labels=='Summary'), height=0, lwd=.2, mapping=aes(xmin=pi_lb, xmax=pi_ub)) +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  # Add data points and color-code them; set point size based on the sample size of the study
  geom_point(data=subset(temp, points=='Addiction'), color=plot_custom_colors[3], fill=plot_custom_colors[1], shape=23, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Anxiety'), color=plot_custom_colors[3], fill=plot_custom_colors[2], shape=21, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Depression'), color=plot_custom_colors[3], fill=plot_custom_colors[4], shape=24, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Mixed'), color=plot_custom_colors[5], fill=plot_custom_colors[5], shape=8, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Psychosis'), color=plot_custom_colors[3], fill=plot_custom_colors[6], shape=25, aes(size=NControl))+
  geom_point(data=subset(temp, points=='Overall'), color=plot_custom_colors[3], fill=plot_custom_colors[3], shape=22, aes(size=NControl))+
  # Specify the limits of the x-axis and relabel it
  scale_x_continuous(limits=c(-20,40), name='Directed Forgetting (%)', breaks=seq(-20,40,10), minor_breaks=seq(-10,50,20))+
  #scale_y_discrete(labels=roles) +
  geom_point(data=temp, x=temp$obs*100, pch=4)+
  # Give y-axis a meaningful label
  ylab('')+
  scale_size(range=c(.5, 3)) + 
  # Create sub-plots (i.e., facets) based on levels of instructions
  # Allow unique axes (references don't repeat)
  facet_grid(facet_labels~label, scales= 'free', space='free')+
  # Apply custom APA theme
  apatheme

# Apply different colors to instructions legend on the right
PCP <- ggplot_gtable(ggplot_build(PCP))
strip_both <- which(grepl('strip-', PCP$layout$name))
fills <- c(plot_custom_colors[1],plot_custom_colors[2],
           plot_custom_colors[4], plot_custom_colors[5],
           plot_custom_colors[6],"grey")
k <- 1

for (i in strip_both[1:2]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- 'black'
  j <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  k <- which(grepl('text', PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$children[[k]]$gp$col <- 'white'
}

for (i in strip_both[-1:-2]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$alpha <- 0.6
  k <- k+1
}

plot(PCP)

# Display forest plot for clinical and sub-clinical groups

pdf('figures/Figure 4.pdf', width=8, height=8)
plot(PCP)
dev.off()

temp %>% filter(facet_labels == 'Summary') %>% select(label,plot_labels2, c_m, c_lb) %>% arrange(c_m) %>% print(n=1000)

