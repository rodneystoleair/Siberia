library('MKinfer')
load('Temp12k_v1_0_0.RData')

recon_plot_meteo = recon_plot |> 
  rename(YEAR = ages,
         param = type1,
         model = type2,
         value = fitted) |> 
  select(-depth)

meteodata = readxl::read_xlsx('data/modern/tura_meteodata.xlsx') |> 
  mutate(round(across(-YEAR), 2)) |> 
  rename(T_jan = JAN,
         T_jul = JUL,
         T_ann = MAAT,
         P_ann = MAP) |> 
  na.omit()

smooth = tibble(
  YEAR = meteodata$YEAR,
  T_jan =
    predict(loess(T_jan ~ YEAR, meteodata, span = 0.25)),
  T_jul =
    predict(loess(T_jul ~ YEAR, meteodata, span = 0.25)),
  T_ann =
    predict(loess(T_ann ~ YEAR, meteodata, span = 0.25)),
  P_ann =
    predict(loess(P_ann ~ YEAR, meteodata, span = 0.25)),
  model = 'loess'
) |> 
  pivot_longer(T_jan:P_ann,
               names_to = 'param',
               values_to = 'value')

meteodata_smooth = meteodata |>
  mutate(model = 'measured') |> 
  pivot_longer(T_jan:P_ann,
               names_to = 'param',
               values_to = 'value') |> 
  rbind(recon_plot_meteo) |>
  rbind(smooth) |> 
  relocate(model, .after = last_col())

meteo_plot = ggplot(meteodata_smooth,
                    aes(x = value,
                        y = YEAR,
                        color = model, 
                        group = model)) +
  geom_lineh(size = 0.3) +
  facet_geochem_gridh(vars(param)) +
  scale_y_continuous(breaks = seq(1890, 2020, 10)) +
  labs(x = 'Year',
       y = 'Value') +
  theme_paleo() +
  rotated_axis_labels(45)
meteo_plot

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions_withMeteodata.pdf'),
       plot = meteo_plot, device = 'pdf', width = 2700, height = 1800,
       units = 'px')

recons_corr = tibble(
  YEAR = abs(recons$ages - 1950),
  T_ann_rf = recons$T_ann.rf,
  P_ann_rf = recons$P_ann.rf,
) |> 
  left_join(meteodata, by = 'YEAR') |> 
  na.omit()

boot.t.test(recons$T_ann.rf, meteodata$T_ann)
boot.t.test(recons$P_ann.rf, meteodata$P_ann)

meteodata_smooth |> 
  filter(model == 'measured' | model == 'rf') |> 
  filter(param == 'P_ann' | param == 'T_ann') |> 
  ggplot(mapping = aes(x = param, y = value, fill = model)) +
  geom_boxplot() +
  facet_wrap(~param, scale = 'free') +
  theme_paleo()

boot.t.test(recons_corr$T_ann_rf, recons_corr$T_ann)
boot.t.test(recons_corr$P_ann_rf, recons_corr$P_ann)

cor.test(recons_corr$T_ann_rf, recons_corr$T_ann,
         method = 'pearson')
cor.test(recons_corr$P_ann_rf, recons_corr$P_ann,
         method = 'pearson')

meteodata_smooth |> 
  filter(param %in% c('T_ann', 'T_ann_meteo')) |> 
  ggplot(aes(value, color = param)) +
  geom_density() +
  labs(x = 'MAAT, °C',
       y = 'Kernel density estimation') +
  scale_color_discrete(name = 'Tann',
                       labels = c('Reconstructed', 'Measured')) +
  theme_paleo() +
  theme(legend.position = 'bottom')

ggsave('plots/reconstructions/T_ann_kernelDensiry.pdf',
       plot = last_plot(), device = 'pdf', width = 1000, height = 1200,
       units = 'px')

meteodata_smooth |> 
  filter(param %in% c('P_ann', 'P_ann_meteo')) |> 
  ggplot(aes(value, color = param)) +
  geom_density() +
  labs(x = 'MAP, mm/yr',
       y = 'Kernel density estimation') +
  scale_color_discrete(name = 'Pann',
                       labels = c('Reconstructed', 'Measured')) +
  theme_paleo() +
  theme(legend.position = 'bottom')

ggsave('plots/reconstructions/P_ann_kernelDensiry.pdf',
       plot = last_plot(), device = 'pdf', width = 1000, height = 1200,
       units = 'px')
