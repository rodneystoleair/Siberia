library('MKinfer')

recon_plot_meteo = recon_plot |> 
  rename(YEAR = ages,
         param = type1,
         model = type2,
         value = fitted) |> 
  select(-depth) |> 
  filter(model == 'rf')

meteodata = readxl::read_xlsx('data/modern/tura_meteodata.xlsx') |> 
  mutate(round(across(-YEAR), 2)) |> 
  rename(T_jan_meteo = JAN,
         T_jul_meteo = JUL,
         T_ann_meteo = MAAT,
         P_ann_meteo = MAP) |> 
  na.omit()

meteodata_smooth = meteodata |> 
  mutate(T_jan_meteo = 
           predict(loess(T_jan_meteo ~ YEAR, meteodata, span = 0.25)),
         T_jul_meteo = 
           predict(loess(T_jul_meteo ~ YEAR, meteodata, span = 0.25)),
         T_ann_meteo = 
           predict(loess(T_ann_meteo ~ YEAR, meteodata, span = 0.25)),
         P_ann_meteo = 
           predict(loess(P_ann_meteo ~ YEAR, meteodata, span = 0.25)),
         model = NA) |> 
  pivot_longer(T_jan_meteo:P_ann_meteo,
               names_to = 'param',
               values_to = 'value') |>
  rbind(recon_plot_meteo) |>
  relocate(model, .after = last_col()) |> 
  rbind(recon_plot_meteo)

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

vert_plot = ggplot(recon_plot, aes(x = fitted,
                                   y = ages,
                                   color = type2,
                                   group = type2)) +
  geom_lineh(size = 0.3) +
  facet_geochem_gridh(vars(type1),
                      labeller = labeller(type1 = ordinary)) +
  scale_y_continuous(breaks = seq(1880, 2020, 10)) +
  labs(x = 'Reconstructed values',
       y = 'Age (cal yr BP)') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  theme_paleo() +
  rotated_axis_labels(45)
vert_plot

boot.t.test(recons$T_ann.rf, meteodata$T_ann_meteo)
boot.t.test(recons$P_ann.rf, meteodata$P_ann_meteo)

meteodata_smooth |> 
  filter(param %in% c('T_ann', 'T_ann_meteo')) |> 
  ggplot(aes(value, color = param)) +
  geom_density() +
  labs(x = 'MAAT, °C',
       y = 'Kernel density estimation') +
  scale_color_discrete(name = 'MAAT',
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
  scale_color_discrete(name = 'MAP',
                       labels = c('Reconstructed', 'Measured')) +
  theme_paleo() +
  theme(legend.position = 'bottom')

ggsave('plots/reconstructions/P_ann_kernelDensiry.pdf',
       plot = last_plot(), device = 'pdf', width = 1000, height = 1200,
       units = 'px')
