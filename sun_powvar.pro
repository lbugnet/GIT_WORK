PRO SUN_POWVAR
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/VIRGO_g+r_SunAsKepler/VIRGO_r+g_SunMaxAsKepler.txt', freq, pow, format='D,D', count=n

FREQ_INIC_GR_DEB=min(freq)
FREQ_FIN_GR=max(freq)
FREQ_INIC_GR_NS=273
FREQ_FIN_GR_NS=max(freq)

star_tab_psd=dblarr(n_elements(freq), n_elements(pow))
star_tab_psd=[freq,pow]
ns=1
ID_STAR='SUN'
POWVAR, STAR_TAB_PSD=STAR_TAB_PSD, OUTPUT_A2Z_1=OUTPUT_A2Z_1, NS=NS, FREQ_INIC_GR, FREQ_FIN_GR, FREQ_INIC_GR_NS, FREQ_FIN_GR_NS, ID_STAR=ID_STAR, STAR_PATH_PSD=STAR_PATH_PSD











END