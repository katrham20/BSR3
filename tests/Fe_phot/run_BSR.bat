

# w_bsw 3d6_4s2_5D.w

# w_bsw 3d7_4s_5P.w
# repeat for all traget states

bsr_prep3

bsr_conf3

bsr_breit3 klsp1=1 klsp2=3

bsr_mat3 klsp1=1 klsp2=3

bsr_hd3 klsp1=1 klsp2=3 itype=1  

bsr_hd3 klsp1=1 klsp2=3 itype=-1

mult3 3d6_4s2_5D.c cfg.001 E1
bsr_dmat3 3d6_4s2_5D.c cfg.001 c p

mult3 3d6_4s2_5D.c cfg.002 E1
bsr_dmat3 3d6_4s2_5D.c cfg.002  c p

mult3 3d6_4s2_5D.c cfg.003 E1
bsr_dmat3 3d6_4s2_5D.c cfg.003  c p

# bsr_phot3 klsp=1
# bsr_phot3 klsp=2
# bsr_phot3 klsp=3

# photo_tab < photo_tab.inp 

