#  test run for bound states and oscillator strengths
#  calculations in C

#  all input atomic states are supposed to obtained 
#  from MCHF package (1996 year version) and represented  as 
#  pare of name.c and name.w files

#  if you have problems with the unformatted w-files
#  due to uncompatable structure, first transform
#  the formated frm-files to w-file  by command
#             frm_w  name.frm
#  (corresponding frm-file are placed in the folder 'frm')

#--------------------------------------------------------------
#  preparation of target files:
#
#  supposed the HF or MCHF calculations were carried out for
#  2s2_2p, 2s_2p2, 2p3  and 2s_2p3 states (see file "target")
#  and we have corresponding w- and c-files
#
#  As preliminary step, transform all involved w-files 
#  to corresponding B_spline expansions (bsw-files);
#  (file knot.dat should be already prepared)

w_bsw 2s2_2p_2Po.w
w_bsw 2s_2p2_2D.w
w_bsw 2s_2p2_2P.w
w_bsw 2s_2p2_2S.w
w_bsw 2p3_2Po.w
w_bsw 2s_2p3_j0.w
w_bsw 2s_2p3_j1.w
w_bsw 2s_2p3_j2.w
w_bsw 2s_2p3_j3.w

# accuracy of the B-spline representation can be checked in w_bsw.log
#--------------------------------------------------------------
#  bound-state BSR calculations for excited odd states:

bsr_prep3
bsr_conf3

bsr_breit3 klsp1=1 klsp2=4 oper=1111000
bsr_mat3 klsp1=1 klsp2=4
bsr_hd3 klsp1=1 klsp2=4  

bound_tab

#--------------------------------------------------------------
#  preparation of initial state in BSR-format

#  suppose that the CI Breit-Pauli calculation was 
#  done for the initial 2s2/2p2 3P state and results are
#  in the files: 2s2_2p2_3P.c, 2s2_2p2_3P.w, 2s2_2p2_3P.j

#  prepare c-files for each J-value of the initial state:

cfile 2s2_2p2_3P.j 1 0 2s2_2p2_3P0.c 0.0
cfile 2s2_2p2_3P.j 1 2 2s2_2p2_3P1.c 0.0
cfile 2s2_2p2_3P.j 1 4 2s2_2p2_3P2.c 0.0

#  prepare bsw-files from w-files

w_bsw  2s2_2p2_3P0.w
w_bsw  2s2_2p2_3P1.w
w_bsw  2s2_2p2_3P2.w

#--------------------------------------------------------------
#  oscillator-strengths calculations:

mult3 2s2_2p2_3P.c cfg.001 E1
mult3 2s2_2p2_3P.c cfg.002 E1
mult3 2s2_2p2_3P.c cfg.003 E1
mult3 2s2_2p2_3P.c cfg.004 E1

# rm zf_res    

bsr_dmat3 2s2_2p2_3P0.c cfg.002 c b
bsr_dmat3 2s2_2p2_3P1.c cfg.001 c b
bsr_dmat3 2s2_2p2_3P1.c cfg.002 c b
bsr_dmat3 2s2_2p2_3P1.c cfg.003 c b
bsr_dmat3 2s2_2p2_3P2.c cfg.002 c b
bsr_dmat3 2s2_2p2_3P2.c cfg.003 c b
bsr_dmat3 2s2_2p2_3P2.c cfg.004 c b

#  final results are oscillator strengths in file "zf_res"
#  To check if the test runs correctly,
#  compare the f-values with data in zf_res.test

# fc zf_res zf_res.test > diff
diff zf_res zf_res.test > diff

