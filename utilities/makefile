#     BSR  utils

include ${HOME}/DEFS_04

all:  bound_bsw  bound_tab  bsw_tab    bsw_w      bsw123  \
      photo_tab  w_bsw  zf_cc_bsr  print_bs  divide_bnk

# extract seprate states from "bound.nnn" file:
bound_bsw: bound_bsw.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCONFLS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# list of states in the "bound.nnn" files:
bound_tab: bound_tab.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)

check_matrix: check_matrix.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)

# hydrogen-like bound and pseudocontinuum states for given B-soline basis:
bs_hwf: bs_hwf.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# connverts the wbs-files with orbitals in B-spline repersentation 
# into tab-files suitable for grafic display
bsw_tab: bsw_tab.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# connvert the B-spline results in the MCHF format
bsw_w:  bsw_w.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# merging  1.bsw + 2.bsw + 3.bsw + ... --> res.bsw
bsw123: bsw123.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# different photoionization paramters
photo_collect: photo_collect.f90
	$(FC) -o $@ $(link) $<  $(ZCOM) 
	mv $@ $(BINDIR)


# different photoionization paramters
photo_tab: photo_tab.f90
	$(FC) -o $@ $(link) $<  $(ZCONFLS) $(ZCOM) 
	mv $@ $(BINDIR)

photo_tab_new: photo_tab_new.f90
	$(FC) -o $@ $(link) $<  $(ZCONFLS) $(ZCOM) 
	mv $@ $(BINDIR)


# prints B-splines in separate files:
print_bs: print_bs.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

# Generate B-spline representation for orbitals 
# given in MCHF package format, w-files 
w_bsw: w_bsw.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK) -Bstatic
	mv $@ $(BINDIR)

#  f-value calculations between states in bound.nnn files
zf_cc_bsr: zf_cc_bsr.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

#  f-value calculations between set of states in the c- and bsw-files
zf_bb_bsr: zf_bb_bsr.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)


#  int_bnk -> int_inf + int_int 
divide_bnk: divide_bnk.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)


target_states: target_states.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)


target_states1: target_states1.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)

target_states2: target_states2.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

# prints B-splines in separate files:
test_BS_Cint: test_BS_Cint.f90
	$(FC) -o $@ $(link) $< $(BS) $(ZCOM) $(LAPACK)
	mv $@ $(BINDIR)

pert_thresholds: pert_thresholds.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)

convert_cfg: convert_cfg.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

make_target_LS_expn: make_target_LS_expn.f90
	$(FC) -o $@ -fbacktrace -fcheck=bounds $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

f_values: f_values.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

prepare_detexp: prepare_detexp.f90 
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(SHELLS) $(ZCOM)
	mv $@ $(BINDIR)

s_values: s_values.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

expand_det: expand_det.f90
	$(FC) -o $@ $(link) $<  $(ZCOM)
	mv $@ $(BINDIR)

dd_bsr: dd_bsr.f90
	$(FC) -o $@ $(link) $< $(ZCONFLS) $(ZCOM)
	mv $@ $(BINDIR)

rsol_collect: rsol_collect.f90
	$(FC) -o $@ $(link) $< $(ZCOM)
	mv $@ $(BINDIR)
