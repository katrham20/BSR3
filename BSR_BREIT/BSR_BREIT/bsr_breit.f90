!=====================================================================
!     PROGRAM   B S R _ B R E I T                        b10 -> v.10
!
!               C O P Y R I G H T -- 2004
!
!     Written by:   Oleg Zatsarinny
!                   email: oleg_zoi@yahoo.com
!======================================================================
!
!    generates angular coefficient in non-orthogonal mode 
!
!----------------------------------------------------------------------
!
!    INPUT ARGUMENTS:
!    
!    klsp1,klsp2  - range of partial wave in BSR calculations,
!                   then cfg.001, cfg.002, ..., are input files
!                   (default -> 0, with input file is cfg.inp)
!   
!    oper  - character(7), where each position can be 0 or 1,
!            and indicate the operator under consideration:
!            oper(1) - OVERLAPS       
!            oper(2) - KINATIC ENERGY
!            oper(3) - TWO-ELECTRON ELECTROSTATIC
!            oper(4) - SPIN ORBIT       
!            oper(5) - SPIN-OTHER-ORBIT
!            oper(6) - SPIN-SPIN
!            oper(7) - ORBIT-ORBIT       
!            Default -> 1110000 - non-relativistic calculations
!
!    mk    - max.multipole index (default -> 9, see module param_br)
!
!    nzero - zero-order dimension (default -> 0, all configurations)
!                                 (not used at the moment !)                                 
!
!----------------------------------------------------------------------
!
!    example:    1.  bsr_breit 
!                2.  bsr_breit klsp1=1 klsp2=5 oper=1111110  
!                3.  bsr_breit km=5
!            
!    last version of bsr_breit -> b10
!
!----------------------------------------------------------------------
!
!    INPUT FILES:
!    
!    cfg.###     -  configuration list for partial wave ### = klsp
!                   (cfg.inp in case klsp = 0)
!                  
!    int_bnk.### -  input data bank for angular coefficients
!                   (optional; int_bnk in case klsp = 0)
!                   
!    
!    OUTPUT FILES:
!    
!    int_bnk.###  - output data bank for angular coefficients
!                   (int_bnk in case klsp = 0)
!                   
!---------------------------------------------------------------------     

      USE param_br; USE inout_br; USE configs; USE inter  
      USE det_list; USE def_list; USE coef_list

      Implicit none 
      Integer(4) :: klsp
      Integer(4) :: i, ii, nc
      Real(8) :: time, t1,t2,tt, adet,adef
      Real(8), External :: RRTC
 
      Character AS*80

!----------------------------------------------------------------------

! ... output HEADER:

      Call open_br(pri,0) 
      write(pri,'(/20x,a/20x,a/20x,a/)') &
             '=======================',     &
             ' B R E I T - P A U L I ',     &
             '======================='

! ... read arguments from command line:

      Call Read_arg 

!----------------------------------------------------------------------
!                                             cycle over partial waves:
      time = 0.d0
      Do klsp = klsp1,klsp2

       write(pri,'(80(''-''))') 
       write(pri,'(/a,i5/)') ' Partial wave: ',klsp
       if(klsp.gt.0) write(*,'(/a,i5/)') ' Partial wave: ',klsp
       t1 = RRTC()

! ... open relevant files: 

       Call open_br(nuc,klsp)       ! c-file
       Call open_br(nub,klsp)       ! data bank results, if any
       Call open_br(nur,klsp)       ! new results
       Call open_br(nui,klsp)       ! intermediate results

       if(new) write(pri,'(a/)') ' It is new calculations '
       if(.NOT.new) write(pri,'(a/)') ' It is continued calculations '

! ...  read the configuration list:

       Call R_conf

       if(.not.icalc) then; Close(nur,STATUS='DELETE'); Cycle; end if        

! ...  extract old results:

       if(new) then

        ndet=0; Call Alloc_det(idet)
        ndef=0; Call Alloc_def(idef)

       else

        read(nub) ndet,ii; jdet = ii/ndet + 1
        Call Alloc_det(ndet+idet)
        Call R_i4(nub,mrecl,ndet,KPD)  ! read(nub) KPD(1:ndet)
        Call R_i4(nub,mrecl,ndet,IPD)  ! read(nub) IPD(1:ndet)
        Call R_i4(nub,mrecl,ii,  NPD)  ! read(nub) NPD(1:ii)

        read(nub) ndef,ii; jdef = ii/ndef + 1
        Call Alloc_def(ndef+idef)
        Call R_i4(nub,mrecl,ndef,KPF)  ! read(nub) KPF(1:ndef)
        Call R_i4(nub,mrecl,ndef,IPF)  ! read(nub) IPF(1:ndef)
        Call R_i4(nub,mrecl,ii,  NPF)  ! read(nub) NPF(1:ii)

        Call RW(nub,nui,ntotc)

        write(pri,'(/a/)') &
          ' Results for the old symmetry calculations:'
        write(pri,'(/3(a,i8))') &
          ' ndet =',ndet,'   ndef  =',ndef,'  ncoef =',ntotc

       end if

! ... prepare det. expantions:

      Call open_br(nua,0); Call open_br(nud,0)
 
      Call Pre_detexp 

! ... calculations for new angular symmetries:

       Call Conf_loop 

! ...  record results:

       ii = nsymt*(nsymt+1)/2
       write(nur) ii
       Call W_i1(nur,mrecl,noper,ii,IT_oper) ! write(nur) (IT_oper(:,i),i=1,ii)

       ii=IPD(ndet)+KPD(ndet); adet=ii; adet=adet/ndet
       write(nur) ndet,ii
       Call W_i4(nur,mrecl,ndet,KPD)  ! write(nur) KPD(1:ndet)
       Call W_i4(nur,mrecl,ndet,IPD)  ! write(nur) IPD(1:ndet)
       Call W_i4(nur,mrecl,ii  ,NPD)  ! write(nur) NPD(1:ii)

       ii=IPF(ndef)+KPF(ndef); adef=ii; adef=adef/ndef
       write(nur) ndef,ii
       Call W_i4(nur,mrecl,ndef,KPF)  ! write(nur) KPF(1:ndef)
       Call W_i4(nur,mrecl,ndef,IPF)  ! write(nur) IPF(1:ndef)
       Call W_i4(nur,mrecl,ii  ,NPF)  ! write(nur) NPF(1:ii)

       rewind(nui);  Call RW(nui,nur,nc)

! ...  print the main dimensions:      

       write(pri,'(/a/)') &
          ' Results for new angular symmetry calculations:'
       write(pri,'(a,i10,f10.1))') &
          ' number of overlap determinants =', ndet,adet
       write(pri,'(a,i10,f10.1))') &
          ' number of overlap factors      =', ndef,adef 
       write(pri,'(a,i10,f10.1))') &
          ' total number of coeff.s        =', ntotc

! ...  rename new results as new data bank (int_res -> int_bnk): 
 
       close(nui); close(nur); close(nub)
       Do i = 1,80; AS(i:i) = ' '; End do

       AS = 'move '; i = 5               !  Windows
!       AS = 'mv ';   i = 3               !  UNIX

       if(klsp.eq.0) then
        ii = LEN_TRIM(AF_r); AS(i+1:i+ii)=AF_r; i=i+ii+1
        ii = LEN_TRIM(AF_b); AS(i+1:i+ii)=AF_b; i=i+ii
       else
        ii = LEN_TRIM(BF_r); AS(i+1:i+ii)=BF_r; i=i+ii+1
        ii = LEN_TRIM(BF_b); AS(i+1:i+ii)=BF_b; i=i+ii
       end if

       Call System(AS(1:i))

! ... time for one partial wave:

       t2=RRTC(); tt=(t2-t1)/60
       write(pri,'(/a,F12.2,a)') ' Partial wave:',tt,' min'
       write(*,     '( a,F12.2,a)') ' Partial wave:',tt,' min'
       time = time + tt

      End do  ! over klsp
!----------------------------------------------------------------------

! ... total time:

      write(pri,'(a,F12.2,a)') ' Total time: ',time,' min'
      write(*,     '(a,F12.2,a)') ' Total time: ',time,' min'

      END ! Program Breit_bsr





!======================================================================
      Subroutine Read_arg 
!======================================================================
! 
!     read arguments from command line and check default settings
!
!======================================================================

      Use param_br;  Use inter; Use inout_br

      Implicit none

      Character(7) :: oper='1110000'	

! ... read arguments in command line:

      Call Read_iarg('klsp1' ,klsp1 )
      Call Read_iarg('klsp2' ,klsp2 )
      Call Read_iarg('nzero' ,nzero )
      Call Read_iarg('mk'    ,mk    )
      Call Read_iarg('is_soo',is_soo)
      Call Read_aarg('oper'  ,oper  )

      if(klsp2.lt.klsp1) klsp2=klsp1 

! ... define the operators under consideration:

      read(oper,'(7i1)') ioper
 
      write(pri,'(/a/)') ' Operators included: '

      if(ioper(1).gt.0) write(pri,'(a)') ' OVERLAPS'
      if(ioper(2).gt.0) write(pri,'(a)') ' KINATIC ENERGY'
      if(ioper(3).gt.0) write(pri,'(a)') ' TWO-ELECTRON ELECTROSTATIC'

      if(ioper(4).gt.0) write(pri,'(a)') ' SPIN ORBIT'
      if(ioper(5).gt.0) write(pri,'(a)') ' SPIN-OTHER-ORBIT'
      if(ioper(6).gt.0) write(pri,'(a)') ' SPIN-SPIN'
      if(ioper(7).gt.0) write(pri,'(a)') ' ORBIT-ORBIT'

      if(nzero.gt.0) write(pri,'(/a,i8/)') ' Zero order set =',nzero
      
      write(pri,'(/a,i3/)') ' Max.multipole index =',mk

      if(is_soo.ne.0) &
      write(pri,'(/a,i1/)') ' is_soo = ',is_soo, &
                            ' ->    Vk => V''k ' 
    
      End Subroutine Read_arg





!======================================================================
      Subroutine RW(nu1,nu2,nc)
!======================================================================
!
!     re-write bnk-data from file 'nu1' to 'nu2' by blocks
!
!----------------------------------------------------------------------

      Implicit none

      Integer(4), Intent(in) :: nu1,nu2
      Integer(4), Intent(out) :: nc
      Integer(4) :: i,j

      Integer(4),Parameter :: mc = 100000
      Integer(4),Allocatable,Dimension(:) :: K1,K2,K3
      Real(8),Allocatable,Dimension(:) :: C

      Allocate(C(mc),K1(mc),K2(mc),K3(mc))

      nc = 0
      i = 1
    1 read(nu1,end=2) c(i),k1(i),k2(i),k3(i)
      i = i + 1; if(i.le.mc) go to 1
    2 j = i - 1
      nc = nc + j

      Do i = 1,j
       write(nu2) c(i),k1(i),k2(i),k3(i)
      End do

      i = 1;  if(j.eq.mc) go to 1

      Deallocate(C,K1,K2,K3)

	  End Subroutine RW

