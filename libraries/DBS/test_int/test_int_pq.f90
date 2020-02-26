!======================================================================
      PROGRAM test_int
!======================================================================
!     Testing two-electron integrals: list of desirable integrals are
!     supposed to be in name.int file, the corresponding one-electron
!     orbitals - in name.bsw.
!     Results - in name.test 
!     B-splines requires knot.dat file
!----------------------------------------------------------------------

      Use DBS_grid
      Use DBS_nuclear, z => atomic_number
      Use DBS_gauss
      Use DBS_orbitals_pq

      Implicit real(8) (A-H,O-Z)

      Character(80) :: name,AF
      Integer :: nui=2; Character(80) :: AF_int = 'int.inp'
      Integer :: nut=3; Character(80) :: AF_out = 'int.out'

! ... define B-splines:

      Call def_grid('knot.dat', ' ',1.d0,1.d0)
      Call alloc_DBS_gauss

! ... open files:

      Call Read_aarg('AF_int',AF_int)
      Call Read_aarg('AF_out',AF_out)

! ... generate 

      nn=3;  Call Read_iarg('nn',nn) 

      Do n=1,nn
       Do k = -n,n-1
        if(k.eq.0) Cycle
        i = Ifind_bsorb(n,k,0,2)
        CALL bdcwf_pq(n,k,z,pq(1,1,i),pq(1,2,i)); mbs(i)=ns
       End do
      End do

      kmax = maxval(jbs(1:nbf))+1

      Call alloc_DBS_integrals(ns,ks,-1,kmax,6)

!      Call Print_cwf_pq

      Open(nui,file=AF_int)
      Open(nut,file=AF_out)
    
      write(nut,'(a,i5)') 'ksp =',ksp
      write(nut,'(a,i5)') 'ksq =',ksq
      write(nut,*) 

      write(nut,*) 'number of radial functions:',nbf
      write(nut,'(10a5)') EBS(1:nbf)
      write(nut,*) 

      CALL test_integrals(nui,nut)

      END PROGRAM test_int



!========================================================================
      SUBROUTINE test_integrals(nui,nut)
!========================================================================
!     tests the given set of radial integrals
!     The integrals are given in unit nui, results - in unit nut.
!------------------------------------------------------------------------

      IMPLICIT REAL(8) (A-H,O-Z)

      Character(1) :: INT
      Character(80) :: AS
      Character( 5) :: EL1, EL2, EL3, EL4

      INTEGER(4) :: i,k, in,iout

      INTEGER(4) :: n1,n2,n3,n4
      INTEGER(4) :: l1,l2,l3,l4
      INTEGER(4) :: k1,k2,k3,k4
      INTEGER(4) :: i1,i2,i3,i4
      INTEGER(4) :: ie1,ie2,ie3,ie4

      REAL(8) :: S, SS, SA,SB, S1, S2

      REAL(8) :: T1,T2

      INTEGER(4), EXTERNAL :: Ifind_bsorb
      REAL(8), EXTERNAL :: RRTC

!----------------------------------------------------------------------
! ... loop over input strings

    1 read(nui,'(a)',end=2) AS
      if(AS(1:1).eq.'*') go to 2          ! end of data
      if(LEN_TRIM(AS).eq.0) go to 1
!----------------------------------------------------------------------
      if(AS(1:1).eq.'#') then             ! timing block                 
       go to 1
      end if                              ! end of timing mode (#)

!----------------------------------------------------------------------
! ... test of accuracy:

      Read(AS,'(a1,i2,4(1x,a5))') int,k,el1,el2,el3,el4
      m=0
      if(int.eq.'R') m=1
      if(int.eq.'S') m=2
      if(m.eq.0) go to 1

      Call EL_NLJK(el1,n1,k1,l1,j1,i1);  ie1=Ifind_bsorb(n1,k1,i1,0)
      if(ie1.eq.0) go to 1
      Call EL_NLJK(el2,n2,k2,l2,j2,i2);  ie2=Ifind_bsorb(n2,k2,i2,0)
      if(ie2.eq.0) go to 1
      Call EL_NLJK(el3,n3,k3,l3,j3,i3);  ie3=Ifind_bsorb(n3,k3,i3,0)
      if(ie3.eq.0) go to 1
      Call EL_NLJK(el4,n4,k4,l4,j4,i4);  ie4=Ifind_bsorb(n4,k4,i4,0)
      if(ie4.eq.0) go to 1

      i = INDEX(AS,'='); if(i.eq.0) go to 1
      read(AS(i+1:),*) SA

      write(nut,'(a,d24.16)') AS(1:30),SA
      write(*  ,'(a,d24.16)') AS(1:30),SA

!----------------------------------------------------------------------
      if(INT.eq.'R') then

       S = rk_pq (ie1,ie2,ie3,ie4,k);  if(SA.eq.0) SA=S
       SS = (S-SA)/SA
       write(nut,'(30x,d24.16,d12.3,5x,a)') s,ss,'  rk1(.,a;.,b)'
       write(*  ,'(30x,d24.16,d12.3,5x,a)') s,ss,'  rk1(.,a;.,b)'

      end if

!----------------------------------------------------------------------

      if(INT.eq.'S') then

       S = sk_ppqq (ie1,ie2,ie3,ie4,k); if(sa.eq.0.d0) SA=s

       SS = (S-SA)/SA

       write(nut,'(30x,d24.16,d12.3,5x,a)') s,ss,'  sk_ppqq'
       write(*  ,'(30x,d24.16,d12.3,5x,a)') s,ss,'  sk_ppqq'

      end if


      go to 1    ! go to new integral

    2 Continue   ! end of data 

      END SUBROUTINE test_integrals



