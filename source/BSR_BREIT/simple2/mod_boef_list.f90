!======================================================================
      MODULE boef_list
!======================================================================
!     Containes two-electron integrals for matrix elements
!     in uncouple nlmj-representation.
!     The coefficient in the list are recorded by blocks -
!     all integrals for all operators under concideration for
!     the given matrix element    < 1, 2 | O | 3, 4>
!     The operator O is defined by external procedure: Breit_coef 
!     This list is introduced to decrease the number of calls for
!     nj-symbol subroutine. 
!----------------------------------------------------------------------
      Implicit none

      Integer :: nboef = 0       ! number of integrals
      Integer :: mboef = 0       ! current dimension of list
      Integer :: iboef = 50000   ! initial dimension

! ... IB_int(1:mboef) - integral indentifier
! ... boef  (1:mboef) - correspondent angular coefficient

      Integer, allocatable :: IB_int(:)     
      Real(8), allocatable :: boef(:)

      Integer :: nblk  = 0       ! number of blocks
      Integer :: mblk  = 0       ! current dimension of list
      Integer :: iblk  = 5000    ! initial dimentsion
      Integer :: kblk  = 0       ! block under consideration

! ... identifiers of block:

      Integer, allocatable :: id1(:),id2(:),id3(:),id4(:) 

! ... current identifiers:

      Integer :: jd1,jd2,jd3,jd4    

! ... ncblk - pointer on the last element in the block
! ... ipblk - ordering pointer

      Integer, allocatable :: ipblk(:),ncblk(:) 

! ... incoding basis parameters, see subroutine check_boef:

      Integer, parameter :: ib14 = 2**14
      Integer, parameter :: ib28 = 2**28

      End MODULE BOEF_list


!======================================================================
      Subroutine alloc_boef(mm)
!======================================================================
!     allocate, deallocate or reallocate coefficient arrays IB_int and
!     Boef in module boef_list
!----------------------------------------------------------------------
      Use boef_list

      Implicit none
      Integer, intent(in)  :: mm
      Integer              :: m
      Integer, allocatable :: ia(:) 
      Real(8), allocatable :: ra(:) 

      m = mm; if(mm.lt.0) m=iboef
      if(m.le.0) then
       if(allocated(IB_int)) Deallocate(IB_int,Boef)
       mboef=0; nboef=0
      elseif(.not.allocated(IB_int)) then
       Allocate(IB_int(m), Boef(m));  mboef=m; nboef=0
      elseif(m.le.mboef) then
       Return
      elseif(nboef.eq.0) then
       Deallocate(IB_int,Boef)
       Allocate(IB_int(m), Boef(m));  mboef=m
      else
       Allocate(ra(nboef))
       ra(1:nboef)=boef(1:nboef); Deallocate(boef) 
       Allocate(boef(m)); boef(1:nboef)=ra(1:nboef)
       Deallocate(ra) 
       Allocate(ia(nboef))
       ia(1:nboef)=IB_INT(1:nboef); Deallocate(IB_INT) 
       Allocate(IB_INT(m)); IB_INT(1:nboef)=ia(1:nboef) 
       Deallocate(ia) 
       mboef=m
      end if

      End Subroutine alloc_boef


!======================================================================
      Subroutine alloc_blk(mm)
!======================================================================
!     allocate, deallocate or reallocate coefficient "block" arrays 
!     in module boef_list
!----------------------------------------------------------------------
      Use boef_list

      Implicit none
      Integer, intent(in)  :: mm
      Integer              :: m
      Integer, allocatable :: ia(:) 

      m = mm; if(mm.lt.0) m=iblk
      if(m.le.0) then
       if(allocated(ipblk)) Deallocate(ipblk,ncblk,id1,id2,id3,id4)
       mblk = 0; nblk = 0
      elseif(.not.allocated(ipblk)) then
       Allocate(ipblk(m),ncblk(0:m),id1(m),id2(m),id3(m),id4(m))
       mblk = m; nblk = 0; ncblk = 0
      elseif(m.le.mblk) then
       Return
      elseif(nblk.eq.0) then
       Deallocate(ipblk,ncblk,id1,id2,id3,id4)
       Allocate(ipblk(m),ncblk(0:m),id1(m),id2(m),id3(m),id4(m))
       mblk = m
      else
       Allocate(ia(0:nblk))
       ia(1:nblk)=ipblk(1:nblk); Deallocate(ipblk) 
       Allocate(ipblk(m)); ipblk(1:nblk)=ia(1:nblk) 
       ia(0:nblk)=ncblk(0:nblk); Deallocate(ncblk) 
       Allocate(ncblk(0:m)); ncblk(0:nblk)=ia(0:nblk) 
       ia(1:nblk)=id1(1:nblk); Deallocate(id1) 
       Allocate(id1(m)); id1(1:nblk)=ia(1:nblk) 
       ia(1:nblk)=id2(1:nblk); Deallocate(id2) 
       Allocate(id2(m)); id2(1:nblk)=ia(1:nblk) 
       ia(1:nblk)=id3(1:nblk); Deallocate(id3) 
       Allocate(id3(m)); id3(1:nblk)=ia(1:nblk) 
       ia(1:nblk)=id4(1:nblk); Deallocate(id4) 
       Allocate(id4(m)); id4(1:nblk)=ia(1:nblk) 
       Deallocate(ia) 
       mblk = m
      end if

      End Subroutine alloc_blk


!=======================================================================
      Subroutine Iadd_boef(C,int)   ! ???
!=======================================================================
!     add new integral to the coefficient list in module boef_list
!-----------------------------------------------------------------------
      Use boef_list

      Implicit none
      Integer, Intent(in) :: int
      Real(8), Intent(in) :: C

      if(mboef.eq.0.or.nboef.eq.mboef) Call Alloc_boef(mboef+iboef)

      nboef=nboef+1; Boef(nboef)=C; IB_int(nboef)=int

      End Subroutine Iadd_boef


!=======================================================================
      Subroutine Check_boef(l1,m1,s1,l2,m2,s2,l3,m3,s3,l4,m4,s4)
!=======================================================================
!     Check if we already have the m.e. for given orbitals,
!     otherwise - calculate them. 
!     Procedure uses "packing" the orbitals parameters, and that
!     restricts the max. l to 2**10=1024. 
!----------------------------------------------------------------------
      USE boef_list

      Implicit none
      Integer, intent(in) :: l1,m1,s1,l2,m2,s2,l3,m3,s3,l4,m4,s4
      Integer :: k,l,m,ipm 

      if(mblk.eq.0) Call Alloc_blk(iblk)

! ... prepare indentifiers:

      jd1 = s1*ib28+l1*ib14+l1+m1    
      jd2 = s2*ib28+l2*ib14+l2+m2    
      jd3 = s3*ib28+l3*ib14+l3+m3    
      jd4 = s4*ib28+l4*ib14+l4+m4    

! ... look for the same case in the list:

      k=1; l = nblk 
    1 if(k.gt.l) go to 2              
 
      m=(k+l)/2; ipm=ipblk(m)
 
      if(jd1.lt.id1(ipm)) then;      l = m - 1
      elseif(jd1.gt.id1(ipm)) then;  k = m + 1
      else

      if(jd2.lt.id2(ipm)) then;      l = m - 1
      elseif(jd2.gt.id2(ipm)) then;  k = m + 1
      else

      if(jd3.lt.id3(ipm)) then;      l = m - 1
      elseif(jd3.gt.id3(ipm)) then;  k = m + 1
      else

      if(jd4.lt.id4(ipm)) then;      l = m - 1
      elseif(jd4.gt.id4(ipm)) then;  k = m + 1

      else
       kblk = ipm
       Return
      end if; end if; end if; end if

      go to 1
    2 Continue 
    
! ... new block:    
            
      Call ee_coef(l1,m1,s1,l2,m2,s2,l3,m3,s3,l4,m4,s4)

      nblk = nblk + 1;  ncblk(nblk) = nboef; kblk = nblk   
      id1(nblk)=jd1; id2(nblk)=jd2; id3(nblk)=jd3; id4(nblk)=jd4
      
      if(k.eq.nblk) then
       ipblk(k)=nblk
      else
       Do m = nblk,k+1,-1; ipblk(m) = ipblk(m-1); End do
       ipblk(k)=nblk
      end if        

! ... it is time for re-allocation:

      if(nblk.eq.mblk) Call Alloc_blk(mblk+iblk)

      End Subroutine Check_boef











