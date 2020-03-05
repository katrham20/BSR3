!======================================================================
!> @ingroup utilities
!> @brief     Change det.expansion
!======================================================================
!
!----------------------------------------------------------------------
      Implicit real(8) (A-H,O-Z)

      Integer(1), allocatable :: IS_NEED(:), JS_NEED(:), NS_NEED(:)

      Integer, allocatable :: ipt(:)

      Integer, allocatable :: IP_kt(:), IM_det(:,:), IS_det(:,:), Msym(:), Lsym(:)
      Real(8), allocatable :: C_det(:,:)

      Integer(8) :: ij8, ji8
      Integer(8), external :: DEF_ij8

      Integer, parameter :: ma=80
      Integer :: pri=66;  Character(ma) :: AF_pri   = 'expand_det.log'
      Integer :: ne1=11;  Character(ma) :: AF_det1  = 'det_exp1'
      Integer :: ne2=12;  Character(ma) :: AF_det2  = 'det_exp2'
      Integer :: nd1=13;  Character(ma) :: AF_done1 = 'det_done1'
      Integer :: nd2=14;  Character(ma) :: AF_done2 = 'det_done2'

      Call Read_aarg('det1',AF_det1)
      Call Read_aarg('det2',AF_det2)  
      Call Read_aarg('done1',AF_done1)
      Call Read_aarg('done2',AF_done2)                              

      Call Check_file(AF_det1)
      Call Check_file(AF_done1)

      Open(ne1,file=AF_det1,form='UNFORMATTED')
      Open(ne2,file=AF_det2,form='UNFORMATTED')
      Open(nd1,file=AF_done1,form='UNFORMATTED')
      Open(nd2,file=AF_done2,form='UNFORMATTED')

      nua = 91
      Open(nua,form='UNFORMATTED',status='SCRATCH')

! ... read done cases:

      read(nd1) ic_case
      Open(pri,file=AF_pri)
      write(pri,'(a,T20,i10)') 'ic_case =',ic_case

!      ij=DEF_ij(ic_case,ic_case)
      ij8=DEF_ij8(ic_case,ic_case)

      Allocate(IS_NEED(ij8))
      read(nd1) IS_NEED

      Allocate(JS_NEED(ic_case))
      JS_NEED=0

      Do i=1,ic_case
       Do j=1,i
        ij8 = DEF_ij8(i,j)
        if(IS_NEED(ij8).eq.0) Cycle 
        JS_NEED(i)=1
        JS_NEED(j)=1
       End do
      End do

      Close(nd1)

! ... expand needed det expansins: 

      ne=0;      Call Read_iarg('ne',ne)
      write(pri,'(/a,T20,i10)') 'ne    =',ne
      if(ne.eq.0) Stop 'Stop: ne = 0'
      Allocate(Msym(ne),Lsym(ne))

      mkt=100;   Call Read_iarg('mkt',mkt)
      mkdt=1000; Call Read_iarg('mkdt',mkdt)

      write(pri,'(/a,T20,i10)') 'mkt  =',mkt
      write(pri,'( a,T20,i10)') 'mkdt =',mkdt

      nc_case = 0
      Do is = 1,ic_case
   
       Read(ne1) ic,kt,kdt,ILT,IST,MLT,MST

       if(Allocated(IP_kt)) Deallocate(IP_kt)
       Allocate(IP_kt(kt)); Read(ne1) IP_kt

       if(Allocated(C_det)) Deallocate(C_det)
       Allocate(C_det(kt,kdt)); Read(ne1) C_det

       if(Allocated(IM_det)) Deallocate(IM_det)
       Allocate(IM_det(ne,kdt)); Read(ne1) IM_det

       if(Allocated(IS_det)) Deallocate(IS_det)
       Allocate(IS_det(ne,kdt)); Read(ne1) IS_det

       read(ne1) Msym(1:ne)
       read(ne1) Lsym(1:ne)

       if(JS_need(is).eq.0) Cycle

       Do k = 1,kt,mkt 
         kk = k+mkt-1; if(kk.gt.kt) kk=kt; ktn=kk-k+1
        Do m = 1,kdt,mkdt 
          mm = m+mkdt-1; if(mm.gt.kdt) mm=kdt; kdtn=mm-m+1
          nc_case=nc_case+1
          write(ne2) ic,ktn,kdtn,ILT,IST,MLT,MST
          write(ne2) IP_kt(k:kk)
          write(ne2) C_det(k:kk,m:mm)
          write(ne2) IM_det(1:ne,m:mm)
          write(ne2) IS_det(1:ne,m:mm)
          write(ne2) Msym(1:ne)
          write(ne2) Lsym(1:ne)
          write(nua) is 
        End do
       End do

      End do  ! is
      Close(ne1)
      write(ne2) nc_case
      Close(ne2)
      write(pri,'(/a,T20,i10)') 'nc_case =',nc_case

      Allocate(ipt(nc_case))
      rewind(nua)
      Do i=1,nc_case; read(nua) ipt(i); End do

      ij8=DEF_ij8(nc_case,nc_case)
      Allocate(NS_NEED(ij8))
      NS_NEED = 0

      k = 0
      Do i=1,nc_case; ii = ipt(i)
       Do j=1,i; jj = ipt(j) 
        ij8=DEF_ij8(i,j)
        ji8=DEF_ij8(ii,jj)
        if(IS_NEED(ji8).eq.0) Cycle
        NS_NEED(ij8)=1
        k = k + 1
       End do
      End do

      write(nd2) nc_case
      write(nd2) NS_NEED
      Close(nd2) 

      write(pri,'(/a,T20,2i10)') 'NS_NEED =>',k, SUM(NS_NEED)

      End ! program








