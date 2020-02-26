!======================================================================
!     Utility program   divide_bnk:   int_bnk -> int_inf + int_int
!======================================================================
      Use symc_list_LS
      Use symt_list_LS
      Use det_list
      Use def_list

      Implicit real(8) (A-H,O-Z)

      Integer :: nub=1;  Character(20) :: AF_bnk = 'int_bnk'
      Integer :: nuf=2;  Character(20) :: AF_inf = 'int_inf'
      Integer :: nui=3;  Character(20) :: AF_int = 'int_int'

      Integer :: mc = 10000000,  nc = 0
      Integer :: mode = 1

      Real(8) :: t1,t2

      Call cpu_time(t1)

      Call Read_aarg('bnk',AF_bnk)
      Call Read_aarg('inf',AF_inf)
      Call Read_aarg('int',AF_int)

      Call Read_iarg('mc',mc)
      Call Read_iarg('mode',mode)

      Call Check_file(AF_bnk)
      Open(nub,file=AF_bnk,form='UNFORMATTED')
      Open(nuf,file=AF_inf,form='UNFORMATTED')
      Open(nui,file=AF_int,form='UNFORMATTED')

      Call Read_symc_LS(nub)
      Call Write_symc_LS(nuf)
      write(*,*) 'nsymc = ',nsymc
      Call cpu_time(t2)
      write(*,'(a,f10.2,a)') 'symc: ',(t2-t1)/60,' min'

      Call Read_symt_LS(nub)
      Call Write_symt_LS(nuf)
      write(*,*) 'nsymt = ',nsymt
      Call cpu_time(t2)
      write(*,'(a,f10.2,a)') 'symt: ',(t2-t1)/60,' min'

      Call Alloc_it_oper_LS(1)
      Call Read_oper_LS(nub)
      Call Record_oper_LS(nuf)
      Call cpu_time(t2)
      write(*,*) 'oper: ',ij,ij_oper
      write(*,'(a,f10.2)') 'mem_oper:', mem_oper 
      write(*,'(a,f10.2,a)') 'oper: ',(t2-t1)/60,' min'

      Call Read_det(nub)
      Call Write_det(nuf)
      write(*,*) 'dets:',ndet,ldet,jmdet
      Call cpu_time(t2)
      write(*,'(a,f10.2,a)') 'dets: ',(t2-t1)/60,' min'

      Call Read_def(nub)
      Call Write_def(nuf)
      write(*,*) 'defs:',ndef,ldef,jmdef
      Call cpu_time(t2)
      write(*,'(a,f10.2,a)') 'defs: ',(t2-t1)/60,' min'

    rewind(nuf)

      Call Read_symc_LS(nuf)
      write(*,*) 'nsymc = ',nsymc

      Call Read_symt_LS(nuf)
      write(*,*) 'nsymt = ',nsymt

      nsymt=2*nsymt
      Call Alloc_it_oper_LS(1)
      Call Load_oper_LS(nuf)
      write(*,*) 'oper: ',ij,ij_oper
      write(*,'(a,f10.2)') 'mem_oper:', mem_oper 

      Call Load_det(nuf)
      write(*,*) 'dets:',ndet,ldet,jmdet

      Call Load_def(nuf)
      write(*,*) 'defs:',ndef,ldef,jmdef

      End ! Program divide_bnk



!======================================================================
      Subroutine RW1(nu1,nu2,mc,nc)
!======================================================================
!     re-write bnk-data from file 'nu1' to 'nu2' by blocks
!----------------------------------------------------------------------
      Implicit none

      Integer, intent(in) :: nu1,nu2,mc
      Integer, intent(out) :: nc
      Integer :: i,j

      Integer, allocatable :: K1(:),K2(:),K3(:),K4(:)
      Real(8), allocatable :: C(:)

      Allocate(C(mc),K1(mc),K2(mc),K3(mc),k4(mc))

    1 Do i = 1,mc
       read(nu1,end=2) c(i),k1(i),k2(i),k3(i),k4(i) 
      End do
      i = mc+1
    2 j = i-1 
      nc = nc + j

      write(nu2) j
      write(nu2) c(1:j)
      write(nu2) k1(1:j)
      write(nu2) k2(1:j)
      write(nu2) k3(1:j)
      write(nu2) k4(1:j)

      if(j.eq.mc) go to 1

      Deallocate(C,K1,K2,K3,k4)

      End Subroutine RW1


!======================================================================
      Subroutine RW0(nu1,nu2,mc,nc)
!======================================================================
!     re-write bnk-data from file 'nu1' to 'nu2' by blocks
!----------------------------------------------------------------------
      Implicit none

      Integer, intent(in) :: nu1,nu2,mc
      Integer, intent(out) :: nc
      Integer :: i,j,k,m, ibc = 2**15

      Integer, allocatable :: K1(:),K2(:),K3(:),K4(:)
      Real(8), allocatable :: C(:)

      nc = 0
      Allocate(C(mc),K1(mc),K2(mc),K3(mc),k4(mc))

    1 Do i = 1,mc
       read(nu1,end=2) c(i),k1(i),k3(i),k4(i) 
      End do
      i = mc+1
    2 j = i-1 
      nc = nc + j

      Do i = 1,j
       k = k1(i); k1(i)=k/ibc; k2(i) = mod(k,ibc)
      End do

      write(nu2) j
      write(nu2) c(1:j)
      write(nu2) k1(1:j)
      write(nu2) k2(1:j)
      write(nu2) k3(1:j)
      write(nu2) k4(1:j)

      if(j.eq.mc) go to 1

      Deallocate(C,K1,K2,K3,k4)

      End Subroutine RW0




