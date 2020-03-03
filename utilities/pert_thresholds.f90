!----------------------------------------------------------------------
!     define  thresholds.### if any
!----------------------------------------------------------------------
      Implicit real(8) (A-H,O-Z)

      Real(8), allocatable :: E_exp(:), E_pert(:)
      Integer, allocatable :: ip_pert(:)
      Character(80) :: AF_targ='target'
      Character(80) :: AF_exp='thresholds'
      Character(200) :: AF

      nut = 1
      if(Icheck_file(AF_targ).eq.0) Stop 'Stop: no target file' 
      open(nut,file=AF_targ)
      nlsp=0;  Call Read_ipar(nut,'nlsp',nlsp)
      if(nlsp.eq.0) Stop 'Stop: nlsp = 0'
      kpert=0; Call Read_ipar(nut,'kpert',kpert)
      if(kpert.eq.0) Stop 'Stop: kpert = 0'

      Allocate(ip_pert(kpert),  E_exp(kpert), E_pert(kpert) )
      read(nut,*)
      E_exp = 0.d0
      E_pert = 0.d0
      nup = 2
      Do i = 1, kpert
       read(nut,*) ip_pert(i), AF
       AF = trim(AF)//'.c'
       open(nup,file=AF)
       read(nup,'(15x,f16.8)') E_pert(i)
       Call Read_rpar(nup,'E_exp',E_exp(i))
       close(nup)
      End do 

      Do ilsp=1,nlsp
       if(count(ip_pert.eq.ilsp).eq.0) Cycle
       write(AF,'(a,a,i3.3)') trim(AF_exp),'.',ilsp
       open(nup,file=AF)
       Do i=1,kpert
        if(ip_pert(i).ne.ilsp) Cycle
        write(nup,'(3f16.8)') E_exp(i)-E_pert(i),E_pert(i),E_exp(i)
       End do 
       close(nup)
      End do

      End ! program



      
                


      