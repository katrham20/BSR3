!------------------------------------------------------------------
!> @ingroup utilities
!> @brief     Play with bsr_mat.nnn
!------------------------------------------------------------------
      Implicit real(8) (A-H,O-Z)

      Real(8), allocatable :: x(:,:)
      Character(80) :: AF


      Call Read_iarg('klsp',klsp)

      write(AF,'(a,i3.3)') 'bsr_mat.',klsp
      Call Check_file(AF)
      nu = 1; open(nu,file=AF,action='READ')
     
      read(nu) ns,nch,npert
      write(*,*) 'ns,nch,npert',ns,nch,npert
      allocate(x(ns,ns))
      if(npert.ne.0) Stop 'npert > 0'

       Do 

        read(nu) ic,jc
        if(ic.le.0) Exit
        if(ic.gt.nch.or.jc.gt.nch) Stop 'not channel-channel block'

        read(nu) x(:,:)

        S = Sum(abs(x)) 
        write(*,'(2i5,f20.10)')  ic,jc,S

       End do
         
      End program

