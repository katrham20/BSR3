!======================================================================
      Subroutine Check_file(AF)
!======================================================================
!     stop with message if given file AF does not exist
!----------------------------------------------------------------------
      Character(*), Intent(in) :: AF
      Logical :: EX
      Inquire(file=AF,exist=EX)
      if(.not.EX) then
       write(*,*) ' can not find file  ',AF;  Stop
      end if
      End Subroutine Check_file
       

!======================================================================
      Integer Function Icheck_file(AF)
!======================================================================
!     check if the file AF exists
!----------------------------------------------------------------------
      Character(*), Intent(in) :: AF
      Logical :: EX
      Inquire(file=AF,exist=EX)
      Icheck_file = 1
      if(.not.EX) Icheck_file = 0
      End Function Icheck_file

       
!======================================================================
      Subroutine Find_free_unit(nu)
!======================================================================
!     provide free unit to open new file
!----------------------------------------------------------------------	        
      Implicit none
      Integer :: nu,i
      Logical :: connected
      nu = 0
      Do i=21,999
       INQUIRE(UNIT=i,OPENED=connected)
       if(connected) Cycle
       nu = i
       Exit
      End do
      if(nu.eq.0) Stop 'Find_free_unit: nu = 0'
      End Subroutine Find_free_unit
