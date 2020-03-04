!======================================================================
      Integer Function Ifind_channel_serial(ic)
!======================================================================
!> @ingroup bsr_mat
!> @brief     find channel (or perturber) for given configuration 'ic'
!----------------------------------------------------------------------
      Use target; Use channel

      Implicit none
      Integer, intent(in) :: ic
      Integer :: ich

      if(ic.le.0) Stop 'Ifind_channel_serial: ic <= 0'
      Ifind_channel_serial = 1
      Do ich = 1,nch
       if(ic.gt.ipconf(ich)) Ifind_channel_serial=ich+1
      End do 

      if(Ifind_channel_serial.le.nch) Return

      Do ich = 1,npert
       if(ic.gt.ippert(ich)) Ifind_channel_serial=nch+ich+1   
      End do 

      if(Ifind_channel_serial.gt.nch+npert) then
       write(*,*) 'npert=',npert
       write(*,*)  ippert(0:npert)
       Stop 'Ifind_channel_serial: configuration index, ic, is to big'
      end if

      End Function Ifind_channel_serial
