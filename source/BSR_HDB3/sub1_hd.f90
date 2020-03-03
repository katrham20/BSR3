!=====================================================================
      Subroutine SUB1_HD    
!=====================================================================
!     calculations for given partial wave
!---------------------------------------------------------------------

      USE blacs

      USE bsr_hd
      USE target
      USE channel
      Use spline_param, only: ns
      
      Implicit none

! ... channels information:

      if(io_processor) then
       Call R_channel(nut,klsp)
       if(ncp.gt.0) ippert = ippert + ipconf(nch)
      end if

! ... check and print main parameters and file:

      if(io_processor) Call pri_mainpar

      Call br_ipar(fail); if(fail.ne.0) Return

!----------------------------------------------------------------------
! ... diagonalize the matrix and get inner-region solutions:

      Call Diag_hd

      Call br_ipar(fail); if(fail.ne.0) Return

!----------------------------------------------------------------------
! ... output of solutions and find the surface amplitudes:

      if(itype.ge.0)  Call rsol_out

!----------------------------------------------------------------------
! ... output of standard H.nnn file:

      if(itype.ge.0.and.io_processor) then

       ! read max. mutipole order and asymptotic coef.s:

       read(nui) lamax

       if(allocated(CF)) Deallocate(CF)
       Allocate (CF(kch,kch,lamax+1))
       read(nui) CF
       read(nui) RA     !  RM radius:

       if(itype.ge.0)  then
         if(iiexp.eq.0) Call H_out
         if(iiexp.ne.0) Call H_out1
       end if

      end if

      Call BLACS_BARRIER (ctxt, 'all')


!----------------------------------------------------------------------
! ... output of bound states in bound.nnn:

      if(itype.eq.-1)  Call B_out 

      End Subroutine SUB1_HD





