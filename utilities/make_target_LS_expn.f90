!----------------------------------------------------------------------
!     make target_LS_expn  using information in the end of c-file
!     (see npert =  flag)
!     this information if any is created by 'bound_bsw' program 
!----------------------------------------------------------------------
      Use target

      Implicit real(8) (A-H,O-Z)

      Integer :: nut = 1; Character(80) :: AF_targ = 'target'
      Integer :: nur = 2; Character(80) :: AF_expn = 'target_LS_expn'
      Integer :: nuc = 3; Character(80) :: AF_c    = 'name.c'
      Real(8), allocatable :: C_LS(:,:)
      Real(8) :: eps = 5E-8


! ... read target information:

      Call Check_file(AF_targ)
      Open(nut,file=AF_targ)
      Call R_target(nut)

      Call Read_rarg('eps',eps)
      ntarg_LS = 0
      Call Read_iarg('ntarg_LS',ntarg_LS)

! ... open target_LS_expn

      Open(nur,file=AF_expn)

      m = 0
      Do it = 1,ntarg
       write(AF_c,'(a,a)') trim(BFT(it)),'.c'
       Call Check_file(AF_c)
       Open(nuc,file=AF_c)
       npert = 0
       Call Read_ipar(nuc,'npert',npert)
       if(npert.eq.0) then
        write(*,*) 'no expansion for target', it, '   ', AF_c
        Stop
       end if

       read(nuc,*)
       write(nur,'(a,i5,a,i5)') 'target',it,'  npert =',npert
       CC = 0.d0
       Do ip = 1,npert
        read(nuc,*) i,C
        write(nur,'(i6,f16.8)') i,C
        m = m + 1
        CC = CC + C*C
       End do
       S = abs(CC - 1.d0)
       if(S.gt.eps)   write(*,'(i5,f12.8)') it,CC-1.d0

      End do
      write(nur,'(/a,i6)') 'mpert = ', m
 
!-----------------------------------------------------------------------------      
      if(ntarg_LS.eq.0) Stop 'ntarg_LS = 0 -> no orth. checking'

      Allocate(C_LS(ntarg,ntarg_LS))
      C_LS = 0.d0

      Do it = 1,ntarg
       write(AF_c,'(a,a)') trim(BFT(it)),'.c'
       Open(nuc,file=AF_c)
       npert = 0
       Call Read_ipar(nuc,'npert',npert)
       read(nuc,*)
       CC = 0.d0
       Do ip = 1,npert
        read(nuc,*) i,C
        C_LS(it,i) = C
       End do
      End do

      Do it = 1,ntarg
       Do jt = 1,it-1
        if(Jtarg(it).ne.Jtarg(jt)) Cycle
         S = SUM(C_LS(it,:)*C_LS(jt,:)) 
         if(S.gt.eps)  write(*,'(2i5,f12.8)') jt,it,S
       End do
      End do

      End ! program

