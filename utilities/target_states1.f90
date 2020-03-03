!----------------------------------------------------------------------
!     target, thresholds --> target_states
!     provide information about target states and exp. thresholds
!     put E_exp in the target states
!----------------------------------------------------------------------
      Implicit real(8) (A-H,O-Z)
      Character(80), allocatable :: conf(:), name(:)
      Real(8), allocatable :: E_au(:), E_exp(:)
      Integer, allocatable :: J2(:), Ltarg(:), IStarg(:), IPtarg(:)
      Character(80) :: AF_inp='target'
      Character(80) :: AF_out='target_states'
      Character(80) :: AF_exp='thresholds'
      Character(200) :: AF, AF1,AF2
      Character(1) :: ap

      Call Read_aarg('inp',AF_inp)
      Call Check_file(AF_inp)
      inp=1; open(inp,file=AF_inp)

      ntarg=0; Call Read_ipar(inp,'ntarg',ntarg)
      if(ntarg.le.0) Stop 'ntarg <= 0'

      Allocate(E_au(ntarg), E_exp(ntarg), name(ntarg), conf(ntarg), J2(ntarg), &
               Ltarg(ntarg), IStarg(ntarg), IPtarg(ntarg))
      read(inp,*)
      Do i = 1, ntarg
       read(inp,*) name(i), conf(i), Ltarg(i),IStarg(i),IPtarg(i), E_au(i)
      End do
      if(IStarg(1).eq.0) J2 = Ltarg

      nexp = 3
      Call Read_aarg('exp',AF_exp)
      iexp = Icheck_file(AF_exp)

      if(iexp.ne.0)  then
        open(nexp,file=AF_exp)
        Do i = 1, ntarg
         read(nexp,*) E_exp(i)
        End do
        close(nexp)
      end if

      nu = 2;  J2 = 0
      Do i = 1, ntarg

       AF = trim(name(i))//'.c'
       open(nu, file = AF)
       rewind(nu)
       read(nu,'(a)') AF
       in = INDEX(AF,'=') 
       if(in.gt.0)  read(AF(in+1:),*) J2(i)
       conf(i) = ' '
       in = INDEX(AF,':') 
       if(in.gt.0)  read(AF(in+1:),*) conf(i)

       if(iexp.ne.0) then
        S = 0.d0
        Call Read_rpar(nu,'E_exp', S)
        Backspace(nu)
        write(nu,*)
        write(nu,'(a,f16.8)') 'E_exp = ',E_exp(i)
       else
        Call Read_rpar(nu,'E_exp', E_exp(i))
       end if

      End do

      Call Read_ipar(inp,'nz',nz);  Z=nz
      Call Conv_au (Z,AWT,au_cm,au_eV,0)      

      imax=0; jmax=0
      Do i = 1,ntarg
       ii = LEN_TRIM(name(i)); if(ii.gt.imax) imax=ii
       ii = LEN_TRIM(conf(i)); if(ii.gt.jmax) jmax=ii
      End do

      Call Read_aarg('out',AF_out)
      open(nu,file=AF_out)
      write(nu,'(a,i6)') 'ntarg =', ntarg
      write(nu,*)
      Do i = 1,ntarg
       E = E_au(i) - E_au(1)
       E_Ry = 2*E
       E_eV = E*au_eV
       E_cm = E*au_cm
       AF1 = name(i); AF2 = conf(i)
       write(nu,'(i4.4,3x,a,3x,a,2x,a,i3,3x,F20.8,F17.6,F15.5,F13.1,F20.8,f8.3)') &
          i, AF1(1:imax),AF2(1:jmax),'2J =',J2(i), E_au(i), E_Ry, E_ev, E_cm,  &
          E_exp(i), (E_exp(i)-E_au(i))*au_eV

      End do 
Stop
!------------------------------------------------------------------------------------
!     sort according J:

      it=1; jmin = mod(2*Ltarg(it)+1+IStarg(it),2)
      write(*,*) 'jmin =',jmin

      met = 0;  ns = 0

      Do ip = 1,-1,-2

      write(nu,*)
      write(nu,*)
      write(nu,*)

      Do J = jmin,jmin+20,2

       ks = 0
       Do it = 1,ntarg
        if(IPtarg(it).ne.ip) Cycle
        if(ITRI(2*Ltarg(it)+1, IStarg(it), J+1).eq.0) Cycle
        ns = ns + 1 
        ks = ks + 1
        if(ks.eq.1) met = met + 1
        write(nu,'(i5,3x,a40,i5)')  met, name(it), ns
       End do

      End do
      End do

! ... repeat for nlsp:

      met = 0;  ns = 0
      Do ip = 1,-1,-2
      write(nu,*)
      write(nu,*)
      Do J = jmin,jmin+20,2
       ks = ns
       Do it = 1,ntarg
        if(IPtarg(it).ne.ip) Cycle
        if(ITRI(2*Ltarg(it)+1, IStarg(it), J+1).eq.0) Cycle
        ns = ns + 1 
       End do
       if(ns.gt.ks) then
        met = met + 1
        ap = 'e'; if(ip.eq.-1) ap = 'o'
        write(nu,'(i2.2,a1,3i5)') J,ap,J,0,ip
       end if
      End do
      End do
 
      End ! program

      