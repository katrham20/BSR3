!======================================================================
      Subroutine R_arg(nu)
!======================================================================
!     read arguments, first from file unit 'nu, then from comand line
!----------------------------------------------------------------------
      Use bsr_hd
      Use target,        only: ntarg, Etarg
      Use spline_atomic, only: z
    
      Implicit none
      Integer, intent(in) :: nu
      Integer :: i,it

! ... read parameters from file if any

      Call Read_ipar(nu,'klsp'  ,klsp  )
      Call Read_ipar(nu,'klsp1' ,klsp1 )
      Call Read_ipar(nu,'klsp2' ,klsp2 )
      Call Read_ipar(nu,'itype' ,itype )
      Call Read_ipar(nu,'iexp'  ,iexp  )
      Call Read_ipar(nu,'msol'  ,msol  )
      Call Read_ipar(nu,'jmvc'  ,jmvc  )
      Call Read_ipar(nu,'it_max',it_max)
      Call Read_rpar(nu,'Emin'  ,Emin  )
      Call Read_rpar(nu,'Emax'  ,Emax  )
      Call Read_rpar(nu,'Edmin' ,Edmin )
      Call Read_rpar(nu,'Edmax' ,Edmax )
      Call Read_rpar(nu,'Egap'  ,Egap  )
      Call Read_ipar(nu,'iwt'   ,iwt   )
      Call Read_rpar(nu,'cwt'   ,cwt   )
      Call Read_ipar(nu,'debug' ,debug )

! ... overwrite parameters from arguments if any

      Call Read_iarg('klsp'  ,klsp  )
      Call Read_iarg('klsp1' ,klsp1 )
      Call Read_iarg('klsp2' ,klsp2 )
      Call Read_iarg('itype' ,itype )
      Call Read_iarg('iexp'  ,iexp  )
      Call Read_iarg('msol'  ,msol  )
      Call Read_iarg('jmvc'  ,jmvc  )
      Call Read_iarg('it_max',it_max)
      Call Read_rarg('Emin'  ,Emin  )
      Call Read_rarg('Emax'  ,Emax  )
      Call Read_rarg('Edmin' ,Edmin )
      Call Read_rarg('Edmax' ,Edmax )
      Call Read_rarg('Egap'  ,Egap  )
      Call Read_iarg('iwt'   ,iwt   )
      Call Read_rarg('cwt'   ,cwt   )
      Call Read_iarg('debug' ,debug )

! ... set the range of partial waves under consideration:

      if(klsp.gt.0) then
       klsp1=klsp; klsp2=klsp
      else
       if(klsp1.le.0) klsp1=1
       if(klsp2.lt.klsp1) klsp2=klsp1
      end if

! ... read experimental energies:

      Allocate(E_exp(ntarg)); E_exp = Etarg
      if(iexp.gt.0) then
       Inquire(file=AF_exp,EXIST=EXP)
       if(.not.EXP) Stop 'iexp >0 but no file for exp.energies'
       Open(nue,file=AF_exp)
       Do i=1,ntarg; read(nue,*) E_exp(i); End do
       Call Read_apar(nue,'unit',unit)
       it=1; Call Read_ipar(nue,'it',it)
       if(it.lt.1.or.it.gt.ntarg) it=1
       if(unit.eq.'cm') E_exp = E_exp / au_cm + Etarg(it)
       if(unit.eq.'eV') E_exp = E_exp / au_eV + Etarg(it)
       Close(nue)
       Allocate(ip_exp(ntarg))
       Call SORTR(ntarg,E_exp,ip_exp)
       iiexp=0
       Do i=1,ntarg; if(ip_exp(i).ne.i) iiexp=1; End do
      end if
      End Subroutine R_arg

