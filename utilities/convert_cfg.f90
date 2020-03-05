!======================================================================
!> @ingroup utilities
!> @brief     Convert c-file to unformated form
!======================================================================
      Use symc_list_LS
      Use symt_list_LS
      Use conf_LS
      Use orb_LS  

      Implicit real(8) (A-H,O-Z)

      Integer :: nuc=1;  Character(80) :: AF_c = 'cfg.inp'
      Integer :: nu =2;  Character(80) :: AF_u = 'cfgu.out'

      Character(200) :: line
      
      Integer(8) :: ijc
      Integer(8), external :: Def_ij8
      Real(8), allocatable :: C_term(:)
      Real(8) :: t1,t2,t3
 
      klsp=0; Call Read_iarg('klsp',klsp)
      if(klsp.gt.0) then
       write(AF_c,'(a,i3.3)') 'cfg.',klsp
       write(AF_u,'(a,i3.3)') 'cfgu.',klsp
      end if
      Call Check_file(AF_c)
      Open(nuc,file=AF_c)
      Open(nu,file=AF_u,form='UNFORMATTED')

      Call R_closed(nuc)                                                 

! ... define configurations in c-file:

      Call CPU_time(t1)

      Call Add_conf_LS(nuc,0)

      Call CPU_time(t2)

      write(*,'(/a,a/)') 'c-file data: ',trim(AF_c) 
      write(*,'(a,2i10,a)')  'ncfg   = ',ncfg, lcfg,  &
        ' -  number of configurations'
      write(*,'(a,2i10,a)')  'nsymc  = ',nsymc,lsymc, &
        ' -  number of conf. symmetries'
      write(*,'(a,2i10,a)')  'nsymt  = ',nsymt,lsymt, &
        ' -  number of term  symmetries'
      write(*,'(a,2i10,a)')  'nwf    = ',nwf,  ncore, &
        ' -  number of orbitals: '
      m = m_conf_LS + m_symt + m_symc

! ... record orbitals:

      write(nu) nwf,nclosd
      write(nu) (NEF(i),i=1,nwf)
      write(nu) (LEF(i),i=1,nwf)
      write(nu) (KEF(i),i=1,nwf)
      write(nu) (ELF(i),i=1,nwf)

! ... record configuration symmetries:

      write(nu) nsymc,lsymc
      write(nu) (LT_conf(i),i=1,nsymc) 
      write(nu) (ST_conf(i),i=1,nsymc) 
      write(nu) (no_conf(i),i=1,nsymc) 
      write(nu) (ip_conf(i),i=1,nsymc) 
      write(nu) (iq_conf(i),i=1,lsymc) 
      write(nu) (ln_conf(i),i=1,lsymc) 

! ... record angular symmetries:

      write(nu) nsymt,lsymt
      write(nu) (IT_conf(i),i=1,nsymt)
      write(nu) (ip_term(i),i=1,nsymt)
      write(nu) (LS_term(i,:),i=1,lsymt)

! ... record atomic states: 

      write(nu) ncfg,lcfg,ne
      write(nu) (ip_state(i),i=1,ncfg)
      write(nu) (ic_term(i),i=1,ncfg)
      write(nu) (ip_orb(i),i=1,lcfg)
      write(nu) (WC(i),i=1,ncfg)
       
      write(*,'(a,T20,F10.2,a)')  'conf. memory = ', 4.0*m/(1020*1024), '  Mb'
      write(*,'(a,T20,F10.2,a)')  'conf. time = ', (t2-t1)/60, ' min'

!----------------------------------------------------------------------
! ... sorting the terms according to configuration symmetries,
! ... (define IC_term1 and IC_term2):

      Call Sort_terms

      Call CPU_time(t3)

      write(nu) (IT_sort(i),i=1,nsymt)
      write(nu) (IC_term1(i),i=1,nsymc)
      write(nu) (IC_term2(i),i=1,nsymc)

      m = nsymt + 2*nsymc
      write(*,'(a,T20,F10.2,a)')  'nsymt memory = ', 4.0*m/(1020*1024), '  Mb'
      write(*,'(a,T20,F10.2,a)')  'Sort. symt = ', (t3-t2)/60, '  min'

!----------------------------------------------------------------------
! ... sorting states according terms (define IT_state1 and IT_state2):

      Call Sort_states

      t2 = t3
      Call CPU_time(t3)

      write(nu) (IP_stat(i),i=1,ncfg)
      write(nu) (IT_state1(i),i=1,nsymt)
      write(nu) (IT_state2(i),i=1,nsymt)

      m = 2*nsymt + ncfg
      write(*,'(a,T20,F10.2,a)')  'nsymc memory = ', 4.0*m/(1020*1024), '  Mb'
      write(*,'(a,T20,F10.2,a)')  'Sort. symc = ', (t3-t2)/60, '  min'

!----------------------------------------------------------------------
! ... orthogonal conditions:
! ... they stay in cfg-file: so we need use cfgu + cfg together

      End ! program Convert_cfg



