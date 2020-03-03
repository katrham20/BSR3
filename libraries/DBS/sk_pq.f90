!======================================================================
      Real(8) Function sk_ppqq_pq (i1,j1,i2,j2,k)
!======================================================================
!     Returns  S^k (i1, j1; i2, j2), base on the assembling two-electron
!     B-spline integrals (see module DBS_integrals).
!----------------------------------------------------------------------
      Use DBS_grid,          only: ns,ks
      Use DBS_orbitals_pq,   only: p => pq
  
      Implicit none
      Integer, intent(in) :: i1,j1,i2,j2,k
      Real(8) :: di(ns,ns),dj(ns,ns),dc(ns,ns)
      Real(8), external :: SUM_AmB
      Save

      if(k.lt.0) Stop 'Stop in Sk_ppqq: k < 0'                 ! ???

      Call msk_ppqq(k)
      Call density (ns,ks,di,p(1,1,i1),p(1,2,i2),'n')
      Call convol  (ns,ks,dc,di,2,'n','n')
      Call density (ns,ks,dj,p(1,1,j1),p(1,2,j2),'n')
      sk_ppqq_pq = SUM_AmB(ns,ks,dc,dj,'n')

      End Function sk_ppqq_pq

!======================================================================
      Real(8) Function sk_pqqp_pq (i1,j1,i2,j2,k)
!======================================================================
!     Returns  S^k (i1, j1; i2, j2), base on the assembling two-electron
!     B-spline integrals (see module DBS_integrals).
!----------------------------------------------------------------------
      Use DBS_grid,          only: ns,ks
      Use DBS_orbitals_pq ,  only: p => pq
  
      Implicit none
      Integer, intent(in) :: i1,j1,i2,j2,k
      Real(8) :: di(ns,ns),dj(ns,ns),dc(ns,ns)
      Real(8), external :: SUM_AmB

      if(k.lt.0) Stop 'Stop in sk_pqqp: k < 0'                   ! ???

      Call msk_pqqp(k)
      Call density (ns,ks,di,p(1,1,i1),p(1,2,i2),'n')
      Call convol  (ns,ks,dc,di,2,'n','n')
      Call density (ns,ks,dj,p(1,2,j1),p(1,1,j2),'n')
      sk_pqqp_pq = SUM_AmB(ns,ks,dc,dj,'n')

      End Function sk_pqqp_pq

