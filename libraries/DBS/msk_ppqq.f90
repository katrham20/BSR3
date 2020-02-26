!======================================================================
      Subroutine msk_ppqq(k)
!======================================================================
!     Defines matrix of Sk integrals in the B-spline basis
!     by cell algorithm for PPQQ case.
!----------------------------------------------------------------------
      Use DBS_grid
      Use DBS_gauss
      Use DBS_moments
      Use DBS_integrals
      
      Implicit none
      Integer, intent(in) :: k
      Integer :: i,j, ii,jj, iv,jv, ih,jh, ihp,jhp, ip,jp, met
      Real(8) :: c
   

! ... check the need of calculations

      if(itype == 'ppqq' .and. krk == k) Return

      if (associated(rkb)) nullify(rkb)

      met = -1
      if(stype.gt.0 .and. k.ge.krs_min .and. k.le.krs_max) then
       rkb => rks(:,:,:,:,k,1)
       if(isk(k,1) == 1) then
        krk=k; itype = 'ppqq'; Return 
       end if
       met = 0
      end if

      if(met.eq.-1) then 
       if(jtype1.eq.0) Call alloc_Sk_integral(ns,ks)
       rkb => rks1(:,:,:,:)
       if(stype1.eq.'ppqq'.and.krs1.eq.k) then
        krk=k; itype = 'ppqq'; Return
       end if 
       met = 1
      end if

! ... compute the spline moments:
   
      Call moments_pq(  k   ,kk,nv,rkd1)
      Call moments_pq(-(k+1),kk,nv,rkd2)
      Call diag_ppqq(k)
  
! ... generate the rkb array
   
      rkb=0.d0
   
      DO jv = 1,nv;   jj = 0
      DO jh = 1,ksp;  j  = jv  + jh - 1
      DO jhp= 1,ksq;  jp = jhp - jh + ks  
                      jj = jj + 1

      DO iv = 1,jv;   ii = 0
      DO ih = 1,ksp;  i  = iv  + ih - 1
      DO ihp= 1,ksq;  ip = ihp - ih + ks  
                      ii = ii + 1

        IF( iv == jv ) THEN
          c =  rkd(ii,jj,iv)
        ELSE         
          c =  rkd1(ii,iv)*rkd2(jj,jv)
        END IF

        rkb(i,j,ip,jp) = rkb(i,j,ip,jp) + c

      END DO;  END DO;  END DO
      END DO;  END DO;  END DO
          
      if(met.eq.0) isk(k,1)=1
      if(met.eq.1) then; krs1=k; stype1 = 'ppqq'; end if
      krk=k; itype = 'ppqq' 

      End Subroutine msk_ppqq


!======================================================================
      Subroutine diag_ppqq(k)
!======================================================================
!     Controls the scaling propeties for diagonal B-spline Sk-interals
!     (not implimented)
!----------------------------------------------------------------------
      Use DBS_grid, only: nv

      Implicit none
      Integer :: k,iv

      Do iv=1,nv; Call triang_ppqq(k,iv); End do

      End Subroutine diag_ppqq


!======================================================================
    Subroutine triang_ppqq (k,iv)
!======================================================================
!   Returns the two-dimensional array of B-spline integrals 
!               <B_i B_j|r^k/r^(k+1)|B_i' B_j'>
!   over the given triangle diagonal cell 
!
!   On entry   iv  -  index of the diagonal cell
!   --------
!
!   On exit    rkd(.,.,iv) - arrays of Rk B-spline integrals for given 
!   --------                 interval iv in the reduced-dimension mode
!
!   Calls:   gauleg, zbsplvd
!----------------------------------------------------------------------
    Use DBS_grid
    Use DBS_gauss
    Use DBS_moments

    Implicit none
    Integer, intent(in) :: k,iv
    Integer :: i,j, ip,jp, ii,jj, m, left
    REAL(8) :: xbase
    REAL(8) :: x(ks),w(ks),bi(ks)
    REAL(8) :: bspTmp(ks,ksp)
    REAL(8) :: bspTmq(ks,ksq)
    REAL(8) :: INTPQ(ksp,ksq,ks)

    left=iv+ks-1;  xbase=t(left)

!.. setup the gaussian points

    Call gauleg(0.d0,1.d0,x,w,ks)

    DO m=1,ks

! .. the absolute coordinate at the new gaussian point

      gx(:) = (gr(iv,m)-xbase)*x(:) + xbase

! .. the bspline values at the new gaussian points

      DO i=1,ks
       Call zbsplvd(nsp,ksp,nv, tp,iv+ksp-1, 1,gx(i),1,dbip)
       bspTmp(i,1:ksp)= dbip(1,1:ksp,1)
      END DO

      DO i=1,ks
       Call zbsplvd(nsq,ksq,nv, tq,iv+ksq-1, 1,gx(i),1,dbiq)
       bspTmq(i,1:ksq)= dbiq(1,1:ksq,1)
      END DO

! .. and the corresponding gaussian weights

      gw(:) = (gr(iv,m)-xbase)*w(:)

      gx(:) = gw(:)*gx(:)**k

!            / r(iv,m)                               k
! .. INT =  |      pbsp(iv,:,j)(r) qbsp(iv,:,jp)(r) r  dr
!           / r_iv

      Do j=1,ksp
       gw(:) = gx(:)*bspTmp(:,j)
       Do jp=1,ksq; INTPQ(j,jp,m)= SUM(gw(:)*bspTmq(:,jp));  END DO
      End do
    
    END DO     !  over m

! .. second integration  

             gx(:) = grw(iv,:)*grm(iv,:)**(k+1)

             rkd(:,:,iv) = 0.d0

    ii = 0;  DO i=1,ksp;  DO ip=1,ksq;  ii = ii+1

             bi(:) = pbsp(iv,:,i)*qbsp(iv,:,ip)*gx(:)

    jj = 0;  DO j=1,ksp;  DO jp=1,ksq;  jj = jj+1
    
             rkd(jj,ii,iv) =  SUM(bi(:)*INTPQ(j,jp,:))

             END DO; END DO
             END DO; END DO

    End Subroutine triang_ppqq
