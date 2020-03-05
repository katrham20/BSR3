!======================================================================
!> @ingroup utilities
!> @brief     Test Coulomb integrals from the direct integration
!======================================================================
      Use spline_atomic, only: Z
      Use spline_param, only: hmax,rmax,ns

      Implicit real(8) (A-H,O-Z)
      Real(8) :: k1,k2, k_max

      Call Read_rarg('rmax',rmax)
      ek_max = 1.d0
      Call Read_rarg('ek_max',ek_max)

      Z = 1.d0;  lamda = 1

      k_max = sqrt(ek_max)
      hmax = min(hmax,1.d0/k_max)

      Call Def_BS
!      Call mkgrid(Z)
!      Call define_spline
!      write(*,*) 'ns = ', ns

      Do l = 0,0
       l1=l; l2=l+1
      Do ik1 = 1,10
       ek1 = 0.1d0 * ik1
      Do ik2 = 1,10
       ek2 = 0.1d0 * ik2
  if(ik1.ne.ik2) Cycle
       k1 = sqrt(ek1); k2 = sqrt(ek2)

       S = Cint(Z,k1,l1,k2,l2,lamda)    

       C = 0.5d0 * k1 / sqrt(1.d0 + ((l1+1)*k1)**2)


       write(*,'(a,f4.1,a,i1,a,f4.1,a,i1,a,i1,a,1P3e15.4)') &
                '  ek1=',ek1,'  l1=',l1, '  ek2=',ek2,'  l2=',l2, &
                '  lamda=',lamda, '  int =',S,C,S-C

      End do; End do; End do

      End ! program



!======================================================================
      Real(8) Function Cint(Z,k1,l1,k2,l2,lamda) 
!======================================================================
      Use spline_param

      Implicit real(8) (A-H,O-Z)
      Real(8), intent(in) :: k1,k2
      Real(8) :: c1(ns), c2(ns), rm(ns,ks)

      Call bs_cwf(z,l1,k1,c1,acc1)
      Call bs_cwf(z,l2,k2,c2,acc2)

      m = -(lamda + 1)
      Call mrm(m,rm)
            
      Cint = bvmv(ns,ks,rm,'s',c1,c2)

      Cint = Cint /sqrt(k1)/sqrt(k2)

      End Function Cint





 
    
