!======================================================================
      Subroutine R_orth(nuc)
!======================================================================
!     read from unit 'nuc' and store in array IORT (module orb_LS)
!     the imposed orthogonal conditions 
!----------------------------------------------------------------------
      Use conf_LS;   Use orb_LS

      Character(13) :: Aort

      rewind(nuc)
    1 read(nuc,'(a)',end=2) Aort

      if(Aort(1:1).ne.'<') go to 1 
      Call EL4_nlk(Aort(2: 5),n1,l1,k1)
      i1 = Ifind_nlk(n1,l1,k1,0)
      if(i1.eq.0.and.k1.ne.0) go to 1
      Call EL4_nlk(Aort(7:10),n2,l2,k2)
      i2 = Ifind_nlk(n2,l2,k2,0)
      if(i2.eq.0.and.k2.ne.0) go to 1
      if(l1.ne.l2) go to 1 
      read(Aort(13:13),'(i1)') ii

      if(k1.gt.0.and.k2.gt.0) then
       i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
      elseif(k1.gt.0.and.k2.eq.0) then
       Do i2=1,nwf
        if(l2.ne.LEF(i2).or.n2.ne.NEF(i2)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do       
      elseif(k1.eq.0.and.k2.gt.0) then
       Do i1=1,nwf
        if(l1.ne.LEF(i1).or.n1.ne.NEF(i1)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do       
      elseif(k1.eq.0.and.k2.eq.0) then
       Do i1=1,nwf
        if(l1.ne.LEF(i1).or.n1.ne.NEF(i1)) Cycle
       Do i2=1,nwf
        if(l2.ne.LEF(i2).or.n2.ne.NEF(i2)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do; End do       
      end if

      go to 1
    2 Backspace(nuc)

      End Subroutine R_orth 

!======================================================================
      Subroutine Read_orth(nuc)
!======================================================================
!     read from unit 'nuc' and store in array IORT (module orb_LS)
!     the imposed orthogonal conditions 
!----------------------------------------------------------------------
      Use conf_LS;   Use orb_LS;  Use phys_orb_LS

      Character(13) :: Aort

      rewind(nuc)
    1 read(nuc,'(a)',end=2) Aort
      if(Aort(1:1).ne.'<') go to 1 

      Call EL4_nlk(Aort(2: 5),n1,l1,k1)
      i1 = Ifind_nlk(n1,l1,k1,0)
      if(i1.eq.0.and.k1.ne.0) go to 1
      j1 = Ipointer(nphys_orb,ip_phy,i1)    
      if(j1.gt.0) i1 = ip_sub(j1)

      Call EL4_nlk(Aort(7:10),n2,l2,k2)
      i2 = Ifind_nlk(n2,l2,k2,0)
      if(i2.eq.0.and.k2.ne.0) go to 1
      j2 = Ipointer(nphys_orb,ip_phy,i2)    
      if(j2.gt.0) i2 = ip_sub(j2)

      if(l1.ne.l2) go to 1 
      if(j1.eq.0.and.j2.eq.0) go to 1

      read(Aort(13:13),'(i1)') ii

      if(k1.gt.0.and.k2.gt.0) then
       i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
      elseif(k1.gt.0.and.k2.eq.0) then
       Do i2=1,nwf
        if(l2.ne.LEF(i2).or.n2.ne.NEF(i2)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do       
      elseif(k1.eq.0.and.k2.gt.0) then
       Do i1=1,nwf
        if(l1.ne.LEF(i1).or.n1.ne.NEF(i1)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do       
      elseif(k1.eq.0.and.k2.eq.0) then
       Do i1=1,nwf
        if(l1.ne.LEF(i1).or.n1.ne.NEF(i1)) Cycle
       Do i2=1,nwf
        if(l2.ne.LEF(i2).or.n2.ne.NEF(i2)) Cycle
        i=max(i1,i2); j=min(i1,i2); IORT(i,j)=ii
       End do; End do       
      end if

      go to 1
    2 Backspace(nuc)

      End Subroutine Read_orth 

