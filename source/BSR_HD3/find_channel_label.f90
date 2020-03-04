!======================================================================
      Subroutine Find_channel_label(ich,jch,is,E,Lab)
!======================================================================
!> @ingroup bsr_hd
!> @brief     define channel label
!----------------------------------------------------------------------
      Use bsr_hd; Use target; Use channel;  Use conf_LS

      Implicit none
      Integer :: is,ich,jch,i,ic,ic1,ic2,nodes
      Real(8) :: S,E
      Character(*) ::  Lab

      if(ich.le.nch) then
       i = ich
       ic1=1; if(i.gt.1) ic1=ipconf(i-1)+1
       ic2=ipconf(i)
      else
       i = ich-nch
       ic1=ippert(i-1)+1
       ic2=ippert(i)
      end if

      i=0; S=0.d0
      Do ic=ic1,ic2
       if(abs(WC(ic)).lt.S) Cycle
       S=abs(WC(ic)); i=ic
      End do
      Call Get_cfg_LS(i)

      if(ich.le.nch) then
       nn(no) = ICHAR('k')
       if(i.gt.0.and.Etarg(iptar(ich)).gt.E) then
        if(jch.gt.1) then
         nn(no) = ICHAR('n')
        else
         Call Find_channel_nodes(ich,is,nodes)
         nn(no) = nodes + lch(ich) + 1
        end if
       end if
      end if

      Call Label_c(Lab,1,0)

      End Subroutine Find_channel_label


!======================================================================
      Subroutine Find_channel_nodes(ich,is,nodes)
!======================================================================
!> @ingroup bsr_hd
!> @brief     define number of nodes in the given solusion
!----------------------------------------------------------------------
      Use bsr_hd; Use spline_param, only: ns

      Implicit none
      Integer :: ich,is,nodes, j,j1,j2
      Real(8) :: S,vv(ns)

! ... find solution in original B-spline basis

      vv = 0.d0
      Do j=ipsol(ich-1)+1,ipsol(ich)
       vv(1:ns) = vv(1:ns) + a(j,is)*bb(1:ns,j)     
      End do

! ... find nodes:

      nodes=0
      S = maxval(abs(vv)) / 200.d0
      j1=2
      Do j=2,ns; if(abs(vv(j)).lt.S) Cycle; j1=j; Exit; End do
      j2=ns-1
      Do j=ns-1,1,-1; if(abs(vv(j)).lt.S) Cycle; j2=j; Exit; End do
      Do j=j1,j2-1
       if(vv(j)*vv(j+1).lt.0.d0) nodes=nodes+1
      End do

      End Subroutine Find_channel_nodes
