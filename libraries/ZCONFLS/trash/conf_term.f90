!====================================================================
      Subroutine conf_term(ic1,ic2)
!====================================================================
!     reads the term list from c-file (unit nu)
!--------------------------------------------------------------------

      USE conf_LS; USE term_LS; Use symc_list_LS; Use symt_list_LS

      IMPLICIT NONE
      
      Integer, INTENT(in) :: ic1,ic2
      Integer, External :: Ifind_term
      Integer :: ic,it,is

      Call Alloc_term_LS(0)
      Call Alloc_term_LS(iterms)
      ncfgt = ic2-ic1+1
      Call Alloc_LSP(ncfgt)

      Do ic = ic1,ic2
       it=IC_term(ic); is=IT_conf(it)
       LSP(ic)= Ifind_term(LT_conf(is),ST_conf(is),2)
      End do

      End Subroutine conf_term

