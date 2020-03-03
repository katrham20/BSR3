!=====================================================================
      Subroutine ZCFG(E,L,Z,R,F,G,FP,GP)
!=====================================================================
!   Gives the magnitude and derivative for continuuum Coulomb Function
!   with k^2 = E, orbital momentum L, and charge Z in point R
!
!   Call: COULFG 
!---------------------------------------------------------------------
      Implicit none
      Integer, intent(in) :: L
      Real(8), intent(in) :: E,Z,R
      Real(8), intent(out) :: F,G,FP,GP
      Integer, parameter :: max_lc = 99
      Integer :: lmin, lmax, ifail
      Real(8) :: RHO,ETA,K,AN,FC,FCP,GC,GCP
 
      COMMON /COUL/ FC(100),GC(100),FCP(100),GCP(100)
 
      if(l.gt.max_lc) Stop ' ZCFG: l > max_lc=99'
 
      K = SQRT(E)
      ETA = -Z/K
      RHO = K*R
      LMIN = 0
      LMAX = L
 
      CALL ZCOULFG(RHO,ETA,LMIN,LMAX,1,0,IFAIL)
 
      AN=DSQRT(1.0/K)
      F = FC(L+1)!*AN
      G = GC(L+1)!*AN
      FP = FCP(L+1)*K!*AN
      GP = GCP(L+1)*K!*AN

      End Subroutine ZCFG


!====================================================================== 
      Subroutine ZCOULFG (XX,ETA1,LMIN,LMAX,MODE1,KFN,IFAIL) 
!====================================================================== 
 
!     REVISED COULOMB WAVEFUNCTION PROGRAM USING STEEDS METHOD 
 
!        A. R. BARNETT  MANCHESTER MARCH 1981 
 
!     ORIGINAL PROGRAM 'RCWFN' IN CPC 8(1974) 377-395 
!                     +'RCWFF' IN CPC 11(1976) 141-142 
 
!     FOLL DESCRITION OF ALGORITHM IN CPC 21(1981) 297-314 
 
!     THIS VERSION WRITTEN UP  IN  CPC  XX (1982) YYY-ZZZ 
!     COULFG RETURN F,G,F',G',FOR REAL XX.GT.0,REAL ETA1(INCLUDING O), 
!     AND INTEGER LAMBDA.GT.0 
!     THUS GIVING POSITIVE-ENERGY SOLUTIONS TO THE COULOMB SCHRODINGER 
!     EQUATION,TO THE KLEIN-GORDON EQUATION AND TO SUITABLE FORMS OF 
!     EQUATIONS 
!     THE DIRAC EQUATION,ALSO SPHERICAL & CYLINDRICAL BESSEL 
!     FOR A RANGE OF LAMBDA VALVES (XLMAX-XLMIN) MUST BE AN INTEGER, 
!     STARTING ARRAY ELEMENT  IS M1=MAX0(IDINT(XLMIN+ACCUR),0)+1 
!     SEE TENT FOR MODIFICATIONS FOR INTEGER L-VALUES 
 
!     IF'MODE'=1 GET F,G,F',G' FOR INTEGER-SPESID LAMBDA VALUES 
!             =2 F,G  UNUSED ARRAYS MUST BE DIMENSIONED IN 
!             =3 F  CALL TO AT LEAST LENGTH(1) 
!     IF 'KEN'=0 REAL  COULOMB FUNCTIONS ARE RETURNED 
!             =1 SPHERICAL  BESSEL  "   "   " 
!             =2 CYLINDRICAL BESSEL  "   "   " 
!     THE USE OF 'MODE' AND 'KFN' IS INDEPENDENT 
 
!     PRECISION: RESULTS TO WITHIN 2-3 DESIMALS OF 'MACHINE ACCHRACY' 
!     IN OSCILLATING REGION X.GE.ETA1+SQRT(ETA1**2+XLM(XLM+1)) 
!     COULFG IS CODED FOR REAL*8 ON IBM OR EQUIVALENT ACCUR=10**-16 
!     USE AUTODBL+EXTENDED RRECISION ON HX COMPILER ACCUR=10**-33 
!     FOR MANTISSAS OF 56 & 112 BITS. FOR SINGLE PRECISION CDC (48 BITS) 
!     REASSIGN DSQRT=SQRT ETC. SEE TEXT FOR COMPLEX ARITHMETIC VERSION 
 
      IMPLICIT REAL*8(A-H,O-Z) 
 
      COMMON /COUL/ FC(100),GC(100),FCP(100),GCP(100) 
 
      LOGICAL   ETANEO,XLTURN 
 
!***  COMMON BLOCK IS FOR INFORMATION ONLY. NOT REQUIREV IN CODE 
!***  COULFG HAS CALLS TO: DSQRT,DABS,DMOD,IDINT,DMIGN,DFLOAT,DMIN1 
 
      DATA ZERO,ONE,TWO,TEN2,ABORT/0.0D0,1.0D0,2.0D0,1.0D2,2.0D4/ 
      DATA HALF,TM30 /0.5D0,1.0D-30/ 
      DATA RT2DPI/0.79788456080286535587989211986876373D0/ 
 
!***  THIS CONSTANT IS DSQRT(TWO/PI): USE QO FOR IBM REAL*16:DO FOR 
!***  REAL*8 & CDC DOUBLE P: E0 FOR CDC SINGLE P; AND TRUNCATE VALUE. 
 
      ACCUR = 1.d-16

!***  CHANGE ACCUR TO SUIT MACHINE AND PRECISION REQURID 
 
      MODE=1 
      IF(MODE1.EQ. 2 .OR. MODE1.EQ. 3) MODE=MODE1 
      IFAIL=0 
      IEXP=1 
      NPQ=0 
      ETA=ETA1 
      GJWKB=ZERO 
      PACCQ=ONE 
      IF(KFN .NE. 0) ETA=ZERO 
      ETANEO=ETA .NE. ZERO 
      ACC=ACCUR 
      ACC4=ACC*TEN2*TEN2 
      ACCH=DSQRT(ACC) 
 
!***  TEST RANGE OF XX,EXIT IF.LE.DSQRT(ACCUR) OR) OR IF NEGATIVE 
 
      IF(XX .LE. ACCH)   GO TO 100 
      X=XX 
      XLM= LMIN 
      IF(KFN .EQ. 2) XLM=XLM-HALF 
      IF(XLM .LE.-ONE .OR.  LMAX .LT.  LMIN) GO TO 105 
      E2MM1=ETA*ETA+XLM*XLM+XLM 
      XLTURN=X*(X -TWO*ETA) .LT. XLM*XLM+XLM 
 
!ZZZ DELL=XLMAX-XLMIN+ACC 
!ZZZ IF(DABS(DMOD(DELL,ONE)) .GT.ACC) WRITE(6,2040)XLMAX,XLMIN,DELL 
 
      LXTRA=LMAX-LMIN 
      XLL=XLM+DFLOAT(LXTRA) 
 
!***  LXTRA IS NUMBER OF ADDITIONAL LAMBDA VALUES TO BE COMPUTED 
!***  XLL IS MAX LAMBDA VALUE,OR 0.5 SMALLER FOR J,Y BESSELS 
!***  DETERMINE STARTING ARRAY ELEMENT (M1) FROM LMIN 
 
      M1=LMIN+1 
      L1=M1+LXTRA 
! 
!***  EVALUATE CF1=F=FRIME(XL,ETA,X)/F(XL,ETA,X) 
! 
      XI=ONE/X 
      FCL=ONE 
      PK=XLL+ONE 
      PX=PK+ABORT 
    2 EK=ETA/PK 
      F=(EK+PK*XI)*FCL+(FCL-ONE)*XI 
      PK1=PK+ONE 
 
!***  TEST ENSURES B1 .NE. ZERO FOR NEGATIVE ETA,FIXUR IS EXACT. 
 
      IF(DABS(ETA*X+PK*PK1) .GT.ACC) GO TO 3 
      FCL=(ONE+EK*EK)/(ONE+(ETA/PK1)**2) 
      PK=TWO+PK 
      GO TO 2 

    3 D=ONE/((PK+PK1)*(XI+EK/PK1)) 
      DF=-FCL*(ONE+EK*EK)*D 
      IF(FCL .NE. ONE) FCL=-ONE 
      IF(D .LT. ZERO) FCL=-FCL 
      F=F+DF 
 
!***  BEGIN CF1 LOOP ON PK=K=LAMBDA+1 
 
      P=ONE 
    4 PK=PK1 
      PK1=PK1+ONE 
      EK=ETA/PK 
      TK=(PK+PK1)*(XI+EK/PK1) 
      D=TK-D*(ONE+EK*EK) 
      IF(DABS(D) .GT. ACCH)   GO TO 5 
      WRITE(6,1000) D,DF,ACCH,PK,EK,ETA,X 
      P=P+ONE 
      IF( P .GT. TWO)   GO TO 110 

    5 D=ONE/D 
      IF(D .LT.ZERO) FCL=-FCL 
      DF=DF*(D*TK-ONE) 
      F=F+DF 
      IF(PK.GT.PX) GO TO 110 
      IF(DABS(DF).GE. DABS(F)*ACC) GO TO 4 
      NFP=PK-XLL-1 
      IF(LXTRA.EQ.0) GOTO 7 
 
!***DONWA WARD RECURRENCE TO LAMBDA=XLM.ARRAY GC,IF PRESENT,STORES R 
 
      FCL=FCL*TM30 
      FPL=FCL*F 
      IF(MODE.EQ.1) FCP(L1)=FPL 
      FC(L1)=FCL 
      XL=XLL 
      RL=ONE 
      EL=ZERO 
      DO 6 LP=1,LXTRA 
      IF(ETANEO) EL=ETA/XL 
      IF(ETANEO) RL=DSQRT(ONE+EL*EL) 
      SL=EL+XL*XI 
      L=L1-LP 
      FCL1=(FCL*SL+FPL)/RL 
      FPL=FCL1*SL-FCL*RL 
      FCL=FCL1 
      FC(L)=FCL 
      IF(MODE.EQ.1) FCP(L)=FPL 
      IF(MODE.NE.3.AND.ETANEO) GC(L+1)=RL 

    6 XL=XL-ONE 
      IF(FCL.EQ. ZERO) FCL=ACC 
      F=FPL/FCL 

!***NOW WE HAVE REACHED LAMBDA=XLMIN=XLM 
!***EVALUATE CF2=P+I.Q AGAIN USING STEED'S ALGORITHM 
!***SEE TE FOR COMAACT COMPLEX CODE FOR SP CDC OR NON-ANSI IBM 

    7 IF(XLTURN) CALL JWKB(X,ETA,DMAX1(XLM,ZERO),FJWKB,GJWKB,IEXP) 
      IF(IEXP.GT.1.OR.GJWKB.GT.ONE/(ACCH*TEN2)) GO TO 9 
      XLTURN=.FALSE. 
      TA=TWO*ABORT 
      PK=ZERO 
      WI=ETA+ETA 
      P=ZERO 
      Q=ONE-ETA*XI 
      AR=-E2MM1 
      AI=ETA 
      BR=TWO*(X-ETA) 
      BI=TWO 
      DR=BR/(BR*BR+BI*BI) 
      DI=-BI/(BR*BR+BI*BI) 
      DP=-XI*(AR*DI+AI*DR) 
      DQ=XI*(AR*DR-AI*DI) 

    8 P=P+DP 
      Q=Q+DQ 
      PK=PK+TWO 
      AR=AR+PK 
      AI=AI+WI 
      BI=BI+TWO 
      D=AR*DR-AI*DI+BR 
      DI=AI*DR+AR*DI+BI 
      C=ONE/(D*D+DI*DI) 
      DR=C*D 
      DI=-C*DI 
      A=BR*DR-BI*DI-ONE 
      B=BI*DR+BR*DI 
      C=DP*A-DQ*B 
      DQ=DP*B+DQ*A 
      DP=C 
      IF(PK.GT.TA)   GO TO 120 
      IF(DABS(DP)+DABS(DQ).GE.(DABS(P)+DABS(Q))*ACC) GO TO 8 
      NPQ=PK/TWO 
      PACCQ=HALF*ACC/DMIN1(DABS(Q),ONE) 
      IF(DABS(P).GT.DABS(Q)) PACCQ=PACCQ*DABS(P) 

!***SOLVEFOR FCM=F AT LAMBDA=XLM,THEM FIND NORM FACTOR W=W/FCM 

      GAM=(F-P)/Q 
      IF(Q.LE. ACC4*DABS(P)) GO TO 130 
      W=ONE/DSQRT((F-P)*GAM+Q) 
      GO TO 10 

!***ARRIVEHERE IF G(XLM).GT.10**6 OR IEXP .GT.70 & XLTURN=.TRUE. 

    9 W=FJWKB 
      GAM=GJWKB*W 
      P=F 
      Q=ONE 
 
!***NORMA LISE FOR SPHERICAL3 OR CYLINDRICAL BESSEL FUNCTIONS 

  10 ALPHA=ZERO 
      IF(KFN.EQ.1) ALPHA=XI 
      IF(KFN.EQ.2) ALPHA=XI*HALF 
      BETA=ONE 
      IF(KFN.EQ.1) BETA=XI 
      IF(KFN.EQ.2) BETA=DSQRT(XI)*RT2DPI 
      FCM=DSIGN(W,FCL)*BETA 
      FC(M1)=FCM 
      IF(MODE.EQ.3) GO TO 11 
      IF(.NOT.XLTURN) GCL=FCM*GAM 
      IF(XLTURN) GCL=GJWKB*BETA 
      IF(KFN.NE.0) GCL=-GCL 
      GC(M1)=GCL 
      GPL=GCL*(P-Q/GAM)-ALPHA*GCL 
      IF(MODE.EQ.2) GO TO 11 
      GCP(M1)=GPL 
      FCP(M1)=FCM*(F-ALPHA) 
   11 IF(LXTRA.EQ.0) RETURN 

!***URWARD RECURRENCE FROM GC(M1),GCP(M1) STORED VALUE IS RL 
!***RENORMA LISE FC,FCP AT EACH LAMBDA AND CORRECT REGULAR DERIVATIVE 
!***XL=XLM HERE AND RL=ONE, EL=ZERO FOR BESSELS 

      W=BETA*W/DABS(FCL) 
      MAXL=L1-1 
      DO 12 L=M1,MAXL 
      IF(MODE.EQ.3) GOTO 12 
      XL=XL+ONE 
      IF(ETANEO) EL=ETA/XL 
      IF(ETANEO) RL=GC(L+1) 
      SL=EL+XL*XI 
      GCL1=((SL-ALPHA)*GCL-GPL)/RL 
      GPL=RL*GCL-(SL+ALPHA)*GCL1 
      GCL=GCL1 
      GC(L+1)=GCL1 
      IF(MODE.EQ.2) GO TO 12 
      GCP(L+1)=GPL 
      FCP(L+1)=W*(FCP(L+1)-ALPHA*FC(L+1)) 
   12 FC(L+1)=W*FC(L+1) 
      RETURN 

 1000 FORMAT(/'CF1 ACCURACY LOSS : D,DF,ACCH,K,ETA/K,ETA,X=',1P7D9.2/) 

!***  ERROR MESSAGES 

  100 IFAIL=-1 
      WRITE(6,2000) XX,ACCH 
 2000 FORMAT('FOR XX=',1PD12.3,'TRY SMALL-X SOLUTIONS', &
      'OR X NEGATIVE'/,'SQUARE ROOT ACCURACY PARAMETER = ',D12.3/) 
      RETURN 

  105 IFAIL=-2 
      WRITE(6,2005)  LMAX, LMIN,XLM 
 2005 FORMAT(/'PROBLEM WITH INPUT ORDER VALUES: LMAX',  &
      '  LMIN,XLM = ',2I10,1PD15.6/) 
      RETURN 

  110 IFAIL=1 
      WRITE(6,2010) ABORT,F,DF,PK,PX,ACC 
 2010 FORMAT('CF1 HAS FAILED TO CONVERGE AFTER ',  &
      F10.0,'ITERATIONS',/'F,DF,PK,PX,ACCUR=',1P5D12.3//) 
      RETURN 

  120 IFAIL=2 
      WRITE(6,2020) ABORT,P,Q,DP,DQ,ACC,XX 
 2020 FORMAT('CF2 HAS FAILED TO CONVERGE AFTER ',  &
      F7.0,'ITEPATIONS',/'P,Q,DP,DQ,ACCUR = ',1P4D17.7,2D12.3//) 
      RETURN 

  130 IFAIL=3 
      WRITE(6,2030) P,Q,ACC,LXTRA,M1 
 2030 FORMAT('FINAL Q.LE.DABS(P)*ACC*10**4,P,Q,ACC = ',1P3D12.3, &
      4X,'LXTRA,M1=',2I5/) 
      RETURN 

!2040 FORMAT('XLMAX-XLMIN=DELL NOT AN INTEGER',1P3D20.10/) 
      END 
 
!====================================================================== 
      Subroutine JWKB(XX,ETA1,XL,FJWKB,GJWKB,IEXP) 
!====================================================================== 
      Real(8) ::  XX,ETA1,XL,FJWKB,GJWKB,DZERO 
 
!***COMPUTES JWKB APPROXIMATIONS TO COULOMB FUNCTIONS FOR XL.GE.0 
!***AS MODI FIED BY BIEDENHARN ET AL.PHYS REV 97(1955)542-554 
!***CALLS D MAX1,SQRT,ALOG,EXP,ATAN2,FLOAT,INT BARNETT FEB 1981 
 
      DATA ZERO,HALF,ONE,SIX,TEN/0.0E0, 0.5E0,1.0E0,6.0E0,10.0E0/ 
      DATA DZERO,RL35,ALOGE/0.0D0,35.0E0,0.4342945E0/ 
 
      X=XX 
      ETA=ETA1 
      GH2=X*(ETA+ETA-X) 
      XLL1=DMAX1(XL*XL+XL,DZERO) 
      IF(GH2+XLL1.LE.ZERO) RETURN 
      HLL=XLL1+SIX/RL35 
      HL=SQRT(HLL) 
      SL=ETA/HL+HL/X 
      RL2=ONE+ETA*ETA/HLL 
      GH=SQRT(GH2+HLL)/X 
      PHI=X*GH-HALF*(HL*ALOG((GH+SL)**2/RL2)-ALOG(GH)) 
      IF(ETA.NE.ZERO) PHI=PHI-ETA*ATAN2(X*GH,X-ETA) 
      PHI10=-PHI*ALOGE 
      IEXP=INT(PHI10) 
      IF(IEXP.GT.70) GJWKB=TEN**(PHI10-FLOAT(IEXP)) 
      IF(IEXP.LE.70) GJWKB=EXP(-PHI) 
      IF(IEXP.LE.70) IEXP=0 
 
      FJWKB=HALF/(GH*GJWKB) 
 
      End Subroutine JWKB

