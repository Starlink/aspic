C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      **************
C      *            *
C      * S/R FITLOR *
C      *            *
C      **************
C
C
C   PURPOSE
C      This fits an array with a sloping background and up to 8
C      stars with the same Lorentz 2-D profile, whose shape
C      parameters can be found at the same time.
C
C      It does this by using the s/r PROFIT (qv), but does it
C      carefully, taking the approximate input parameters and
C      solving for the easy ones roughly and then with refined
C      parameters doing the real fit. This ensures that even if
C      the input parameters are not too good, the fit will still
C      converge.
C
C      Even so, the input parameters must be quite good to be safe.
C
C      For details of the fitting see a listing of PROFIT.
C
C
C  ARGUMENTS
C    IN
C      DATA(NX,NY)    Real        The input array to fit
C      NX             Integer     The X length of DATA
C      NY             Integer     The Y length of DATA
C      JFIT(32)       Integer     Flag for CCC parameter fit
C                                 -1=Fixed  0=Variable  1=Not used
C      NSTARS         Integer     No of stars to fit
C      ITSLIM         Integer     Max no of iterations allowed
C      KPRINT         Integer     Flag for typing some result (no=0)
C      PARFIX         Logical     Flag for fixed star profile (yes=TRUE)
C   INPUT/OUTPUT
C      CC(32)         Real        The input profile and output fit
C                                 1=C  2=A  3=B             for background
C                                 4=RX 5=RY 6=P 7=PRX 8=PRY for profile
C                                 9=XO 10=YO 11=Height1     for 1st star
C                                 12=X2 13=Y2 14=Height2    for 2nd star
C                                      and so on for the other stars
C   OUT
C      ITER           Integer     No of iterations done
C      FIT(NX,NY)     Real        The fit to the DATA
C      RESID(NX,NY)   Real        The residuals DATA - FIT
C      PERMS          Real        RMS percentage error over array
C
C
C   CALLS
C     Grasp
C       PROFIT
C
C
C
C
C    A J PENNY                  RGO                            82-OCT-31
C --------------------------------------------------------------
C
C
C
      SUBROUTINE FITLOR(DATA,FIT,RESID,NX,NY,CC,JFIT,NSTARS,
     +                  ITER,PERMS,ITSLIM,KPRINT,PARFIX)
C
C
C
      REAL DATA(NX,NY),FIT(NX,NY),RESID(NX,NY),CC(32)
      INTEGER JFIT(32)
      LOGICAL PARFIX
C
C  Get 1st approx to heights by keeping posns and profile fixed
C
      DO K = 4,8
         JFIT(K) = -1
      ENDDO
      DO K = 1,NSTARS
         JFIT(9+(K-1)*3) = -1
         JFIT(10+(K-1)*3) = -1
      ENDDO
      CALL PROFIT(DATA,FIT,RESID,NX,NY,
     +            CC,JFIT,NSTARS,ITER,PERMS,3,0)
      DO K = 1,NSTARS
         IF (JFIT(9+(K-1)*3).NE.0) THEN
            JFIT(9+(K-1)*3) = 1
            JFIT(10+(K-1)*3) = 1
         ENDIF
      ENDDO
C
C  If variable profile
C
      IF (.NOT.PARFIX) THEN
C
C  First centre stars
C
         CALL PROFIT(DATA,FIT,RESID,NX,NY,
     +               CC,JFIT,NSTARS,ITER,PERMS,3,0)
C
C  Then get approx radius
C
         JFIT(4) = 1
         JFIT(5) = 1
         CALL PROFIT(DATA,FIT,RESID,NX,NY,
     +               CC,JFIT,NSTARS,ITER,PERMS,5,KPRINT)
C
C  Then get profile power
C
         JFIT(6) = 1
         CALL PROFIT(DATA,FIT,RESID,NX,NY,
     +               CC,JFIT,NSTARS,ITER,PERMS,5,KPRINT)
         JFIT(7) = 1
         JFIT(8) = 1
      ENDIF
C  Now do proper fit
C
       CALL PROFIT(DATA,FIT,RESID,NX,NY,
     +             CC,JFIT,NSTARS,ITER,PERMS,ITSLIM,KPRINT)
C
C
C
      END



