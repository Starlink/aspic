C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      **************
C      *            *
C      * S/R FITFST *
C      *            *
C      **************
C
C
C   PURPOSE
C     This s/r controls the use of the Lorentzian fixed profile, multiple
C     star fitting s/r FSTPRF. It makes one fit with fixed position and
C     then uses the heights obtained to start the real fit.
C     For a full description of the parameters see the start of FSTPRF.
C
C
C   ARGUMENTS
C    IN
C      DATA(NX,NY)      Real        The input array
C      KDO(NX,NY)       Integer*2   Work space for use in FSTPRF
C      NX               Integer     X length of DATA
C      NY               Integer     Y length of DATA
C      JFIT(32)         Integer     Flags for use of CC
C      NSTARS           Integer     No of stars to fit
C      GAURX            Real        'X Gaussian radius' of profile
C      GAURY            Real        'Y Gaussian radius' of profile
C      ALOW             Real        Limit below which pixels not to be done
C      ITLIM            Int         Max no of iterations allowed
C   IN/OUT
C      CC(32)           Real        Profile and fit parameters
C   OUT
C      ITER             Integer     No of iterations done
C
C
C   CALLS
C      Grasp
C        FSTPRF
C
C   USES
C     Integer*2 array
C
C
C
C
C   A.J.Penny                 RGO                     1983-AUG-5
C ----------------------------------------------------------------
C
C
C
      SUBROUTINE FITFST(DATA,KDO,NX,NY,CC,JFIT,NSTARS,ITER,
     +                  GAURX,GAURY,ALOW,ITLIM)
C
C
C
      REAL DATA(NX,NY),CC(32)
      INTEGER JFIT(32)
      INTEGER*2 KDO(NX,NY)
C
C  Ensure profile is fixed
C
      DO K = 4,8
         JFIT(K) = -1
      ENDDO
C
C  Get 1st approx to heights by keeping posns fixed, and doing one
C  iteration
C
      DO K = 1,NSTARS
         JFIT(9+(K-1)*3) = -1
         JFIT(10+(K-1)*3) = -1
      ENDDO
      CALL FSTPRF(DATA,KDO,NX,NY,CC,JFIT,NSTARS,ITER,1,
     +            GAURX,GAURY,ALOW)
C
C  Do the real fit, resetting the real stars to variable posns
C  Do up to ITLIM iterations
C
      DO K = 1,NSTARS
         IF (JFIT(9+(K-1)*3).NE.0) THEN
            JFIT(9+(K-1)*3) = 1
            JFIT(10+(K-1)*3) = 1
         ENDIF
      ENDDO
      CALL FSTPRF(DATA,KDO,NX,NY,CC,JFIT,NSTARS,ITER,ITLIM,
     +            GAURX,GAURY,ALOW)
C
C
C
      END


