      SUBROUTINE ELLFIT (MAXPAR,MAXPOS,MAXCNT,ELLPOS,LOGI,PARMS,
     :                   ELLPAR)
C+
C     ELLFIT.
C
C     Subroutine to fit an ellipse to each of a set of contours
C     and return the parameters of the various fitted ellipses.
C
C  Given;
C   MAXPAR   (I)  Max. no. of parameters for each contoured point;
C                 must be .ge. 2.
C   MAXPOS   (I)  Max. permitted no. of points in each contour
C                 = array size.
C   MAXCNT   (I)  No. of contours, = array size.
C   ELLPOS   (RA) Array of positions of points along contours.
C   LOGI     (RA) Array holding the Log Intensity value for each
C                 contour (this will be added to the file of 
C                 parameters for each fitted ellipse.
C   PARMS    (I)  No. of parameters retained for each fitted ellipse
C                 Must be .ge. 6 (5 to define the ellipse + log I).
C
C  Returned;
C   ELLPAR (RA) Array holding parameters for the fitted ellipses.
C
C  Subroutines called;
C   Interfaces:-  OUTPUT.
C   NAG:-         E04CGF.
C   E2D:-         INTELL.
C
C  Structure:-
C    For each contour
C      extract contour into work array
C      find initial approximation to the ellipse.
C      attempt lst. sqrs. fit.
C      If fit ok
C        copy parameters into output array.
C        print message.
C      else
C        copy initial parameters into output array.
C        print error message.
C      end if
C    end do
C   
C  A C Davenhall./ROE/                                     23/9/82.
C-

C  FUNCT1 is declared external here so that the symbol is resolved
C  from the higher level library though it will be called from a lower
C  level routine in NAG

      EXTERNAL FUNCT1

      INTEGER MAXPAR,MAXPOS,MAXCNT,PARMS
      REAL ELLPOS(MAXPAR,MAXPOS,MAXCNT),
     :     LOGI(MAXCNT),ELLPAR(PARMS,MAXCNT)
C
C    Common block to communicate the currently extracted contour
C    level to the routine for evaluating the fitted ellipse,
C    FUNCT1. This is necessitated by the use of the NAG routine
C    E04CGF. 
C
      COMMON /CNTR/MAXEXT,NPTS,CNTRX,CNTRY
      INTEGER MAXEXT,NPTS
      REAL CNTRX(5000),CNTRY(5000)
C
      INTEGER MXPARM
      PARAMETER (MXPARM=5)
      DOUBLE PRECISION PARAM(MXPARM)
C
      INTEGER STATUS,IOSTAT
      REAL XCEN,YCEN,A,B,THETA,ELLIP
C
      INTEGER LIW,LW
      PARAMETER (LIW=7)
      PARAMETER (LW=90)
      INTEGER IWORK(LIW)
      DOUBLE PRECISION WORK(LW)
      DOUBLE PRECISION SUMSQ
C
      CHARACTER BUFFER*70
C
C    Set MAXEXT to = size of arrays CNTRX and CNTRY
C    (This has to be done in this rather inelegant way because
C    the variable appears in a common block).
C
      MAXEXT=5000
C
C    Fit the contours.
C
      DO K=1,MAXCNT
C
C    Copy the Kth contour into the work arrays.
C
        NPTS=1
        DO WHILE (ELLPOS(1,NPTS,K).GT.-9.0E3.AND.
     :     NPTS.LT.MAXPOS.AND.NPTS.LT.MAXEXT)
          CNTRX(NPTS)=ELLPOS(1,NPTS,K)
          CNTRY(NPTS)=ELLPOS(2,NPTS,K)
          NPTS=NPTS+1
        END DO
        NPTS=NPTS-1
C
C    Find the initial approximation to the ellipse.
C
        CALL INTELL (MAXEXT,NPTS,CNTRX,CNTRY,XCEN,YCEN,A,B,
     :             THETA)
        PARAM(1)=A
        PARAM(2)=B
        PARAM(3)=-XCEN
        PARAM(4)=-YCEN
        PARAM(5)=THETA
C
C    Attempt to make the fit.
C
        STATUS=1
        CALL E04CGF (MXPARM,PARAM,SUMSQ,IWORK,LIW,WORK,LW,STATUS)
C
        IF (STATUS.EQ.0) THEN
C
C    Fit obtained Ok: Copy parameters to the return array.
C
C    Check that the major and minor axes have not got transposed.
C
          IF (PARAM(1).GT.PARAM(2)) THEN
            A=PARAM(1)
            B=PARAM(2)
            THETA=PARAM(5)
          ELSE
            B=PARAM(1)
            A=PARAM(2)
            THETA=PARAM(5)+1.570796E0
            IF (THETA.GT.3.14159E0) THETA=THETA-3.14159E0
          END IF
          ELLIP=1.0E0-(B/A)
          ELLPAR(1,K)=LOGI(K)
          ELLPAR(2,K)=-PARAM(3)
          ELLPAR(3,K)=-PARAM(4)
          ELLPAR(4,K)=A
          ELLPAR(5,K)=ELLIP
          ELLPAR(6,K)=THETA
C
C    Print a message.
C
          WRITE(BUFFER,2000) K
 2000     FORMAT(1X,'Contour ',I3,2X,'fitted successfully.')
          CALL OUTPUT (BUFFER,IOSTAT)
        ELSE
C
C    Fit failed to converge. Save the parameters of the initial
C    attempt.
C
          ELLIP=1.0E0-(PARAM(2)/PARAM(1))
          ELLPAR(1,K)=LOGI(K)
          ELLPAR(2,K)=-PARAM(3)
          ELLPAR(3,K)=-PARAM(4)
          ELLPAR(4,K)=PARAM(1)
          ELLPAR(5,K)=ELLIP
          ELLPAR(6,K)=PARAM(5)
C
C    Print message.
C
          WRITE(BUFFER,2001) K
 2001     FORMAT(1X,'***ERROR  Contour ',I3,2X,'failed to converge.')
          CALL OUTPUT (BUFFER,IOSTAT)
        END IF
      END DO
      END
