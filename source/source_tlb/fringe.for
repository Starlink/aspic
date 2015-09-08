      PROGRAM FRINGE
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ******************** 
C                     *                  * 
C                     * Program   FRINGE * 
C                     *                  * 
C                     ******************** 
C
C
C
C          CALLING SEQUENCE:- 
C               FRINGE  [FACTOR=f]
C
C
C          FUNCTION:- 
C               It  works  out  what  multiple  of  one  image  should   be 
C               subtracted  from  a  second  image  so  that the noise in a 
C               selected region is minimised. 
C               Alternatively the scaling factor may be specified
C               on the command line.
C
C
C          USE:- 
C               It can be used to remove fringes from CCD images. The IMAGE 
C               should  be  flat fielded first and the model fringe pattern 
C               (MAY OPTIONALLY BE)
C               zero-meaned. It  is  assumed  that  the  IMAGE  is  already 
C               displayed on the ARGS. 
C
C               If it appears not to be converging, or oscillating then
C               hitting Cntrl/C will interrupt the process and use the best
C               available value.
C
C
C         USER PARAMETERS:- 
C
C         IMAGE                               This   is   the   input   2-D 
C                                             Starlink image which is to be 
C                                             corrected. 
C
C         FRINGES                             This  is   a   model   fringe 
C                                             pattern relevant to the data. 
C
C         OUTPUT                              This  stores  the  correected 
C                                             image   as   a  2-D  Starlink 
C                                             frame. 
C
C         BESTFAC                             This  is  the  value  of  the 
C                                             factor   used   to  give  the 
C                                             results, which is returned to 
C                                             the environment. 
C
C            NORMALLY DEFAULTED PARAMETERS
C                  FACTOR   (0)               If a non-zero value is found
C                                             that value is used directly.
C
C         USE OF TRACKER-BALL BUTTONS:- 
C
C         RED   4     Used to select two points in the image, which  should 
C                     be free of stars and defects to give best results. 
C
C
C
C
C
C
C 
C
C
C         K F Hartley              RGO                            25-FEB-82 
C         (Amended by KFH on 9-12-83)
C
C
C-------------------------------------------------------------------------- 

C
C......N.B. This program uses the CNTRL/C routines to handle
C......     keyboard interrupts and so needs to be linked with
C.....      ,INTERRUPT or LLIBDIR:RGOLIB/LIB
C



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AXI(2),AXF(2),STATUS
C
C   First get the input image
C
      CALL RDIMAG('IMAGE',FMT_R,2,AXI,NDIM,IPI,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
C
C      If OK then read fringe pattern.
C
         CALL RDIMAG('FRINGES',FMT_R,2,AXF,NDIM,IPF,STATUS)
         IF (STATUS.EQ.ERR_NORMAL.AND.NDIM.EQ.2) THEN
C
C         Check that they are the same size.
C
            IF (AXI(1).EQ.AXF(1).AND.AXI(2).EQ.AXF(2)) THEN
C
C            Find out if a non-zero factor has been specified
C
               CALL RDKEYR('FACTOR',.FALSE.,1,FACTOR,I,STATUS)
               IF (FACTOR.NE.0.0) THEN
C
C            If a non-zero factor has been specified, then no
C            sub-region of the image will be used
C
                  I1=1
                  I2=AXI(1)
                  J1=1
                  J2=AXI(2)
               ELSE
C
C            Pick up the region for study.
C
                  CALL ARGS_CURREC(AXI(1),AXI(2),X1,Y1,X2,Y2)
                  I1=MIN(INT(X1),INT(X2))
                  I2=MAX(INT(X1),INT(X2))
                  J1=MIN(INT(Y1),INT(Y2))
                  J2=MAX(INT(Y1),INT(Y2))
               END IF
C
C            Check that the locations are valid.
C
               IF (I1.GT.0.AND.I1.LE.AXI(1).
     :             AND.I2.GT.0.AND.I2.LE.AXI(2).
     :             AND.J1.GT.0.AND.J1.LE.AXI(2).
     :             AND.J2.GT.0.AND.J2.LE.AXI(2) ) THEN
C
C               Now that it is needed, get an output image.
C
                  CALL WRIMAG('OUTPUT',FMT_R,AXI,2,IPO,STATUS)
                  IF (STATUS.EQ.ERR_NORMAL) THEN
C
C                  Now do something useful.
C
                     CALL MATCH (%VAL(IPI),%VAL(IPF),AXI(1),AXI(2),
     :                           I1,J1,I2,J2,%VAL(IPO),FACTOR)
C
C                   Now store the results.
C
                      CALL WRKEYR('BESTFAC',FACTOR,1,STATUS)
                  ELSE
                     CALL WRERR('ERR0')
                  END IF
                ELSE
                   CALL WRERR('ERR1')
                END IF
            ELSE
               CALL WRERR('ERR2')
            END IF
         ELSE
            CALL WRERR('ERR3')
         END IF
      ELSE
         CALL WRERR('ERR4')
      END IF
C
C   Tidy up and go home.
C
      CALL FRDATA (' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE MATCH(DATA,FRINGE,N,M,I1,J1,I2,J2,OUT,OLDFAC)
C
C   This subroutine computes the factor so that the expression
C
C         DATA - FACTOR*FRINGE
C
C   has the smallest possible RMS deviation from the mean  in a
C   defined sub-region.
C
C   Parameters :-
C
C      INPUT
C
C         DATA     The input 2-d array
C         FRINGE   The 2-d array to be subtracted.
C         N        The first dimension of DATA, FRINGE and OUT
C         M        The second dimension of DATA, FRINGE and OUT
C         I1,J1    The bottom left hand corner of the selected region
C         I2,J2    The top right hand corner of the selected region.
C
C      OUTPUT

C
C         OUT      The 2-d array which contains the output
C         OLDFAC   This is the factor which has been found to
C                  minimize the sigma of the result.
C
C
C      Written by K F Hartley at RGO on 25/2/82
C
      REAL DATA(N,M),FRINGE(N,M),OUT(N,M)
      REAL SIG(3),FAC(3)
      CHARACTER*72 TEXT
C
C   Enable control C interupts from the terminal
C
      LOGICAL CTRLC
      CALL SETUP('TT')
      CALL ENABLE
C
C   CALCULATE MEAN OF FRINGE PATTERN, FOR USE BELOW
C
      SUM=0.0
      DO J=1,M
         DO I=1,N
            SUM=SUM+FRINGE(I,J)
         END DO
      END DO
      AVFR=SUM/(REAL(N)*REAL(M))
C
C   If a non-zero value of OLDFAC is passed through from the
C   calling segment, use it instead of computing a new one.
C
      IF (OLDFAC.NE.0.0) THEN
         CALL WRUSER('Using input scale value',ISTAT)
         GO TO 200
      END IF
C
C   This is the area of the defined region - used to calculate means.
C
      AREA=REAL(I2+1-I1)*REAL(J2+1-J1)
C
C   Set up plausible initial values.
C
      OLDFAC=1.0
      OLDSIG=100.0
      NTRIES=1
C
C   The routine is going to loop back to here until it converges.
C
  100 CONTINUE
C
C   For the first three tries it uses default values for FACTOR.
C
      IF (NTRIES.EQ.1) FACTOR=0.0
      IF (NTRIES.EQ.2) FACTOR=0.5
      IF (NTRIES.EQ.3) FACTOR=1.0
C
C   Having computed three values of sigma, it can predict a new
C   value to try by fitting a parabola to those three pairs of
C   FACTOR and SIGMA.
C
      IF (NTRIES.GT.3) THEN
C
C   After 3 tries it is possible to interupt the program using Control C
C   If this is done the current best factor is used.
C   (ie the one giving the smallest sigma of the three stored)
C
      IF (CTRLC(0)) THEN
         NMIN=1
         IF (SIG(2).LT.SIG(1)) NMIN=2
         IF (SIG(3).LT.SIG(NMIN)) NMIN=3
         OLDFAC=FAC(NMIN)
         GO TO 200
      END IF
         CALL PARMIN(FAC,SIG,FACTOR)
      END IF
C
C   The differences are formed for this value of FACTOR and the
C   sums and sums of squares are built up to compute the sigma.
C
      SUM=0.0
      SUM2=0.0
C
C   Note that only pixels in the defined region are used.
C
      DO J=J1,J2
         DO I=I1,I2
            DIFF=DATA(I,J)-FRINGE(I,J)*FACTOR
            SUM=SUM+DIFF
            SUM2=SUM2+DIFF*DIFF
         END DO
      END DO
C
C   The average and sigma can now be calculated.
C
      AVE=SUM/AREA
      SIGMA=SQRT(SUM2/AREA - AVE*AVE)
C
C   and written to the trerminal.
C
      WRITE (TEXT,900) FACTOR,SIGMA
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER(' ',ISTAT)
C
C   The current value of FACTOR is stored for subsequent use.
C
      OLDFAC=FACTOR
      IF (NTRIES.LE.3) THEN
C
C   If this is one of the first three values, store SIGMA and FACTOR
C   in the two arrays FAC and SIG.
C
         FAC(NTRIES)=FACTOR
         SIG(NTRIES)=SIGMA
      ELSE
C
C   If not, then replace the entry with the largest sigma by the
C   new one. This should ensure the ultimate convergence of the loop.
C
         NPUT=1
         IF (SIG(2).GT.SIG(NPUT))  NPUT=2
         IF (SIG(3).GT.SIG(NPUT)) NPUT=3
         IF (SIG(NPUT).GT.SIGMA) SIG(NPUT)=SIGMA
         FAC(NPUT)=FACTOR
      END IF
C
C   Now test for convergence - a difference of less than 0.1%
C
      DIFSIG=ABS(SIGMA-OLDSIG)/SIGMA
      IF (DIFSIG.GT.0.001) THEN
         OLDSIG=SIGMA
         NTRIES=NTRIES+1
C
C      and return for another try if not converged.
C
         GO TO 100
C
C   If it has converged, use the best factor stored for
C   the subsequent processing.
C
      ELSE
         NMIN=1
         IF (SIG(2).LT.SIG(1)) NMIN=2
         IF (SIG(3).LT.SIG(NMIN)) NMIN=3
         OLDFAC=FAC(NMIN)
      END IF
C
C   Having got a decent value for the factor we can now use it on
C   the WHOLE array to create the output image.
C
  200 CONTINUE
C
C   CONST IS USED TO RE-SET THE MAEN VALUE OF THE OUTPUT ARRAY
C
C   THIS APPROACH IS USED TO TRY TO BEAT THE NOISE
C
      CONST=AVFR*OLDFAC
      DO J=1,M
         DO I=1,N
            OUT(I,J)=DATA(I,J)-FRINGE(I,J)*OLDFAC + CONST
         END DO
      END DO
  300 CONTINUE
  900 FORMAT ('FACTOR =',F10.5,'SIGMA = ',F10.5)
      END
      SUBROUTINE ARGS_CURREC(N,M,X1,Y1,X2,Y2)
C
C   This subroutine allows the user to define a rectangle within
C   an image displayed on the ARGS by using the trackerball cursor.
C
C   Parameters
C      INPUT
C         N      This is the x dimension of the displayed image.
C         M      This is the y dimension of the displayed image.
C
C      OUTPUT
C         X1     The x co-ordinate of the first point selected.
C         Y1      "  y       "      "       "            "
C         X2       " x       "      "    second          "
C         Y2         y       "      "       "            "
C
C   Written by K F Hartley at RGO on 25/2/82
C
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT.NE.0) THEN
C
C      If the ARGS is not available the say so
C      and return illegal values.
C
         CALL WRUSER('Args not available',ISTAT)
         X1=0.0
         Y1=0.0
         X2=0.0
         Y2=0.0
      ELSE
C
C   Otherwise generate the cursor etc.
C
         CALL ARGS_CUROP('4','W')
         CALL ARGS_OVOP(8,'G')
C
C   Loop around waiting for a vaild location to be selected.
C
         ID=0
         DO WHILE (ID.EQ.0)
            CALL ARGS_RDCUR(' ',ID,IB,X1,Y1)
         END DO
C
C   and then plot its location.
C
         CALL ARGS_PLCUR(ID,X1,Y1,N,M)
C
C   Repeat the operation, but this time the point must be in the
C   same image as the first one. (ID1=ID)
C
         ID1=0
         DO WHILE (ID1.NE.ID)
            CALL ARGS_RDCUR(' ',ID1,IB,X2,Y2)
         END DO
         CALL ARGS_PLCUR(ID,X2,Y2,N,M)
         CALL ARGS_CURCL
         CALL ARGS_OVCL(8,.FALSE.)
      END IF
      END
      SUBROUTINE ARGS_PLCUR(ID,X,Y,N,M)
C
C   This subroutine plots a cross on the ARGS at a specific point,
C   which is the whole size of the selected image.
C
C   Parameters
C      INPUT
C         ID      The identifier of the selected image.
C         X       The x co-ordinate of the point
C         Y       The y co-ordinate of the point (Both in PIXEL units)
C         N       The first dimension of the image
C         M       The second dimension of the image.
C
C   Written by K F Hartley at RGO on 25/2/82
C
      REAL HX(2),HY(2),VX(2),VY(2)
C
C   Set up the positions of the ends of the cross.
C
      HX(1)=0.0
      HX(2)=REAL(N)
      HY(1)=Y
      HY(2)=Y
      VX(1)=X
      VX(2)=X
      VY(1)=0.0
      VY(2)=REAL(M)
C
C   and plot them.
C
C   NOTE that the ARGS must have been prepared for plotting
C   in the calling segment.
C
      CALL ARGS_POLYL(ID,2,HX,HY,ISTAT)
      CALL ARGS_POLYL(ID,2,VX,VY,ISTAT)
      END
      SUBROUTINE PARMIN(X,Y,XMIN)
C
C   This subroutine takes three pairs of x,y values
C   and in effect fits a parabola to them.
C   It uses the fit to find the x position of the minimal
C   value of that parabola.
C
C   Parameters
C      INPUT
C         X      An array containing 3 X values.
C         Y      The array containing the corresponding Y values.
C         XMIN   The location of the minimum.
C
C   Written by K F Hartley at RGO on 25/2/82
C
      REAL X(3),Y(3)
C
C   Consider a quadratic function
C
C      Y = A + B*X + C*X*X
C
C   Then the minimum occurs where the derivitive is zero
C      ie B + 2*C*X = 0
C   hence  XMIN = -B/2C
C
      Y12=Y(1)-Y(2)
      Y23=Y(2)-Y(3)
      X12=X(1)-X(2)
      X23=X(2)-X(3)
      XX12=X(1)*X(1)-X(2)*X(2)
      XX23=X(2)*X(2)-X(3)*X(3)
      IF (Y12.EQ.0.0.OR.Y23.EQ.0.0) THEN
         XMIN=0.0
      ELSE
         XMIN= -0.5 * (XX23/Y23-XX12/Y12) / (X12/Y12-X23/Y23)
      END IF
      END
