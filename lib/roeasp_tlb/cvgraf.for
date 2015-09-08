      SUBROUTINE CVGRAF(NX1,NX2,ARRS,SMOOTH,NSIZE,SIGMA,NCON,BASE,
     :                  CINT,TITLE,SCAL)
*+
*
*      CVGRAF
*
*     Purpose : To plot an anotated contour map of an image . Used
*               by CVCONT .
*
*     Original source C Aspin , routines DRWGRf and ANOTCV
*
*     Parameters :
*
*     Given     Type     Usage
*     NX1        I       X-dimension of image
*     NX2        I       Y-dimension of image
*     ARRS       RA      Image to be contoured
*     SMOOTH     C       Type of smoothing used
*     NSIZE      I       Size of the smoothing box
*     SIGMA      R       Sigma of the gaussian for smoothing
*     NCON       I       Number of contours used for the contour plot
*     BASE       R       Value of the first contour
*     CINT       R       Interval between contours
*     TITLE      C       Title for the plot
*     SCAL       R       Scale factor for the polarization vectors
*
*     Subroutines called :
*
*     CHAESC , CHAFIX , CHAFLO , CHAHOL , CHAINT , CHASIZ : ROEFINGS
*     DEVSPE , LINTO2 , MOVTO2 , VUPORT , WINDOL          : ROEFINGS
*     CONMAP , AXIS                                       : E2DASP
*
*     D.W.T.Baines/ROE/Feb 1983/
*
*-
      INTEGER NX1 , NX2 , NCON , NSIZE
      INTEGER ISTART , IFIN , JSTART , JFIN
      REAL ARRS(NX1,NX2) , BASE , CINT , SIGMA , SCAL
      REAL XAX , YAX , XINC , YINC , XL , YL , XM , YM , XE
      CHARACTER SMOOTH*(*) , TITLE*(*)
      LOGICAL CNTRLC
*
*     set up the plotting area
*
      CALL VUPORT(.0,1.0,.0,1.0)
      CALL WINDOL(.0,1.0,.0,1.0)
*
*     set the character size and device speed
*
      CALL CHASIZ(.01)
      CALL DEVSPE(4800)
*
*     write out the title for the plot
*
      CALL MOVTO2( .24 , .900 )
      CALL CHAHOL (%REF(TITLE)//'*.')
*
*     write out the image and contouring information 
*
      CALL MOVTO2 ( 0.24 , .875 )
      CALL CHAHOL('IMAGE SIZE = *.')
      CALL CHAINT(NX1,4)
      CALL CHAHOL(',*.')
      CALL CHAINT(NX2,4)
      CALL CHAHOL ('  NUMBER OF CONTOURS = *.')
      CALL CHAINT(NCON,4)
      CALL MOVTO2( 0.24 , 0.850 )
      CALL CHAHOL ('CONTOUR BASE LEVEL = *.')
      CALL CHAFLO(BASE,13)
      CALL MOVTO2 ( 0.24 , 0.825 )
      CALL CHAHOL('INTERVAL BETWEEN CONTOURS = *.')
      CALL CHAFLO(CINT,13)
*
*     write out the smoothing information
*
      CALL MOVTO2( 0.24 , 0.075 )
      CALL CHAHOL ('SMOOTHING = *.')
      IF ( SMOOTH .EQ. 'GAUSSIAN' ) THEN
         CALL CHAHOL ('GAUSSIAN OF SIGMA *.')
         CALL CHAFIX (SIGMA,6,2)
         CALL CHAHOL (' PIXELS*.')
      ENDIF
      IF(SMOOTH .EQ. 'NONE')CALL CHAHOL('NONE*.')
      IF(SMOOTH .EQ. 'BOX')CALL CHAHOL('BOX AVERAGE*.')
      IF ( SMOOTH .NE. 'NONE' ) THEN
         CALL CHAHOL (' OVER *.')
         CALL CHAINT (NSIZE,2)
         CALL CHAHOL (' PIXELS*.')
      ENDIF
*
*     draw the 100% polarization marker
*
      CALL MOVTO2( 0.24 , 0.050 )
      XE = ( ( 0.6*SCAL*4.0 ) / (REAL(NX1) ) ) + 0.24
      CALL LINTO2( XE , 0.050 )
      CALL CHAHOL('  = 100% POLARIZATION*.')
      CALL MOVTO2(.0,.0)
*
*     Here to perform the contour plotting
*
      CNTRLC = .FALSE.
*
*     calculate the window size for the plot from the array size
*
      XL=1.
      XM=FLOAT(NX1)
      YL=1.
      YM=FLOAT(NX2)
      XAX = XM + 1.
      YAX = YM + 1.
      XINC = XM * 0.3
      YINC = YM * 0.3
      XL = XL - XINC
      XM = XM + XINC
      YL = YL - YINC
      YM = YM + YINC
*
*     set up the new plotting area
*
      CALL VUPORT ( 0. , 1. , 0. , 1. )
      CALL WINDOL ( XL , XM , YL , YM )
*
*     draw in the axes
*
      CALL AXIS ( 0. , XAX , 0. , YAX , 'X' , 'Y' )
*
*     reset the escape character to * ( routine AXIS changes it )
*
      CALL CHAESC ('*')
*
*     set up the contour plot limits
*
      ISTART = 1
      JSTART = 1
      IFIN = NX1
      JFIN = NX2
*
*     perform the contouring
*
      CALL CONMAP(ARRS,NX1,NX2,ISTART,IFIN,JSTART,JFIN,BASE,CINT,
     :            NCON,CNTRLC)
      END
