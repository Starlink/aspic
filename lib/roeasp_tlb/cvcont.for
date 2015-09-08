      SUBROUTINE CVCONT(NX1,NX2,ARRP,ARRT,SCAL,NCON,BASE,CINT,
     :                NSIZE,SIGMA,TITLE,SMOOTH,DEVICE,ARRS,
     :                WK1,WK2)
*+
*
*    CVCONT
*
*    Purpose : To produce the anotated contour and vector plots .
*
*    Parameters :
*
*    Given     Type     Usage
*    NX1        I       X-dimension of data images
*    NX2        I       Y-dimension of data images
*    ARRP       RA      Total polarization image
*    ARRT       RA      Polarization angle image
*    SCAL       R       Scale factor for polarization vectors
*    NCON       I       Number of contours for the plot
*    BASE       R       Value for the first contour level
*    CINT       R       Interval between contours
*    NSIZE      I       Width ( in pixels ) of the smoothing box
*    SIGMA      R       Sigma ( in pixels ) of the smoothing gaussian
*    TITLE      C       Title for the plot
*    SMOOTH     C       Type of smoothing used
*    DEVICE     C       Type of plotting device to be used
*    ARRS       RA      Image to be contoured
*    WK1        RA      Workspace for plotting vectors
*    WK2        RA      Workspace for plotting vectors
*
*    Subroutines called :
*    T4014 , T4010 , ARGS , VERSA , DEVEND   : ROEFINGS
*    PICCLE , MOVTO2 , CHAHOL                : ROEFINGS
*    CVGRAF , POLPLT                         : E2DASP
*    WRUSER                                  : STARLINK
*    SRINIT                                  : ARGSLIB
*
*    original author C Aspin
*    D.W.T.Baines/ROE/Feb 1982/
*
*-
      INTEGER NAXIS2(2) , IARG , IFAIL , INON , ISTAT 
      INTEGER NCON , NPTR2 , NSIZE , NX1 , NX2
      REAL BASE , CINT , SCAL , SIGMA
      REAL ARRP(NX1,NX2) , ARRS(NX1,NX2) , ARRT(NX1,NX2)
      REAL WK1(NX1,NX2) , WK2(NX1,NX2)
      CHARACTER SMOOTH*(*) , DEVICE*(*) , TITLE*(*)
*
*    set status variables to 0
*
      IARG = 0
      INON = 0
      ISTAT = 0
*
*    set up the device specified by DEVICE
*
      IF ( DEVICE .EQ. 'T4014' ) THEN
         CALL T4014
      ELSE IF ( DEVICE .EQ. 'T4010' ) THEN
         CALL T4010
      ELSE IF ( DEVICE .EQ. 'ARGS' ) THEN
         CALL SRINIT ( 0,.TRUE.,IFAIL)
*
*       check that the args has been allocated o.k.
*
         IF ( IFAIL.EQ.0 ) THEN
            CALL ARGS
         ELSE
*
*          output a warning and set the args staus variable IARG
*
            CALL WRUSER ('WARING ! Error allocating the ARGS',
     :                   ISTAT)
            IARG = -1
         ENDIF
      ELSE IF ( DEVICE .EQ. 'VERSATEK' ) THEN
         CALL VERSA
      ELSE
*
*       here if nonsensical device specified
*
         CALL WRUSER ('Graphics device not known',ISTAT)
         INON = -1
      ENDIF
*
*    check for failure to allocate the args or unknown graphics device
*
      IF ( IARG .EQ. 0  .AND.  INON .EQ. 0 ) THEN
*
*       new frame on the graphics device
*
         CALL PICCLE
*
*       plot the anotated contour plot
*
         CALL CVGRAF(NX1,NX2,ARRS,SMOOTH,NSIZE,SIGMA,NCON,BASE,
     :              CINT,TITLE,SCAL)
*
*       plot the polarization vectors
*
         CALL POLPLT (SCAL,NX1,NX2,ARRP,NX1,NX2,ARRT,
     :                NX1,NX2,WK1,WK2)
         CALL MOVTO2(.0,.0)
         CALL CHAHOL('.*.')
         CALL DEVEND
      END IF
      END
