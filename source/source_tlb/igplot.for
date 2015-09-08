      PROGRAM IGPLOT
C+
C   IGPLOT
C
C	does a graphics plot of IAM images to ARGS quadrant
C	user prompted for params file
C
C
C   Given         (program parameters)
C   PARAMS    (RA)    array of image parameters - iyext=no. of objects
C   REPLY      (I)    quadrant on args (called from quad)
C   Given (descriptor to params)
C   MAXDIM     (R)    size of larger dimension of original image
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

*   Plot ellipses matching IAM parameters but scaled down.
*   Images of area less than 2 are not plotted.

      REAL MAXDIM
      INTEGER NAXPAR(2),NPPAR,ISTATUS
      INTEGER JDUM,IDUM,IXR,IYR
C*  char for reading descriptor
      CHARACTER*20 CMAXDIM

      ISTATUS=0
      CALL INPICR('PARAMS','ENTER PARAMS FILE',2,NAXPAR,NPPAR,ISTATUS)
      CALL QUAD(IXR,IYR,IDUM)
C*  input descriptor maxdim
      CALL RDDSCR('PARAMS','MAXDIM',1,CMAXDIM,JDUM,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        READ(CMAXDIM,'(BN,F20.0)') MAXDIM
        CALL IGPLOTS(NAXPAR(1),NAXPAR(2),%VAL(NPPAR),MAXDIM,IXR,IYR)
      ENDIF
      END
