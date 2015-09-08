      PROGRAM IGJOB
C+
C   IGJOB
C
C      produces a Versatek plot file of IAM images with FINGS
C      and then tells user to plot it (eg with VPLOT at ROE)
C	part of IAM suite
C
C   Given         (program parameters)
C   PARAMS    (RA)    array of image parameters - iyext=no. of params
C
C   Given (descriptors in params)
C   MAXDIM    (R)    size of larger dimesion of original image
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

*   Plot ellipses matching IAM parameters.
*   Images of area less than 2 are not plotted.

      INTEGER JDUM
      REAL MAXDIM
      INTEGER NAXPAR(2),NPPAR,ISTATUS
C*  char for reading descriptor
      CHARACTER*20 CMAXDIM

      ISTATUS=0
      CALL INPICR('PARAMS','ENTER PARAMS FILE',2,NAXPAR,NPPAR,ISTATUS)
C*  input descriptor maxdim
      CALL RDDSCR('PARAMS','MAXDIM',1,CMAXDIM,JDUM,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        READ(CMAXDIM,'(BN,F20.0)') MAXDIM
        CALL IGJOBS(NAXPAR(1),NAXPAR(2),%VAL(NPPAR),MAXDIM)
      ENDIF
      END
