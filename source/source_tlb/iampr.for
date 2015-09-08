      PROGRAM IAMPR
C+
C   IAMPR
C
C     OUTPUTS IMAGE PARAMETERS FROM IAM TO .LP
C*      user prompted for file of params
C	only a fixed number of params for each image are output
C
C   Given         (program parameters)
C   PARAMS    (RA)    array of image parameters
C
C   Given (descriptors to params)
C   SKY      (R)    adopted sky background
C   THRLD    (R)    adopted threshold
C   AREA     (I)    adopted area cut
C   SKYMAG   (R)    adopted sky background magnitude in one pixel
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

      INTEGER AREA
      REAL SKY,THRLD,SKYMAG
      INTEGER NAXPAR(2),NPPAR,ISTATUS
      INTEGER JDUM
C*  chars for reading descriptors
      CHARACTER*5 CAREA
      CHARACTER*20 CSKY,CTHRLD,CSKYMAG

      ISTATUS=0
      CALL INPICR('PARAMS','ENTER PARAMS FILE',2,NAXPAR,NPPAR,ISTATUS)
C*  input descriptors and later change chars-->nos
      CALL RDDSCR('PARAMS','SKY',1,CSKY,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','THRLD',1,CTHRLD,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','AREA',1,CAREA,JDUM,ISTATUS)
      CALL RDDSCR('PARAMS','SKYMAG',1,CSKYMAG,JDUM,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        READ(CSKY,'(BN,F20.0)') SKY
        READ(CTHRLD,'(BN,F20.0)') THRLD
        READ(CAREA,'(BN,I5)') AREA
        READ(CSKYMAG,'(BN,F20.0)') SKYMAG
        CALL IAMPRS(NAXPAR(1),NAXPAR(2),%VAL(NPPAR),
     :              SKY,THRLD,AREA,SKYMAG)
      ENDIF
      END
