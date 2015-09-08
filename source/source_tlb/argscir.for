C+
C    ARGSCIR
C
C    throws up circular args cursor and when button hit
C    returns centre point and diameter in pixel coords.
C    Calls CIRCUR2 to do this.
C    if overlapping pictures then values for one on top returned.
C    0 returned in all parameters if an error occurs
C
C    Returned (program paremeters) to environment
C      CX	(R)		cursor centre-x in pixel coords
C      CY	(R)		cursor centre-y in pixel coords
C      DIAM     (R)             diameter of circle in pixel coords
C      IMAGEN	(I)		data base number of image which
C                               CX,CY refer to
C
C    Douglas Tudhope    ROE    August 1982
C-


      REAL CX,CY,DIAM
      INTEGER IMAGEN

C*  local variables
      INTEGER STATUS,NIMAGES
C*  params for CIRCUR2 have to be integers
      INTEGER ICX,ICY,IDIAM

C*  initialise all parameters to 0, so can return 0 if error
      IMAGEN=0
      DIAM=0.0
      CX=0.0
      CY=0.0

      STATUS=0
      CALL SRINIT(0,.FALSE.,STATUS)
C*  test there is an image on args !
      CALL ARGS_NUMIM(NIMAGES)
      IF (NIMAGES.EQ.0) THEN
        CALL WRERR('NOPICT')
      ELSE
C*  initialise params for CIRCUR2
        ICX=256
        ICY=256
        IDIAM=50
        CALL CIRCUR2(ICX,ICY,IDIAM,IMAGEN)
          IF (IMAGEN.EQ.0) THEN
C*  cursor not on an image or abort
            CALL WRERR('MISSED')
          ELSE
            DIAM=IDIAM
            CX=ICX
            CY=ICY
          ENDIF
      ENDIF
      CALL WRITER('DIAM',DIAM,STATUS)
      CALL WRITEI('IMAGEN',IMAGEN,STATUS)
      CALL WRITER('CX',CX,STATUS)
      CALL WRITER('CY',CY,STATUS)
      IF (STATUS.NE.0) CALL WRERR('BADWRITE')
      END
