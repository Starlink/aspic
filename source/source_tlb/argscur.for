C+
C    ARGSCUR
C
C    Throws up the args cursor and returns x,y (using the args
C    data base to get the coordinates in pixels) when a button
C    is hit. If there are overlapping pictures then values for
C    the one on top are returned.
C
C    Coords returned to environment in CX,CY
C
C    Checks for cursor not on an image and for basic error in
C    accessing cursor, gets point in args coords by low level
C    LRDC and then calls IMAGEN to get the image number from
C    the data base and then calls ATOP to convert args into
C    pixel coords.
C    0 returned in all params if an error
C
C    Given (program parameters)
C      LAMPS	(C)		string for which lamps are to
C                               be illuminated - eg '123'
C      CURCOL	(C)		character for cursor colour - eg 'B'
C
C    Returned (program paremeters) to environment
C      CX	(R)		cursor x coord in pixel coords
C      CY	(R)		cursor y coord in pixel coords
C      BUTTON	(I)		button hit
C      IMAGEN	(I)		data base number of image which
C                               CX<,CY refer to
C
C    These 'parameters' are all obtained and returned from environment
C    and can be read or set in DSCL.
C    LAMPS and CURCOL are defaulted in the connection file and the user
C    is not normally expected to change their values (but can set them
C    in DSCL)
C
C    Douglas Tudhope    ROE    April 1982
C-


      REAL CX,CY
      INTEGER BUTTON,IMAGEN
      CHARACTER*4 LAMPS,CURCOL

C*  local variables
      INTEGER STATUS,NIMAGES,AX,AY,PX,PY

C*  initialise all output program parameters to 0, so will return 0 if an error occurs
      IMAGEN=0
      BUTTON=0
      CX=0.0
      CY=0.0

      STATUS=0
      CALL SRINIT(0,.FALSE.,STATUS)
      CALL READC('CURCOL',' ','W','B','Y',CURCOL,STATUS)
      CALL READC('LAMPS',' ','1','1','4444',LAMPS,STATUS)
      IF (STATUS.NE.0) THEN
        CALL WRERR('ERROR1')
      ELSE
C*  test there is an image on args !
        CALL ARGS_NUMIM(NIMAGES)
        IF (NIMAGES.EQ.0) THEN
          CALL WRERR('ERROR2')
        ELSE
          CALL ARGS_CUROP(LAMPS,CURCOL)
          CALL ARGS_LRDC(BUTTON,AX,AY)
          CALL ARGS_CURCL
          IF (BUTTON.EQ.0) THEN
C*  basic error with cursor
            CALL WRERR('ERROR3')
          ELSE
            CALL IDENIM(AX,AY,IMAGEN)
            IF (IMAGEN.EQ.0) THEN
C*  cursor not on an image
              CALL WRERR('ERROR4')
            ELSE
              CALL ARGS_ATOP(IMAGEN,AX,AY,PX,PY,STATUS)
              CX=PX
              CY=PY
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      CALL WRITEI('IMAGEN',IMAGEN,STATUS)
      CALL WRITEI('BUTTON',BUTTON,STATUS)
      CALL WRITER('CX',CX,STATUS)
      CALL WRITER('CY',CY,STATUS)
      IF (STATUS.NE.0) CALL WRERR('ERROR5')
      END
