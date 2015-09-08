	SUBROUTINE HELP
C
C     HELP PACKAGE FOR FRAME
C
	IMPLICIT INTEGER (A-Z)
	CHARACTER MENU*28, IN*8
	DATA MENU/'LOAD  LIST  ROTA  CALC  TRAN'/
C
C    DISPLAY AVAILABLE FUNCTIONS
C
	CALL WRUSER('FRAME HELP Routine',STATUS)
	CALL WRUSER('Information is available on the following functions
     *',STATUS)
	CALL WRUSER(MENU,STATUS)
	CALL CNPAR('COMMAND',STATUS)
	CALL RDKEYC('COMMAND',.FALSE.,1,IN,I,STATUS)
	IF(IN.EQ.'LOAD') THEN
	CALL WRUSER('LOAD command',STATUS)
	CALL WRUSER(' ',STATUS)
	CALL WRUSER('Registration points are loaded into a STARLINK',
     *STATUS)
	CALL WRUSER('file. Data may be entered in one of three ways',
     *STATUS)
	CALL WRUSER(' 1. direct from the keyboard',STATUS)
	CALL WRUSER(' 2. from two files',STATUS)
	CALL WRUSER(' 3. from a displayed image',STATUS)
      CALL WRUSER(' ',STATUS)
      CALL WRUSER('Parameters for LOAD are:',STATUS)
      CALL WRUSER(' KEYING : Default ''K'' for entering from keyboard',
     *            STATUS)
      CALL WRUSER('          Type ''F'' to enter data from two files',
     *            STATUS)
      CALL WRUSER('         To enter data from a displayed image',
     *            STATUS)
      CALL WRUSER('         use the program ''CLOAD''',STATUS)
      CALL WRUSER(' RFILE  : Name of file into which data is put',
     *            STATUS)
      CALL WRUSER(' NUMBER : Number of points to be input',STATUS)
      CALL WRUSER(' IN     : Name of ''INPUT'' file', STATUS)
      CALL WRUSER(' OUT    : Name of ''OUTPUT'' file', STATUS)
      CALL WRUSER(' POINT  : Requests input of one data point in the',
     *            STATUS)
      CALL WRUSER('          form X, Y, Weight',STATUS)
      ELSE IF (IN.EQ.'LIST') THEN
      CALL WRUSER('LIST command',STATUS)
      CALL WRUSER(' ',STATUS)
      CALL WRUSER(' A file containing the coefficients determined by',
     *             STATUS)
      CALL WRUSER('using ''CALC'' is listed',STATUS)
      CALL WRUSER(' LIST has one parameter :',STATUS)
      CALL WRUSER(' IN   : Name of file to be listed', STATUS)
      ELSE IF (IN.EQ.'ROTA') THEN
      CALL WRUSER('ROTA''TE'' command',STATUS)
      CALL WRUSER(' ',STATUS)
      CALL WRUSER(' Coefficients are setup in a file to rotate an image'
     *            ,STATUS)
      CALL WRUSER('clockwise about a given point', STATUS)
      CALL WRUSER('Parameters are :', STATUS)
      CALL WRUSER(' ROTATE : Clockwise rotation of the image in degrees'
     *            , STATUS)
      CALL WRUSER(' ORIGIN : Point about which image is to be rotated',
     *             STATUS)
      CALL WRUSER(' IMAGE  : Name of file to contain coefficients',
     *             STATUS)
      ELSE IF (IN.EQ.'CALC') THEN
      CALL WRUSER('CALC function', STATUS)
      CALL WRUSER('CALC is invoked by a separate program', STATUS)
      CALL WRUSER('i.e. ''RUNSTAR CALC''', STATUS)
      CALL WRUSER('Parameters are :', STATUS)
      CALL WRUSER(' RFILE : Name of file containing registration points'
     *            , STATUS)
      CALL WRUSER('         This file has been set up by the ''LOAD''',
     *              STATUS)
      CALL WRUSER('         command.', STATUS)
      CALL WRUSER(' FIT   : Defines if 3, 6 or 8 parameter fit required'
     *            , STATUS)
      CALL WRUSER(' COFILE: Name of file to contain the coefficients',
     *             STATUS)
      CALL WRUSER(' ITERPR: Controls after how many iterations '
     *            , STATUS)
      CALL WRUSER('         printing is sent to the file',
     *              STATUS)
      CALL WRUSER('            ''PLATELOG.DAT''',STATUS)
      ELSE IF (IN.EQ.'TRAN') THEN
      CALL WRUSER('TRAN function', STATUS)
      CALL WRUSER(' ', STATUS)
      CALL WRUSER('TRAN is invoked by a separate program', STATUS)
      CALL WRUSER('i.e. ''RUNSTAR TRAN''', STATUS)
      CALL WRUSER('Parameters are :',STATUS)
      CALL WRUSER(' IN     : Name of file to be transformed', STATUS)
      CALL WRUSER(' COEFF  : Name of file containing transformation',
     *            STATUS)
      CALL WRUSER('          to be applied', STATUS)
      CALL WRUSER(' SIZE   : Size of output file, default is size of',
     *            STATUS)
      CALL WRUSER('          input file', STATUS)
      CALL WRUSER(' OUT    : Name of file into which transformed ',
     *            STATUS)
      CALL WRUSER('          image will be written', STATUS)
      ENDIF
      RETURN
      END
