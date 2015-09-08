      SUBROUTINE IAMANALS(MAP,MXLEN,MXSCAN,BCKGND,THRESH,ARECUT,SKYMAG,
     :         NUMPAR,NUMLEN,ANALYSER,ICAT,STORE,ADDR,MARKER,SCAN,
     :         STATUS)
*+
*   IAMANALS
*
*   COSMOS ANALYSER
*
*   Given      (arguments)
*
*     MAP(MXLEN,MXSCAN) = 2-D PICTURE ARRAY
*     BCKGND = SKY BACKGROUND LEVEL
*     THRESH = THRESHOLD SETTING
*     ARECUT = AREA CUT. SMALLEST NO. OF PIXELS ACCEPTED IN AN IMAGE.
*     SKYMAG = SKY BACKGROUND MAGNITUDE IN ONE PIXEL.
*     NUMPAR = MAXIMUM NO. OF IMAGE PARAMETERS ALLOWED.
*     NUMLEN = MAXIMUM NO. OF IMAGES ALLOWED.
*
*   Returned   (arguments)
*
*     ANALYSER(NUMPAR,NUMLEN) = LIST OF PARAMETERS FOR EACH IMAGE.
*     ICAT = NUMBER OF IMAGES FOUND
*     STORE(16,(MXLEN+1)/2))  DA  -
*     ADDR(MXLEN)             IA  -     WORKSPACE
*     MARKER(MXLEN+1)         IA  -
*     SCAN(MXLEN)             RA  -
*
*   Returned   (stacks)
*   FREE        unused indices to data storage area
*   PSSTAK      previous-scan connectivity flags
*   OBSTAK      objects currently being traversed
*
*   Returned (status)
*   STATUS  (I) Return status;
*               = 0 - success.
*               = 1 - More than the max. permitted no. of objects present.
*
*   Subroutines called :
*   INITSTK,GETSTK,PUTSTK,POP,NEWPORT,PROMAR,ADDPIX : E2DLIB
*
*     The 19 parameters calculated in this version are :-
*     1  -  x-centroid unweighted.
*     2  -  y-centroid unweighted.
*     3  -  semi-major axis unweighted.
*     4  -  semi-minor axis unweighted.
*     5  -  orientation unweighted.
*     6  -  x-centroid weighted.
*     7  -  y-centroid weighted.
*     8  -  semi-major axis weighted.
*     9  -  semi-minor axis weighted.
*    10  -  orientation weighted.
*    11  -  area.
*    12  -  total intensity.
*    13  -  maximum intensity.
*    14  -  minimum X value in the image.
*    15  -  maximum "   "   "   "    "  .
*    16  -  minimum Y   "   "   "    "  .
*    17  -  maximum "   "   "   "    "  .
*    18  -  ellipticity unweighted.
*    19  -  ellipticity weighted.
*
*   B.D.Kelly/ROE/1981
*   A C Davenhall./ROE/18.1.84 {Modified to check that the max.
*                               permitted no. of objects are not
*                               exceeded.}
*   A C Davenhall./ROE/12.6.84 {Modified so X & Y min. & max. are
*                               included amongst the fields.}
*   Algorithm by R.K.Lutz
*-
*******************************************************************
*
*   The algorithm searches through the picture finding pixels
*   above the given threshold and merging their properties into
*   the individual objects to which they belong.
*   The properties of objects which are not yet complete are
*   stored in the array STORE(no.of.props,max.no.of.objects).
*   The columns of STORE which are unused at any given instant are
*   recorded in the FREE stack. When the first pixel of a new object
*   is found, the FREE stack is popped, and the properties of the
*   new object are accumulated in the corresponding part of STORE.
*   When all the pixels belonging to a given object have been found,
*   its properties are calculated and put into the array ANALYSER.
*   The index of the corresponding part of STORE can then be pushed
*   back onto the FREE stack.
*   The elements of STORE containing a given property are accessed
*   using the following mnemonics for the index.
*
*      AREA   - no. of pixels in object
*      INTMAX - maximum intensity value
*      SIX    - sum of I * X
*      SIXX   - sum of I * X**2
*      SIXY   - sum of I * X * Y
*      SIY    - sum of I * Y
*      SIYY   - sum of I * Y**2
*      SUMI   - sum of intensities, I
*      SUMX   - sum of X-coordinates
*      SUMY   - sum of Y-coordinates
*      SXX    - sum of X**2
*      SXY    - sum of X * Y
*      SYY    - sum of Y**2
*      XMAX   - maximum X-coordinate in object
*      XMIN   - minimum X-coordinate in object
*      YEXT   - Y-extent in pixels of object
*
******************************************************************

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT
      INTEGER STATUS

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)


      INTEGER MXLEN,MXSCAN,NUMPAR,NUMLEN,MAXOBJ
      INTEGER ARECUT,COMPL,CS,F,F0,I,ICAT,IMAGE,INCOMP,
     &        IYPOS,NEWMAR,NONIM,NOTSET,OVER,PS,S,S0

      REAL ANALYSER(NUMPAR,NUMLEN), MAP(MXLEN,MXSCAN), SCAN(MXLEN+1)
      REAL BCKGND,SKYMAG,THRESH,ZEROMAG
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2)


      INTEGER ADDR(MXLEN),MARKER(MXLEN+1)
      INTEGER OBJECT(3)

      LOGICAL CATFUL
*
*   VALUES USED AS IMAGE-CONNECTIVITY MARKERS
*   IN ARRAY MARKER(1:MXLEN+1)
*
*     S  -  FIRST PIXEL OF FIRST SEGMENT OF AN OBJECT SECTION
*    S0  -  FIRST PIXEL OF NON-FIRST SEGMENT OF AN OBJECT SECTION
*     F  -  PIXEL FOLLOWING LAST SEGMENT OF AN OBJECT SECTION
*    F0  -  PIXEL FOLLOWING NON-LAST SEGMENT OF AN OBJECT SECTION
*
      PARAMETER (S=1,F=2,S0=3,F0=4)
*
*   POSSIBLE VALUES OF THE IMAGE STATUSES CS AND PS
*
      PARAMETER (NONIM=0,IMAGE=5,COMPL=6,INCOMP=7)
*
*
*
*
*   FIXED VALUES
*
      OVER=MXLEN+2
      NOTSET=MXLEN+2
      MAXOBJ=(MXLEN+1)/2
*
*   CLEAR ARRAY FOR OUTPUT PARAMETERS
*
      DO J=1,NUMLEN
         DO I=1,NUMPAR
            ANALYSER(I,J)=0.0
         ENDDO
      ENDDO
*
*   Clear storage area
*
      DO J=1,16
         DO I=1,MAXOBJ
            STORE(I,J)=0.0
         ENDDO
      ENDDO
*
*   INITIALISE MARKER
*  -------------------
*
*     THE ANALYSIS PROCEEDS THROUGH ONE ROW OF THE IMAGE AT
*     A TIME. MARKER(1:MXLEN+1) CONTAINS MARKERS SHOWING THE
*     CONNECTIVITY OF THE PREVIOUS ROW.
*
      DO J=1,MXLEN+1
         MARKER(J)=0
      ENDDO
*
*   Set up stack pointing to FREE data area
*
      CALL INITSTK('FREE',MAXOBJ)
*
*   INITIALISE COUNT OF COMPLETED IMAGES...
*
      ICAT=0
*
*   ...AND SET A FLAG SAYING THE CATALOGUE IS NOT YET FULL.
*
      CATFUL=.FALSE.
*
*     PROCESS ONE ROW OF THE IMAGE AT A TIME
*    ----------------------------------------
*
      DO INSCAN=1,MXSCAN+1
         IYPOS=INSCAN
*
*      EXTRACT SCAN
*
         IF(INSCAN.LE.MXSCAN) THEN
            DO J=1,MXLEN
               SCAN(J)=MAP(J,INSCAN)
            ENDDO
         ELSE
            DO J=1,MXLEN
               SCAN(J)=THRESH-1.0
            ENDDO
         ENDIF
         SCAN(MXLEN+1)=0.0
*
*      CS AND PS ARE THE STATUS OF THE CURRENT IMAGE.
*      (CURRENT STATUS, PREVIOUS STATUS)
*      CS FLAGS WHETHER THE CURRENT PIXEL IS IMAGE OR NONIM(AGE).
*      PS = IMAGE WHILE CURRENTLY TRAVERSING OBJECT ON LAST SCAN.
*         = COMPL(ETE) WHILE TRAVERSING NON-OBJECT ON LAST SCAN,
*           AND CURRENT OBJECT NOT YET CONNECTED TO ANY LAST-SCAN
*           IMAGE SECTIONS WITH SEGMENTS STILL TO COME.
*         = INCOMP(LETE) WHILE TRAVERSING NON-OBJECT ON LAST SCAN
*           AND CURRENT OBJECT CONNECTED TO LAST-SCAN IMAGE
*           SECTIONS WITH SEGMENTS STILL TO COME.
*
         PS=COMPL
         CS=NONIM
*
*     OBSTAK IS THE OBJECT DATA STACK (CURRENT IMAGE)
*       THIS IS A TRIPLE STACK GIVING THE THREE VALUES
*       OBJECT(1) = POSITION OF THE LEFTMOST PIXEL IN THE CURRENT
*                   SCAN BELONGING TO THE CURRENT IMAGE.
*       OBJECT(2) = POSITION OF THE LAST F0 MARKER WRITTEN FOR THE
*                   CURRENT IMAGE.
*       OBJECT(3) = INDEX OF OBJECT IN THE STORE ARRAY.
*
*     PSSTAK IS THE STACK FOR THE STATUS PS
*
         CALL INITSTK('OBSTAK',OBJECT)
         CALL INITSTK('PSSTAK',PS)
*
*     START NEXT ROW
*    ----------------
*
         DO I=1,MXLEN+1
            IF((MARKER(I).NE.0).OR.(CS.NE.0).OR.(SCAN(I).GT.THRESH))
     :          THEN
               IF (.NOT.CATFUL) THEN

                  NEWMAR=MARKER(I)
                  MARKER(I)=0

                  IF(SCAN(I).GT.THRESH) THEN
                     IF(CS.EQ.NONIM) CALL NEWPORT
     :                              (COMPL,CS,I,IMAGE,MARKER,MXLEN,
     :                               NEWMAR,NOTSET,OVER,PS,S,S0,STORE)
                     IF(NEWMAR.NE.0) CALL PROMAR
     :                              (ADDR,ANALYSER,ARECUT,COMPL,CS,F,F0,
     :                               I,ICAT,IMAGE,INCOMP,IYPOS,MARKER,
     :                               MXLEN,NEWMAR,NONIM,NOTSET,NUMLEN,
     :                               NUMPAR,PS,S,S0,STORE)
*
*   Add the current pixel to the top image on the data stack
*
                     CALL ADDPIX(BCKGND,I,MXLEN,SCAN(I),STORE)

                  ELSE

                     IF(NEWMAR.NE.0) CALL PROMAR
     :                              (ADDR,ANALYSER,ARECUT,COMPL,CS,F,F0,
     :                               I,ICAT,IMAGE,INCOMP,IYPOS,MARKER,
     :                               MXLEN,NEWMAR,NONIM,NOTSET,NUMLEN,
     :                               NUMPAR,PS,S,S0,STORE)
*
*   Set a termination flag if the max. permitted no. of objects
*   have been detected.
*
                  IF (ICAT.GE.NUMLEN) CATFUL=.TRUE.
*
*   END PORTION
*  -------------
*
*
*   If current pixel image, then
*      set current to nonimage
*      If on the previous scan within an object,
*      or segments of current object known to be yet to come, then
*            set end of segment marker at current position
*            put position on top of end-marker stack
*      else
*            pop PS from PSSTAK
*            set end-of-image marker at current position
*            increment Y-extent for top of data area stack
*            put top of address stack at address corresponding
*                     to start-of-image marker
*            pop object data stack
*      endif
*   endif
*
                     IF(CS.EQ.IMAGE) THEN
                        CS=NONIM
                        IF (PS.NE.COMPL) THEN
                           MARKER(I)=F0
                           CALL GETSTK('OBSTAK',OBJECT)
                           OBJECT(2)=I
                           CALL PUTSTK('OBSTAK',OBJECT)
                        ELSE
                           CALL POP('PSSTAK',PS)
                           MARKER(I)=F
                           CALL POP('OBSTAK',OBJECT)
                           STORE(YEXT,OBJECT(3))=STORE(YEXT,OBJECT(3))+1.0
*
*     ADDR(J) = index in parameter arrays of the object starting
*                  at pixel J. ie. SCAN(J) = first pixel of the
*                  object in the current scan.
*
                           ADDR(OBJECT(1))=OBJECT(3)
                        ENDIF
                     ENDIF
*
*   HAVE FINISHED PORTION
*  -----------------------
*
                  ENDIF

               ENDIF
            ENDIF
         ENDDO
      ENDDO
*
*     convert the summed-intensity parameter to magnitudes,
*     assuming sky background is SKYMAG magnitudes per pixel.
*
      IF(BCKGND.GT.0.0) ZEROMAG=SKYMAG+2.5*ALOG10(BCKGND)
      DO J=1,ICAT
         IF(ANALYSER(12,J).GT.0.0)
     :        ANALYSER(12,J)=ZEROMAG-2.5*ALOG10(ANALYSER(12,J))
      ENDDO
*
*   calculate ellipticities
*
      DO J=1,ICAT
         IF(ANALYSER(8,J).GT.0.01) THEN
            ANALYSER(19,J)=1.0-ANALYSER(9,J)/ANALYSER(8,J)
         ENDIF
         IF(ANALYSER(3,J).GT.0.01) THEN
            ANALYSER(18,J)=1.0-ANALYSER(4,J)/ANALYSER(3,J)
         ENDIF
      ENDDO

 
*
*   Set the return status, depending on whether the max. number
*   of objects were detected or not.
*
      IF (.NOT.CATFUL) THEN
         STATUS=0
      ELSE
         STATUS=1
      END IF

      END
