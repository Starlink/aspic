      SUBROUTINE NEWPORT
     :  (COMPL,CS,I,IMAGE,MARKER,MXLEN,NEWMAR,NOTSET,OVER,PS,S,S0,STORE)
*+
*   NEWPORT
*
*   handles start of new image section for cosmos analyser
*
*     the current pixel is the first of a new image segment
*
*   Given      (arguments)
*   COMPL   I   fixed value, a possible state of PS
*   CS      I   flag for whether current pixel is image or nonimage
*   I       I   pixel number along current scan
*   IMAGE   I   fixed value, a possible state of CS and PS
*   MARKER  IA  array of connectivity markers
*   MXLEN   I   X-dimension of image frame
*   NEWMAR  I   connectivity marker for current pixel
*   NOTSET  I   fixed value, indicating undefined start-address
*               entry in object stack
*   OVER    I   fixed value, indicating values not yet entered int
*               STORE for an object
*   PS      I   flag for image connectivity on previous scan
*   S       I   fixed value, connectivity marker for start of object
*   S0      I   fixed value, connectivity marker for start
*               of object segment
*   STORE   DA  store for accumulating object parameters
*
*   Given      (stacks)
*   OBSTAK      objects currently being traversed
*   PSSTAK      previous-scan connectivity flags
*   FREE        unused indices to data storage area
*
*   Returned   (arguments)
*   CS      I   flag for whether current pixel is image or nonimage
*   STORE   DA  store for accumulating object parameters
*
*   Returned   (stacks)
*   OBSTAK      objects currently being traversed
*   PSSTAK      previous-scan connectivity flags
*   FREE        unused indices to data storage area
*
*   Subroutines called :
*   GETSTK,PUTSTK,PUSH,POP    : E2DLIB
*
*   B.D.Kelly/ROE/15.3.1982
*-
      INTEGER MXLEN
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2)
      INTEGER MARKER(MXLEN+1)
      INTEGER COMPL,CS,EMPTY,I,IMAGE,NEWMAR,NOTSET,OVER,PS,S,S0
      INTEGER OBJECT(3)

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)
*
*
*
*   START NEW PORTION
*  -------------------
*
      CS=IMAGE
*
*   If within an image on the previous scan, then
*      if not yet started image on this scan, then
*         set a start of image marker
*         set top of stack holding image starting position
*      else
*         set a start of segment marker
*      endif
*   else
*      push PS onto PSSTAK
*      push object data stack
*      set a start of image marker
*      set top of stack holding image starting position
*      set previous scan connectivity flag to COMPLete
*      if previous scan not just starting an image, then
*         pop new data storage area from top of free data stack
*         initialize new data area
*      endif
*   endif
*
      IF (PS.EQ.IMAGE) THEN
        CALL GETSTK('OBSTAK',OBJECT)
        IF (OBJECT(1).EQ.NOTSET) THEN
          MARKER(I)=S
          OBJECT(1)=I
          CALL PUTSTK('OBSTAK',OBJECT)
        ELSE
          MARKER(I)=S0
        ENDIF
      ELSE
        CALL PUSH('PSSTAK',PS)
        MARKER(I)=S
        OBJECT(1)=I
        OBJECT(2)=0
        OBJECT(3)=0
        PS=COMPL
*
*   INITIALISE PARAMETERS
*  -----------------------
*
        IF (NEWMAR.NE.S) THEN
*
*         get new data storage area
*         put its address on top of the object stack
*         and initialize it
*
          CALL POP('FREE',EMPTY)
          OBJECT(3)=EMPTY
          STORE(XMAX,EMPTY)=0.0
          STORE(XMIN,EMPTY)=OVER
          STORE(INTMAX,EMPTY)=0.0
          STORE(AREA,EMPTY)=0.0
          STORE(YEXT,EMPTY)=0.0
          STORE(SUMY,EMPTY)=0.0
          STORE(SUMX,EMPTY)=0.0
          STORE(SUMI,EMPTY)=0.0
          STORE(SIX,EMPTY)=0.0
          STORE(SIY,EMPTY)=0.0
          STORE(SXX,EMPTY)=0.0
          STORE(SXY,EMPTY)=0.0
          STORE(SYY,EMPTY)=0.0
          STORE(SIXX,EMPTY)=0.0
          STORE(SIXY,EMPTY)=0.0
          STORE(SIYY,EMPTY)=0.0
        ENDIF
        CALL PUSH('OBSTAK',OBJECT)
      ENDIF

      END
