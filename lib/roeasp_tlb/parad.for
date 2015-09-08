      SUBROUTINE PARAD(MARKER,MXLEN,NOTSET,S0,STORE)
*+
*   PARAD
*
*     ADDS PARAMETERS OF PREVIOUSLY SEPARATED PARTS OF AN IMAGE
*     FOR THE COSMOS ANALYSER.
*
*   Given      (arguments)
*   MXLEN  I    X-dimension of image frame
*   NOTSET I    fixed value, indicating undefined start-address entry
*               in object stack
*   S0     I    fixed value, connectivity marker implying start of an
*               object segment
*   STORE  DA   store for accumulating object parameters
*
*   Given      (stacks)
*   OBSTAK      objects currently being traversed
*   FREE        indices to unused parts of data storage area
*
*   Returned   (arguments)
*   MARKER  IA  array of connectivity markers
*   STORE   DA  store for accumulating object parameters
*
*   Returned   (stacks)
*   OBSTAK      objects currently being traversed
*   FREE        indices to unused parts of data storage area
*
*   Subroutines called :
*   POP,PUSH,GETSTK,PUTSTK,UPDATE  : E2DLIB
*
*   B.D.Kelly/ROE/15.3.1982
*-
      INTEGER MXLEN
      INTEGER MARKER(MXLEN+1)
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2)
      INTEGER OBJA(3),OBJB(3)
      INTEGER NOTSET,S0,KK1,KK2
*
*
*   look-up top two entries in stack pointing to data
*   storage area.
*
      CALL POP('OBSTAK',OBJB)
      CALL GETSTK('OBSTAK',OBJA)
*
*   if not yet started the current image on this scan, then
*      set top of stack holding image starting position
*          to its previous value
*   else
*      put a start of segment marker at previous starting
*          position
*   endif
*
      IF (OBJA(1).EQ.NOTSET) THEN
         OBJA(1)=OBJB(1)
         CALL PUTSTK('OBSTAK',OBJA)
      ELSE
        MARKER(OBJB(1))=S0
      ENDIF
*
*   NOW ADD PARAMS FOR TWO IMAGES JOINING
*  ---------------------------------------
*
*   This condition occurs only if OBJB is enclosed by OBJA,
*   therefore Y-extent of OBJA is the greater.
*
      KK1=OBJA(3)
      KK2=OBJB(3)
      CALL UPDATE(KK1,KK1,KK2,MXLEN,STORE)
*
*   put top of data storage stack back on free stack
*
      CALL PUSH('FREE',KK2)

      END
