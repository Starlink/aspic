      SUBROUTINE CIRCUR2(IXCUR,IYCUR,ISIZE,ID)
*+
*   CIRCUR2
*
*   Identical to CIRCUR except this one also returns ID - the image id.
*   in args data base as a parameter.
*   Puts a circular cursor at (IXCUR,IYCUR) of diameter ISIZE pixels,
*   and allows the user to specify new values of all three by
*   using the trackerball.
*   Uses the args data base - so ixycur + isize in pixel not args coords
*
*   Given         (arguments)
*   IXCUR     I    X-position of cursor in pixel coords
*   IYCUR     I    Y-position of cursor in pixel coords
*   ISIZE     I    diameter of cursor in pixel coords
*
*   Returned      (arguments)
*   IXCUR     I    new X-position of cursor in pixel coords
*   IYCUR     I    new Y-position of cursor in pixel coords
*   ISIZE     I    new size of cursor in pixel coords
*   ID        I    id. of image in args data base
*
*   Subroutines called :
*   ARGS_PICWRT,BLABELS             : E2DLIB
*   ARGS_CSICUR        : ROEARGS
*   SRINIT,ARGS-NUMIM,ARGS_PTOA,ARGS_ATOP,IDENIM,WRUSER
*
*   B.D.Kelly/ROE/19.11.1981
*   D. Tudhope/ROE/August/1982
*-

      INTEGER IXCUR,IYCUR,ISIZE,ID
      INTEGER RET,RDATA(3,1),ITYPE,INUM
      CHARACTER*1 CURCOL,INDCOL
      INTEGER IXA,IYA,ISA,I
      INTEGER ISTATUS

      CALL BLABELS(9,'W','HIT','INC SIZE','DEC SIZE',' ')
      CURCOL='W'
      INDCOL='W'
      NSIZE=ISIZE/2
      ITYPE=1
      INUM=1

      ISTATUS=0
*  initialise args data base
      CALL SRINIT(0,.FALSE.,ISTATUS)
*  to convert initial cursor position and size from pixel coords to args coords,
*   assume initial coords refer to latest image drawn on args.
*  so get id of lastest image.
      CALL ARGS_NUMIM(ID)
*  convert pixel to args coords
      CALL ARGS_PTOA(ID,IXCUR,IYCUR,IXA,IYA,ISTATUS)
*  do same for size, except have to pretend it's a point
      CALL ARGS_PTOA(ID,IXCUR+NSIZE,IYCUR,ISA,I,ISTATUS)
*  call cursor routine in args coords
      CALL ARGS_CSICUR(ITYPE,INUM,ISA-IXA,IXA,IYA,CURCOL,INDCOL,
     :                 RET,RDATA)

      ISA=RDATA(1,1)*2
      IXA=RDATA(2,1)
      IYA=RDATA(3,1)
*  now have returned args (ixa,iya) in args coords. to convert to pixel coords,
*   have to find which image they lie on (may not be latest one plotted).
      CALL IDENIM(IXA,IYA,ID)
*  convert to pixel coords
      CALL ARGS_ATOP(ID,IXA,IYA,IXCUR,IYCUR,ISTATUS)
*  do same for size isa
      CALL ARGS_ATOP(ID,IXA+ISA,IYA,ISIZE,I,ISTATUS)
      ISIZE=ISIZE-IXCUR
      IF (ISTATUS.NE.0) THEN
        CALL WRUSER('ERROR IN ACCESSING ARGS DATA BASE',ISTATUS)
      ENDIF
*
*   Re-enable ARGS picture planes
*
      CALL ARGS_OVCLR(9)
      CALL ARGS_PICWRT

      END
