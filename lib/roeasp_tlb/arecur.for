      SUBROUTINE ARECUR(COLOUR,IXIN,IYIN,IXSIZE,IYSIZE,
     :                  IXOUT,IYOUT,IBUTT)
*+
*   ARECUR
*
*   Rectangular ARGS cursor in overlay plane 8
*   The cursor position is its bottom-left corner.
*   Uses args data base - so args in pixel coords, not args coords
*
*   Given       (arguments)
*   COLOUR   C   cursor colour
*   IXIN     I   initial cursor X-coordinate in pixel coords
*   IYIN     I   initial cursor Y-coordinate in pixel coords
*   IXSIZE   I   X-length of rectangle in pixel coords
*   IYSIZE   I   Y-length of rectangle in pixel coords
*
*   Returned    (arguments)
*   IXOUT    I   X-coordinate in pixel coords
*   IYOUT    I   Y-coordinate in pixel coords
*   IBUTT    I   no. of the trackerball button hit
*
*   Subroutines called :
*   ARGS_RECCUR        : ROEARGS
*   SRINIT,ARGS-NUMIM,ARGS_PTOA,ARGS_ATOP,IDENIM,WRUSER
*
*   B.D.Kelly/ROE/1.7.1982
*   D. Tudhope/ROE/July/1982
*-
      CHARACTER*(*) COLOUR
      INTEGER IXIN,IYIN,IXSIZE,IYSIZE,IXOUT,IYOUT,IBUTT
*  local variables in args coords
      INTEGER IXA,IYA,IXSA,IYSA,IXOA,IYOA
*  local vars - image id. and status return
      INTEGER ID,ISTATUS

      ISTATUS=0
*  initialise args data base
      CALL SRINIT(0,.FALSE.,ISTATUS)
*  to convert initial cursor position and size from pixel coords to args coords,
*   assume initial coords refer to latest image drawn on args.
*  so get id of lastest image.
      CALL ARGS_NUMIM(ID)
*  convert pixel to args coords
      CALL ARGS_PTOA(ID,IXIN,IYIN,IXA,IYA,ISTATUS)
*  do same for size, except have to pretend it's a point (ixin+ixsize,iyin+iysize)
      CALL ARGS_PTOA(ID,IXIN+IXSIZE,IYIN+IYSIZE,IXSA,IYSA,ISTATUS)
*  call cursor routine in args coords
      CALL ARGS_RECCUR(8,COLOUR,IXA,IYA,IXSA-IXA,IYSA-IYA,
     :                 IXOA,IYOA,IBUTT)
*  now have returned args (ixoa,iyoa) in args coords. to convert to pixel coords,
*   have to find which image they lie on (may not be latest one plotted).
      CALL IDENIM(IXOA,IYOA,ID)
*  convert to pixel coords
      CALL ARGS_ATOP(ID,IXOA,IYOA,IXOUT,IYOUT,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRUSER('ERROR IN ACCESSING ARGS DATA BASE',ISTATUS)
      ENDIF
      END
