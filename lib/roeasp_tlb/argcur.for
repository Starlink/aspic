      SUBROUTINE ARGCUR(X,Y)
C+
C   ARGCUR
C
C   Displays ARGS system cursor and returns X,Y position when
C   user has hit illuminated button.
C    Uses args data base - so (x,y) in pixel coords (except real), not args coords
C
C   Given  (arguments):
C     X         R    X start position of cursor in pixel coords
C     Y         R    Y ditto
C   
C   Returned (arguments):
C     X         R    X position of cursor in pixel coords
C     Y         R    Y ditto
C
C   J.A.Cooke/UOE/18Nov81
C    D. Tudhope/ROe/July/1982
C-
      INTEGER IX,IY,IB1,IB2,IB3,IB4,IST
      REAL X,Y
C  local vars in args coords
      INTEGER IXA,IYA
C  image id. and status return
      INTEGER ID,ISTATUS

C
C   initialise ARGS.....
      CALL SRINIT(0,.FALSE.,IST)

      IF (IST.EQ.0) THEN

         IX=X
         IY=Y

*  to convert initial cursor position from pixel coords to args coords,
*   assume initial coords refer to latest image drawn on args.
*  so get id of lastest image.
      CALL ARGS_NUMIM(ID)
*  convert pixel to args coords
      CALL ARGS_PTOA(ID,IX,IY,IXA,IYA,ISTATUS)

         CALL ARGS_CURS('+')

         CALL ARGS_CURP(0,IXA,IYA)

         CALL ARGS_CURC('W')

         CALL ARGS_TBCL(0)

         CALL ARGS_LAMPS(1,0,0,0)

         CALL ARGS_TBCX(IXA,IYA,IB1,IB2,IB3,IB4)

         CALL ARGS_LAMPS(0,0,0,0)

         CALL ARGS_CURS('0')

*  now have returned args (ixa,iya) in args coords. to convert to pixel coords,
*   have to find which image they lie on (may not be latest one plotted).
      CALL IDENIM(IXA,IYA,ID)
*  convert to pixel coords
      CALL ARGS_ATOP(ID,IXA,IYA,IX,IY,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRUSER('ERROR IN ACCESSING ARGS DATA BASE',ISTATUS)
      ENDIF

         CALL SRSEND

         X=IX
         Y=IY

      ELSE

         CALL WRERR('NOARGS')

      ENDIF

      END
