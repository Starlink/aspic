      SUBROUTINE XCUR(X,Y,ABORT)
*+
*    XCUR
*
*    displays ARGS system cursor and returns X,Y position when
*    user has hit the leftmost illuminated button. returns ABORT = .TRUE.
*    when user has hit the rightmost illuminated button.
*    uses args data base - so (x,y) in pixel coords (except real),
*    not args coords
*
*    Arguments :
*
*    Given     Type  Usage
*     X         R    X start position of cursor in pixel coords
*     Y         R    Y start position of cursur in pixel coords
*   
*    Returned  Type  Usage
*     X         R    X position of cursor in pixel coords
*     Y         R    Y position of cursor in pixel coords
*     ABORT     L    set to .TRUE. if abort button hit
*
*    J.A.Cooke/UOE/18Nov81
*    D. Tudhope/ROe/July/1982
*    D.W.T.Baines/ROE/April 1983
*-
      REAL X,Y
      LOGICAL ABORT
      INTEGER IX,IY,IB1,IB2,IB3,IB4,IST,ISTAT
      INTEGER IXA,IYA,ID,ISTATUS
*
*    initialise status variables etc.
*
      IST=0
      ISTAT=0
      ISTATUS=0
      ABORT = .FALSE.
      IB1=0
      IB2=0
      IB3=0
      IB4=0
*
*   initialise ARGS without reseting it
*
      CALL SRINIT(0,.FALSE.,IST)
      IF (IST.EQ.0) THEN
*
*       no error so set up the information boxes
*
         CALL BLABELS(9,'W','MEASURE',' ',' ','ABORT')
*
*       define integer cursor position
*
         IX = NINT( X )
         IY = NINT( Y )
*
*       to convert initial cursor position from pixel coords to args coords,
*       assume initial coords refer to latest image drawn on args.
*       so get id of lastest image.
*
         CALL ARGS_NUMIM(ID)
*
*       convert pixel to args coords
*
         CALL ARGS_PTOA(ID,IX,IY,IXA,IYA,ISTATUS)
*
*       set up the ARGS cursor and trackerball lights
*
         CALL ARGS_CURS('+')
         CALL ARGS_CURP(0,IXA,IYA)
         CALL ARGS_CURC('W')
         CALL ARGS_TBCL(0)
         CALL ARGS_LAMPS(1,0,0,1)
*
*       want to keep cursor displayed if outermost buttons not hit
*
         DO WHILE ((IB1 .NE. 1) .AND. (IB4 .NE. 1))
            CALL ARGS_TBCX(IXA,IYA,IB1,IB2,IB3,IB4)
         ENDDO
*
*      an acceptable button hit so switch off lamps and disable cursor
*
         CALL ARGS_LAMPS(0,0,0,0)
         CALL ARGS_CURS('0')
         IF(IB4 .EQ. 1) THEN
*
*          rightmost button hit so user wants to abort
*
            ABORT = .TRUE.
         ELSE
*
*          leftmost button hit , user wants to take a reading
*          now have returned args (ixa,iya) in args coords. to convert to 
*          pixel coords have to find which image they lie on (may not be 
*          latest one plotted).
*
            CALL IDENIM(IXA,IYA,ID)
*
*          convert to pixel coords
*
            CALL ARGS_ATOP(ID,IXA,IYA,IX,IY,ISTATUS)
            IF (ISTATUS.NE.0) THEN
*
*             here if error accessing the ARGS data base
*
               CALL WRUSER('Error in accessing ARGS data base',ISTAT)
            ENDIF
*
*         convert to 'real' cursor position
*
            X = REAL( IX )
            Y = REAL( IY )
         ENDIF
*
*       clear overlay plane containing information boxes
*
         CALL ARGS_OVCLR( 9 )
         CALL SRSEND
      ELSE
*
*       here on error allocating the ARGS
*
         CALL WRUSER('Error allocating the ARGS',ISTAT)
      ENDIF
      END
