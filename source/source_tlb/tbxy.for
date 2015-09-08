      PROGRAM TBXY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ******************
C                     *                *
C                     * Program   TBXY *
C                     *                *
C                     ******************
C
C
C
C          CALLING SEQUENCE:-
C               TBXY
C
C
C          FUNCTION:-
C               It uses the ARGS cursor to pick a single point  and  return
C               to  the  environment an identification number for the image
C               selected and the co-ordinates of the point selected in user
C               units  (which  are not necessarily the same as array units,
C               but usually will be.)
C
C
C          USE:-
C               It is really a test program, not for serious use.
C
C
C
C         USER PARAMETERS:-
C
C         AX                                  The returned x co-ordinate of
C                                             the  point  selected  in ARGS
C                                             units.
C
C         AY                                  The returned y co-ordinate of
C                                             the  point  selected  in ARGS
C                                             units.
C
C         ID                                  The identification number  of
C                                             the   image   (or   whatever)
C                                             selected, as  stored  in  the
C                                             ARGS database.
C
C         X                                   The returned x co-ordinate of
C                                             the  selected  point  in user
C                                             units (usually  array  units,
C                                             but need not be).
C
C         Y                                   The returned y co-ordinate of
C                                             the  selected  point  in user
C                                             units (usually  array  units,
C                                             but need not be).
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C
C
C
C         GREEN 1     PIck the point - must lie within one of  the  visible
C                     images.
C
C         WHITE 2     Same as 1
C
C         WHITE 3     Same as 1
C
C         RED   4     Same as 1
C
C
C
C
C
C
C         W F Lupton               RGO                            13-JAN-82
C
C
C--------------------------------------------------------------------------




*   Parameters 'ID', 'X' and 'Y' are written to the environment. 'ID' is the
*   identifier for the located object and is a positive integer. 'X' and 'Y'
*   are in user units and are reals. Note that user co-ordinates are
*   normally the same as pixel co-ordinates (ie start at (0.0,0.0) in
*   the bottom left-hand corner of the image), but it is possible to
*   define a "pixel-to-user" transformation, and users should be aware
*   that this may have been done!
*   For the time being parameters 'AX' and 'AY' (in ARGS units) are also set
*   (to interface with ASPIC program AZOOM in particular)
*
*   WFL RGO Sep 1981 - rendered more general 13 Nov 1981

      INTEGER IDMAX,STATUS,ID,IB,AX,AY
      REAL UX,UY

*   Check an object is displayed
      CALL ARGS_NUMIM (IDMAX)
      IF (IDMAX.EQ.0) THEN
          CALL WRERR ('NOIMS')
      ELSE

*       Assign and prepare ARGS
          CALL SRINIT (0,.FALSE.,STATUS)
          IF (STATUS.NE.0) THEN
              CALL WRERR('NOARGS')
          ELSE

*           Read cursor until valid point is chosen
              CALL ARGS_CUROP ('1234','W')
              ID = 0
              DO WHILE (ID.EQ.0)
                  CALL ARGS_RDCUR (' ',ID,IB,UX,UY)
              ENDDO
              CALL ARGS_CURCL

*           Send 'id' and '(ux,uy)' to the environment
*           (next 5 lines are temporary)
              CALL ARGS_DOPDB ('ARGS_DEVICE',STATUS)
              CALL ARGS_UTOA (ID,UX,UY,AX,AY,STATUS)
              CALL ARGS_CLDB (STATUS)
              CALL WRKEYI ('AX',AX,1,STATUS)
              CALL WRKEYI ('AY',AY,1,STATUS)
              CALL WRKEYI ('ID',ID,1,STATUS)
              CALL WRKEYR ('X',UX,1,STATUS)
              CALL WRKEYR ('Y',UY,1,STATUS)

          ENDIF

      ENDIF

      END
