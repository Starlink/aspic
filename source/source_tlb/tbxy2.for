      PROGRAM TBXY2
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   TBXY2 *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               TBXY2
C
C
C          FUNCTION:-
C               This is a more useful version of TBXY, where two points are
C               selected instead of the one in TBXY.
C
C
C          USE:-
C               It may be used, for example to find the co-ordinates  of  a
C               rectangular patch in an image.
C
C
C         USER PARAMETERS:-
C               The parameter names are the same as in TBXY (qv).
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Used to select two points, both of which must  be  in
C                     the  same  image  which  is  visible on the ARGS. The
C                     cursor changes colour after the first point has  been
C                     accepted.
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




*   Parameters 'ID', 'Xn' and 'Yn' are written to the environment. 'ID' is the
*   identifier for the located object and is a positive integer. 'Xn' and 'Yn'
*   are in user units and are reals. Note that user co-ordinates are
*   normally the same as pixel co-ordinates (ie start at (0.0,0.0) in
*   the bottom left-hand corner of the image), but it is possible to
*   define a "pixel-to-user" transformation, and users should be aware
*   that this may have been done!
*   For the time being parameters 'AXn' and 'AYn' (in ARGS units) are also set
*   (to interface with ASPIC programs requiring ARGS units)
*   WFL RGO Sep 1981 - rendered more general 13 Nov 1981

      INTEGER IDMAX,STATUS,ID,IB,AX,AY,IDLOC
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

*           Read cursor until 2 valid points are chosen
              CALL ARGS_CUROP ('1234','W')
              ID = 0
              DO WHILE (ID.EQ.0)
                  CALL ARGS_RDCUR (' ',ID,IB,UX,UY)
              ENDDO
*           Send 'id' and '(ux,uy)' to the environment
*           (next 4 lines are temporary)
              CALL ARGS_DOPDB ('ARGS_DEVICE',STATUS)
              CALL ARGS_UTOA (ID,UX,UY,AX,AY,STATUS)
              CALL WRKEYI ('AX1',AX,1,STATUS)
              CALL WRKEYI ('AY1',AY,1,STATUS)
              CALL WRKEYI ('ID',ID,1,STATUS)
              CALL WRKEYR ('X1',UX,1,STATUS)
              CALL WRKEYR ('Y1',UY,1,STATUS)
              CALL ARGS_CURC ('G')
              IDLOC = ID
              ID = 0
              DO WHILE (ID.NE.IDLOC)
                  CALL ARGS_RDCUR (' ',ID,IB,UX,UY)
              ENDDO
*           Send 'id' and '(ux,uy)' to the environment
*           (next 4 lines are temporary)
              CALL ARGS_UTOA (ID,UX,UY,AX,AY,STATUS)
              CALL ARGS_CLDB (STATUS)
              CALL WRKEYI ('AX2',AX,1,STATUS)
              CALL WRKEYI ('AY2',AY,1,STATUS)
              CALL WRKEYR ('X2',UX,1,STATUS)
              CALL WRKEYR ('Y2',UY,1,STATUS)
              CALL ARGS_CURCL

          ENDIF

      ENDIF

      END
