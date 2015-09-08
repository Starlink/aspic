      PROGRAM AZOOM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   AZOOM *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               AZOOM [ = = = = ]
C
C
C          FUNCTION:-
C               It zooms the ARGS by user defined factors about a specified
C               center.
C
C
C          USE:-
C               It is most useful as a standard way of blowing up an  image
C               to the full size of the ARGS screen.
C
C
C         USER PARAMETERS:-
C
C         XC              255                 This is the x co-ordinate  of
C                                             the  point  about  which  the
C                                             ARGS  is  to  be  zoomed.  If
C                                             there  is  an  image  in  the
C                                             database then the  center  of
C                                             that  image  overides  255 as
C                                             the default value.
C
C         YC              255                 This is the y co-ordinate  of
C                                             the  point  about  which  the
C                                             ARGS  is  to  be  zoomed.  If
C                                             there  is  an  image  in  the
C                                             database then the  center  of
C                                             that  image  overides  255 as
C                                             the default value.
C
C         FX              1                   This is the zoom factor in X,
C                                             and must be in the range 1 to
C                                             16. If there is an  image  in
C                                             the  database then a suitable
C                                             factor is  computed  for  the
C                                             latest  image  displayed, and
C                                             this  becomes   the   default
C                                             value.
C
C         FY              1                   This is the zoom factor in Y.
C                                             In  this  case  it must be an
C                                             even number in the range 1 to
C                                             16.   A   default   value  is
C                                             computed as for the x factor.
C
C
C                                             However, because  it must  be
C                                             even, a  non-square image MAY
C                                             give unequal factors in x and
C                                             y.
C
C
C         P T Wallace-W F lupton   RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------




*  ORIG PROGRAM PTW MOD 28/7/81 WFL TO ACCESS ARGS DATABASE
*
      INCLUDE 'INTERIM(FMTPAR)'
      CHARACTER VALUE*80


*  TRY AND READ CENTRE POS OF MOST RECENT IMAGE
      CALL ARGS_RDIM(IX,IY,ISX,ISY,NVALS,I,JSTAT)
      IF (JSTAT.EQ.0) THEN
         IFX = MIN(16,MAX(1,512/ISX))
         IFY = MIN(16,MAX(1,2*(256/ISY)))
      ELSE
         IX = 255
         IY = 255
         IFX = 1
         IFY = 1
      ENDIF

*  PICK UP AND VALIDATE PARAMETERS
      CALL RDKEYI('XC',.TRUE.,1,IX,NVALS,JSTAT)
      IF (IX.LT.0 .OR. IX.GT.511) GO TO 9000
      CALL RDKEYI('YC',.TRUE.,1,IY,NVALS,JSTAT)
      IF (IY.LT.0 .OR. IY.GT.511) GO TO 9010
      CALL RDKEYI('FX',.TRUE.,1,IFX,NVALS,JSTAT)
      IF (IFX.LT.1 .OR. IFX.GT.16) GO TO 9020
      CALL RDKEYI('FY',.TRUE.,1,IFY,NVALS,JSTAT)
      IF (IFY.LT.1 .OR. IFX.GT.16) GO TO 9030
      IF (IFY.GT.1 .AND. MOD(IFY,2).NE.0) GO TO 9040

*  PREPARE FOR ARGS OUTPUT
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) GO TO 9050

*  DO THE ZOOM
      CALL IZOOM(IX,IY,IFX,IFY)
      CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
      CALL ASP_ITODZ('ZXC',IX,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYC',IY,VALUE,JSTAT)
      CALL ASP_ITODZ('ZXF',IFX,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYF',IFY,VALUE,JSTAT)
      CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
      GO TO 9900

*  ERRORS
 9000 CONTINUE
      CALL WRUSER('X MUST BE 0-511 !',JSTAT)
      GO TO 9900

 9010 CONTINUE
      CALL WRUSER('Y MUST BE 0-511 !',JSTAT)
      GO TO 9900

 9020 CONTINUE
      CALL WRUSER('FX MUST BE 1-16 !',JSTAT)
      GO TO 9900

 9030 CONTINUE
      CALL WRUSER('FY MUST BE 1-16 !',JSTAT)
      GO TO 9900

 9040 CONTINUE
      CALL WRUSER('FY MUST BE 1,2,4,6,8....16 !',JSTAT)
      GO TO 9900

 9050 CONTINUE
      CALL WRUSER('DISPLAY UNAVAILABLE!',JSTAT)

*  WRAP UP
 9900 CONTINUE
      CALL FRDATA(' ',JSTAT)
      END

      SUBROUTINE IZOOM(IX,IY,IFX,IFY)
      INTEGER IX,IY,IFX,IFY



      CALL ARGS_FLUSH(4)
      CALL ARGS_PUT1('C000'X+IX)
      CALL ARGS_PUT1('A000'X+IY)
      CALL ARGS_PUT1('5001'X)
      CALL ARGS_PUT1(256*(IFY-1)+IFX-1)
      CALL SRSEND

      END
