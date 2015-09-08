C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  IJOIN  *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C              IJOIN
C
C
C          FUNCTION:-
C               IJOIN  is designed to merge  several  non-congruent  images
C               together  into  one,  usually  larger,  output image. Up to
C                25   input images may  be  merged.  Associated  with  each
C               (apart  from  the  first  in  the  case of offsets) are two
C               offsets in X & Y giving the relative offset  of  the  input
C               image  origin  from  that  of the first entered image
C                   If a later image overlaps an earlier one, it
C                overwrites it, that is the earlier image overwritten
C                values disappear
C
C
C          USE:-
C               It can be used to  mosaic  images  together
C               It is for use with I*2 images
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The input image name
C
C         OFFSETS                             The X  &  Y  offsets  of  the
C                                             origin  of  IMAGE relative to
C                                             the first image
C
C         OUTPUT                              The name of the output file
C
C         TITLE                               The Title to be added on to
C                                             the output file.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         C D Pike                 RGO                             3-FEB-82
C         A J Penny                RGO                             7-OCT-82
C
C
C
C
C
C--------------------------------------------------------------------------



*
*  CDP/RGO 3/2/81 and AJP RGO 7-10-82
*
*
      INTEGER INAXES(2),OAXES(2),PIN(25),POUT,STATUS
      INTEGER LOWX,LOWY,HIGHX,HIGHY,OSIZEX,OSIZEY,IXSIZE(25),IYSIZE(25)
      REAL DDX(25),DDY(25),DX(25),DY(25),VALS(2)
      REAL BSCALE(25),BZERO(25)
      INTEGER INVAL(25)
      CHARACTER*72 TEXT

      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

*
*  READ IN IMAGES UNTIL NULL RESPONSE
*
      NIMG = 0
   10 NIMG = NIMG + 1

      IF(NIMG.GT.25)  THEN
         CALL WRUSER(' MAX NO OF INPUT IMAGES REACHED',STATUS)
         NIMG = 25
         GO TO 25
      ENDIF


   12 CONTINUE
      CALL GTIMAG(PIN(NIMG),INAXES,BSCALE(NIMG),BZERO(NIMG),
     +            INVAL(NIMG),IERR)
      IF(IERR.EQ.1)  THEN
         CALL WRUSER('BUM IMAGE',STATUS)
         GO TO 12
      ENDIF

      IF(IERR.EQ.0)  THEN
         IXSIZE(NIMG) = INAXES(1)
         IYSIZE(NIMG) = INAXES(2)

*
*        GET OFFSETS
*
         IF(NIMG.EQ.1)  THEN
            DX(1) = 0.0
            DY(1) = 0.0
         ELSE
            VALS(1) = 0.0
            VALS(2) = 0.0
   15       CALL RDKEYR('OFFSETS',.TRUE.,2,VALS,NVAL,STATUS)
            CALL CNPAR('OFFSETS',STATUS)
            IF(STATUS.GT.ERR_PARNUL)  GOTO 15
            DX(NIMG) = VALS(1)
            DY(NIMG) = VALS(2)
         ENDIF
         GO TO 10


      ELSE
         NIMG = NIMG - 1
         IF (NIMG.EQ.0) GOTO 999

      ENDIF

   25 CONTINUE

*
*  SORT OUT THE OFFSETS AND RELATE TO THE EXTREME IMAGES OF THE BUNCH
*

      MINDX = 1
      MINDY = 1

      DXMIN = 0.0
      DYMIN = 0.0
      HIGHX = 0.0
      HIGHY = 0.0

*
*  SEARCH THRO OFFSETS FOR MIN IN X & Y DIRECTIONS
*  MINDX ETC WILL THEN CONTAIN THE NUMBER OF THE IMAGE WHICH DEFINES
*  THE EXTREMUM.
*  ALSO SEARCH FOR MAXIMUM EXTENT OF OUTPUT IMAGE IN X AND Y
*  THIS WILL BE MAX(DX(I)+ISIZEX(I))  ETC.
*

      DO I=1,NIMG

      IF(DX(I).LT.DXMIN)  THEN
         DXMIN = DX(I)
         MINDX = I
      ENDIF

      IF(DY(I).LT.DYMIN)  THEN
         DYMIN = DY(I)
         MINDY = I
      ENDIF

      IF((DX(I)+IXSIZE(I)).GT.HIGHX)  THEN
          HIGHX = DX(I) + IXSIZE(I)
      ENDIF

      IF((DY(I)+IYSIZE(I)).GT.HIGHY)  THEN
          HIGHY = DY(I) + IYSIZE(I)
      ENDIF


      ENDDO

*
*  FIND THE SIZE OF THE OVERALL IMAGE
*
      LOWX = DXMIN
      LOWY = DYMIN

      OSIZEX = (HIGHX-LOWX)
      OSIZEY = (HIGHY-LOWY)

*
*  MAKE THE OFFSETS RELATIVE TO THE EXTREME LOWER IMAGE IN EACH
*  DIRECTION
*
      DO I=1,NIMG
      DDX(I) = DX(I) + ABS(DX(MINDX))
      DDY(I) = DY(I) + ABS(DY(MINDY))
      ENDDO

      DO I=1,NIMG
      DX(I) = DDX(I)
      DY(I) = DDY(I)
      ENDDO

*
*  CREATE OUTPUT IMAGE
*
      OAXES(1) = OSIZEX
      OAXES(2) = OSIZEY

      ITRY = 1
   30 CONTINUE
      CALL PTIMAG(OAXES,BSCALE(1),BZERO(1),INVAL(1),POUT,STATUS)
      IF(STATUS.NE.0)  THEN
         IF(ITRY.EQ.2) GO TO 999
         ITRY = 2
         CALL WRUSER(' ONE MORE TRY ',STATUS)
         GO TO 30
      ENDIF


*
*  DO THE MERGING OF THE FRAMES
*
      DO I=1,NIMG

      INAXES(1) = IXSIZE(I)
      INAXES(2) = IYSIZE(I)
      CALL JOIN (%VAL(POUT),OAXES,%VAL(PIN(I)),INAXES,
     *           DX,DY,I,BSCALE(I),BZERO(I),BSCALE(1),BZERO(1))
      WRITE(TEXT,900) I
  900 FORMAT(' ','INSERTED IMAGE ',I3)
      CALL WRUSER(TEXT,ISTAT)
      ENDDO

  999 CALL FRDATA(' ',STATUS)
      CALL EXIT
      END




      SUBROUTINE JOIN (OUT,OAXES,MAP,INAXES,DX,DY,NMAP,
     +                 BSCALE,BZERO,BS,BZ)

      INTEGER OAXES(2),INAXES(2)
      REAL    DX(25),DY(25)
      INTEGER*2 OUT(OAXES(1),OAXES(2)),MAP(INAXES(1),INAXES(2))


*
*   SET SCALE AND ZERO RELATIVE TO FIRST ARRAY
*
      SCALE = BSCALE/BS
      ZERO = (BZERO-BZ)/BS

*
*   ZERO OUT OUTPUT ARRAY
*
      IF(NMAP.EQ.1)  THEN
         DO J=1,OAXES(2)
            DO I=1,OAXES(1)
               OUT(I,J) = 0
            ENDDO
         ENDDO
      ENDIF

*
*  LOOP AROUND THE PIXELS OF THIS IMAGE
*
      DO J=1,INAXES(2)
         DO I=1,INAXES(1)

            IDX = I + DX(NMAP)
            JDY = J + DY(NMAP)
            OUT(IDX,JDY) = SCALE*REAL(MAP(I,J)) + ZERO
         ENDDO
      ENDDO

      END






C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PTIMAG *
C      *            *
C      **************
C
C
C --------------------------------------------------------------
C
C
C
C
      SUBROUTINE PTIMAG(NSIZE,SCALE,ZERO,INVAL,IPIM,IERR)
C
C
C
      INTEGER NSIZE(2)
      CHARACTER*30 TITLE
C
C  Get Output image
C
      CALL GT2DIW('OUTPUT',102,.FALSE.,NSIZE(1),NSIZE(2),IPIM,IERR)
      IF(IERR.EQ.0) THEN
C
C  Get Title
C
         TITLE = 'Output from IJOIN'
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL CNPAR('TITLE',ISTAT)
C
C  Load Title and descriptors
C
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +              IERRA)
         CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVAL,RVAL,CVAL,IERRA)
         CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,SCALE,CVAL,IERRA)
         CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,ZERO,CVAL,IERRA)
C
C
C
      ENDIF
      CALL CNPAR('OUTPUT',ISTAT)



      END



