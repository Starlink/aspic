C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   MERGE *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               MERGE
C
C
C          FUNCTION:-
C               MERGE is designed to  merge  several  non-congruent  images
C               together  into  one,  usually  larger,  output image. Up to
C               seven input images may  be  merged.  Associated  with  each
C               (apart  from  the  first  in  the  case of offsets) are two
C               offsets in X & Y giving the relative offset  of  the  input
C               image  origin  from  that  of the first entered image and a
C               weight (0-1) which is to be applied to the data  values  in
C               that image if another image overlaps.
C
C
C          USE:-
C               It can be used to  mosaic  images  together  or  to  simply
C               average several images. If appropriate weights are applied,
C               with  some  offset,  images   can   be   made   to   appear
C               semi-transparent.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input image name
C
C         OFFSETS                             The X  &  Y  offsets  of  the
C                                             origin  of  INPUT relative to
C                                             the first image
C
C         WEIGHT                              The weight to be  applied  to
C                                             the  data  in  this  image if
C                                             they   overlap   with   other
C                                             images
C
C         OUTPUT                              The name of the output file
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         C D Pike                 RGO                             3-FEB-82
C
C
C
C
C
C--------------------------------------------------------------------------



*
* Program MERGE is designed to merge non-congruent images into
* a single output image. Non overlapping data are stored in the
* output array undiluted whereas any regions of overlap are weighted
* according to weights specified at run time. In order to avoid having
* to create a large 3-d array with x-y dimensions of the output array
* and z dimension equal to the number of input images prior to merging
* the data (this wastes space and can cause paging problems) only a
* single output image of the minimum size required is used. Together
* with this a BYTE array of the same dimensions is used to act as a mask
* indicating which images, if any, are contributing to the output array.
* The individual bits in each output pixels byte mask are set if that
* input frame number has had data already stored in that output pixel.
* Thus when it comes to adding in image N then the mask at the
* appropriate pixel is examined and if no bits are set then the data
* is dumped directly but if any bits are set then the data already
* there is reconstituted (from a knowledge of the image weights) and
* the new data added and bit N set in the mask. In this way a maximum
* of 7 input images can be handled.

*  CDP/RGO 3/2/81
*
*
      INTEGER INAXES(2),OAXES(2),PIN(7),POUT,PMASK,STATUS
      INTEGER LOWX,LOWY,HIGHX,HIGHY,OSIZEX,OSIZEY,IXSIZE(7),IYSIZE(7)
      REAL DDX(7),DDY(7),DX(7),DY(7),WT(7),VALS(2),WEIGHT

      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

*
*  READ IN IMAGES UNTIL NULL RESPONSE
*
      NIMG = 0
   10 NIMG = NIMG + 1

      IF(NIMG.GT.7)  THEN
         CALL WRUSER(' MAX NO OF INPUT IMAGES REACHED',STATUS)
         NIMG = 7
         GO TO 25
      ENDIF


   12 CALL RDIMAG('INPUT',FMT_SW,2,INAXES,NDIM,PIN(NIMG),ISTATUS)
      IF(ISTATUS.GT.ERR_PARNUL)  THEN
         CALL WRUSER('BUM IMAGE',STATUS)
         CALL CNPAR('INPUT',STATUS)
         GO TO 12
      ENDIF

      IF(ISTATUS.EQ.ERR_NORMAL)  THEN
         IXSIZE(NIMG) = INAXES(1)
         IYSIZE(NIMG) = INAXES(2)
         CALL CNPAR('INPUT',STATUS)

*
*        SKIP OFFSETS FOR FIRST IMAGE
*
         IF(NIMG.EQ.1)  THEN
            DX(1) = 0.0
            DY(1) = 0.0
            GO TO 20
         ENDIF

*
*        GET OFFSETS FOR THIS IMAGE
*
         VALS(1) = 0.0
         VALS(2) = 0.0
   15    CALL RDKEYR('OFFSETS',.TRUE.,2,VALS,NVAL,STATUS)
         IF(STATUS.GT.ERR_PARNUL)  THEN
            CALL CNPAR('OFFSETS',STATUS)
            GO TO 15
         ENDIF

         DX(NIMG) = VALS(1)
         DY(NIMG) = VALS(2)

*
*        PICK UP WEIGHT OF THIS IMAGE
*
   20    WEIGHT = 1.0
         CALL RDKEYR('WEIGHT',.TRUE.,1,WEIGHT,NVAL,STATUS)
         IF(STATUS.GT.ERR_PARNUL)  THEN
            CALL CNPAR('WEIGHT',STATUS)
            GO TO 20
         ENDIF
         WT(NIMG) = WEIGHT
         CALL CNPAR('OFFSETS',STATUS)
         CALL CNPAR('WEIGHT',STATUS)
         GO TO 10

      ELSE

         NIMG = NIMG - 1
         IF(NIMG.EQ.0)  GO TO 999

      ENDIF

*
*  SORT OUT THE OFFSETS AND RELATE TO THE EXTREME IMAGES OF THE BUNCH
*
   25 CONTINUE

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
   30 CALL WRIMAG('OUTPUT',FMT_SW,OAXES,2,POUT,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         IF(ITRY.EQ.2) GO TO 999
         CALL CNPAR('OUTPUT',STATUS)
         ITRY = 2
         CALL WRUSER(' ONE MORE TRY ',STATUS)
         GO TO 30
      ENDIF

*
*  ALLOCATE BYTE ARRAY FOR MASK
*
      CALL GETDYN('MASK',FMT_SB,OAXES(1)*OAXES(2),PMASK,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         CALL WRUSER(' ERROR GETTING SPACE FOR MASK',STATUS)
         GO TO 999
      ENDIF

*
*  DO THE MERGING OF THE FRAMES
*
      DO I=1,NIMG

      INAXES(1) = IXSIZE(I)
      INAXES(2) = IYSIZE(I)
      CALL MERGE(%VAL(POUT),%VAL(PMASK),OAXES,%VAL(PIN(I)),INAXES,
     *           DX,DY,WT,I)
      ENDDO

  999 CALL FRDATA(' ',STATUS)
      CALL EXIT
      END




      SUBROUTINE MERGE(OUT,MASK,OAXES,MAP,INAXES,DX,DY,WT,NMAP)

      INTEGER OAXES(2),INAXES(2),BITS(7)
      REAL    DX(7),DY(7),WT(7)
      INTEGER*2 OUT(OAXES(1),OAXES(2)),MAP(INAXES(1),INAXES(2))
      INTEGER TEMPVALUE
      BYTE MASK(OAXES(1),OAXES(2)),TEMPMASK
      LOGICAL SET


*
*   ZERO OUT OUTPUT ARRAY
*
      IF(NMAP.EQ.1)  THEN
         DO J=1,OAXES(2)
         DO I=1,OAXES(1)
         OUT(I,J) = 0
         MASK(I,J) = 0
         ENDDO
         ENDDO
      ENDIF

*
*  LOOP AROUND THE PIXELS OF THIS IMAGE. CHECK WHETHER MASK AT THAT
*  POINT HAS AN ENTRY. IF NOT JUST ADD DATA IN IF IT HAS THEN
*  ADJUST VALUE ACCORDINGLY
*
      DO 100 J=1,INAXES(2)
      DO 200 I=1,INAXES(1)

*
* SKIP IF ZERO DATA
*
      IF(MAP(I,J).EQ.0)  GO TO 200

      IDX = I + DX(NMAP)
      JDY = J + DY(NMAP)

      SET = .FALSE.
      TEMPMASK = MASK(IDX,JDY)
      CALL CHECK_MASK(TEMPMASK,BITS,SET)

      IF(.NOT.SET)  THEN

         OUT(IDX,JDY) = MAP(I,J)

      ELSE

         TEMPVALUE = OUT(IDX,JDY)
         TOTWT = 0.0
         DO IB=1,NMAP-1
         TOTWT = TOTWT + WT(IB)*BITS(IB)
         ENDDO

         TEMPVALUE = TEMPVALUE * TOTWT
         TEMPVALUE = TEMPVALUE + MAP(I,J)*WT(NMAP)
         TEMPVALUE = TEMPVALUE/(TOTWT+WT(NMAP))
         OUT(IDX,JDY) = MIN0(TEMPVALUE,32767)

      ENDIF
      CALL SET_MASK(MASK(IDX,JDY),NMAP)


  200 CONTINUE
  100 CONTINUE

      END


      SUBROUTINE CHECK_MASK(MASK,BITS,SET)

      INTEGER BITS(7)
      INTEGER*2 IMASK
      BYTE MASK
      LOGICAL SET

      IMASK = MASK
*
*  CHECK WHICH BITS OF THE MASK BYTE ARE SET
*
      DO I=1,7
      BITS(I) = 0

      IF(IMASK) THEN
         BITS(I) = 1
         SET = .TRUE.
      ENDIF

      IMASK = IISHFT(IMASK,-1)

      ENDDO
      END




*
*  SET BIT N IN THE BYTE VALUE MASK
*

      SUBROUTINE SET_MASK(MASK,N)
      INTEGER*2 I1,I2,I3,NM1
      BYTE MASK

      I1 = MASK
      I2 = 1
      NM1 = N-1
      I2 = IISHFT(I2,NM1)
      I3 = IIOR(I1,I2)

      MASK = I3

      END

