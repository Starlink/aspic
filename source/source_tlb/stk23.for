C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   STK23 *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               STK23
C
C
C          FUNCTION:-
C               ALLOWS STACKING OF ANY NUMBER OF 2-D STARLINK IMAGES INTO A
C               3-D  IMAGE  WITH OFFSETS ALLOWED TO BE APPLIED IN THE (X,Y)
C               PLANE. THE (X,Y) DIMENSIONS OF THE OUTPUT FRAME WILL BE THE
C               SAME AS THOSE OF THE FIRST INPUT FRAME
C
C
C          USE:-
C               STORING RELATED DATA IN A SINGLE DATA CUBE WHICH  CAN  THEN
C               BE CONVENIENTLY DISPLAYED BY ADISP3
C
C
C
C         USER PARAMETERS:-
C
C         OFFSET                              LOGICAL SWITCH AS TO  WHETHER
C                                             OFFSETTING   OF   THE   INPUT
C                                             IMAGES IS REQUIRED
C
C         NUMBER                              THE TOTAL NUMBER OF INPUT
C                                             IMAGES TO BE STACKED. IT WILL
C                                             CORRESPOND TO T SIZE  OF  THE
C                                             3-RD  DIMENSION OF THE OUTPUT
C                                             FRAME
C
C         INPUT                               THE   NAME   OF   THE   INPUT
C                                             STARLINK FRAME
C
C         OUTPUT                              THE NAME OF THE OUTPUT FRAME
C                                             TO BE CREATED
C
C         XYOFF                               THE  (X,Y)  OFFSET   OF   THE
C                                             ORIGIN  OF  THE  INPUT  FRAME
C                                             RELATIVE TO THAT OF T
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         CDP                      RGO                            17-MAR-82
C
C
C
C
C--------------------------------------------------------------------------



      INTEGER OAXES(3),INAXES(2),PIN,POUT,STATUS,IXY(2)
      LOGICAL OFFSET
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

*
*  ASK WHETHER OFFSETS TO BE APPLIED IF FRAMES NOT SAME SIZE
*
      CALL WRUSER('Are the frames to be offset in (X,Y)?',STATUS)
      OFFSET = .FALSE.
      CALL RDKEYL('OFFSET',.TRUE.,1,OFFSET,NVAL,STATUS)
      IF(STATUS.GT.ERR_PARNUL)  OFFSET = .FALSE.

*
*  GET NUMBER OF FRAMES
*
   10 CALL WRUSER('How many frames are to be stacked?',STATUS)
      CALL RDKEYI('NUMBER',.FALSE.,1,NF,NVAL,STATUS)
      IF(STATUS.GT.ERR_NORMAL)  THEN
         CALL CNPAR('NUMBER',STATUS)
         GO TO 10
      ENDIF

*
*  PERFORM THE ACQUISITION LOOP
*
      DO I=1,NF
   20 CALL RDIMAG('INPUT',FMT_R,2,INAXES,NDIM,PIN,STATUS)
      IF(STATUS.GT.ERR_NORMAL)  THEN
         CALL CNPAR('INPUT',STATUS)
         CALL WRUSER('Bum input ... doesnt count.',STATUS)
         GO TO 20
      ENDIF

*
*  FIRST TIME ROUND CREATE OUTPUT
*
      IF(I.EQ.1)  THEN
         OAXES(1) = INAXES(1)
         OAXES(2) = INAXES(2)
         OAXES(3) = NF
   30    CALL WRIMAG('OUTPUT',FMT_R,OAXES,3,POUT,STATUS)
         IF(STATUS.NE.ERR_NORMAL)  THEN
            CALL CNPAR('OUTPUT',STATUS)
            CALL WRUSER('Bum output specification - try again',STATUS)
            GO TO 30
         ENDIF
      ENDIF

*
*  KEEP A CHECK ON SIZE OF AXES
*

      IF(INAXES(1).GT.OAXES(1).OR.INAXES(2).GT.OAXES(2)) THEN
         CALL WRUSER('Input (X,Y) dimensions are > those of',STATUS)
         CALL WRUSER('output frame - this input frame ignored',STATUS)
         CALL CNPAR('INPUT',STATUS)
         CALL FRDATA('INPUT',STATUS)
         GO TO 20
      ENDIF


*
*  IF OFFSETS ARE REQUIRED THEN PROMPT FOR THEM
*
      IF(OFFSET.AND.I.GT.1)  THEN
   40    CALL RDKEYI('XYOFF',.FALSE.,2,IXY,NVAL,STATUS)
         IF(STATUS.GT.ERR_PARNUL)  THEN
            CALL WRUSER('Bum offsets specified - try again',STATUS)
            CALL CNPAR('XYOFF',STATUS)
            GO TO 40
         ENDIF

         IX = IXY(1)
         IY = IXY(2)
         CALL CNPAR('XYOFF',STATUS)

      ELSE

         IX = 0
         IY = 0

      ENDIF


*
*  DO THE STACKING
*
      CALL STACK(%VAL(POUT),OAXES,%VAL(PIN),INAXES,IX,IY,I)

*
*  FREE INPUT FRAME
*
      CALL FRDATA('INPUT',STATUS)
      CALL CNPAR('INPUT',STATUS)

*
*  END MAIN (ONLY) DO LOOP
*
      ENDDO

*
*  TIDY UP AND EXIT
*
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END





      SUBROUTINE STACK(DAT3D,OAXES,DAT2D,INAXES,IX,IY,IFR)
      INTEGER OAXES(3),INAXES(2)
      REAL DAT3D(OAXES(1),OAXES(2),OAXES(3)),DAT2D(INAXES(1),INAXES(2))

*
*  CLEAR OUTPUT FRAME
*
      DO J=1,OAXES(2)
      DO I=1,OAXES(1)

      DAT3D(I,J,IFR) = 0.0

      ENDDO
      ENDDO

      DO J=1,MIN0(INAXES(2),OAXES(2)-IY)
      DO I=1,MIN0(INAXES(1),OAXES(1)-IX)

         DAT3D(I+IX,J+IY,IFR) = DAT2D(I,J)

      ENDDO
      ENDDO

      END


