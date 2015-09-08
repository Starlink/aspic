C++
C
C      STARLINK ENVIRONMENT PROGRAM   *** PIXDUPE ***
C
C      WRITTEN BY K F HARTLEY AT RGO ON 9/11/80
C
C      VERSION #2
C
C      GIVEN A 2-D STARLINK IMAGE IT EXPANDS PART OF IT BY A
C      GIVEN FACTOR UP TO AN ARRAY OF THE SAME SIZE AS THE ORIGINAL.
C      (DEFAULT) OR OF DIFFERANT SIZE ( IN VERSION 2 )!
C
C      THIS VERSION USES PIXEL DUPLICATION - SEE EXPAND FOR BETTER
C      INTERPOLATION!
C
C
C      PARAMETERS ARE:
C
C            INPUT:   THE INPUT IMAGE
C            SIZEOFOU:   THE DIMENSIONS OF THE OUTPUT IMAGE
C            OUTPUT:     THE OUTPUT IMAGE
C            XYOFCEN:    THE PIXEL IN THE INPUT IMAGE WHICH WILL
C                        BECOME THE CENTRE OF THE OUTPUT IMAGE
C            MAGN:       THE FACTOR BY WHICH THE PIXELS CENTRED ON
C                        XYOFCEN WILL BE DUPLICATED TO FILL THE OUTPUT
C                        ARRAY. IT MUST BE A POSITIVE INTEGER.
C
C
C--
      INTEGER*4 AXISO(2),AXIS(2),STATUS,XC,YC,CEN(2),PIN,POUT
      INTEGER*4 FACTOR
      INCLUDE 'INTERIM(FMTPAR)'
C
C      GET THE TWO ARRAYS OLD AND NEW
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIS,I,PIN,STATUS)
      IF (STATUS.NE.0.OR.I.NE.2) THEN
         CALL WRERR('HELLIN')
         CALL EXIT
      END IF
C
C      NOW PICK UP SIZE OF OUTPUT IMAGE
C
      AXISO(1)=AXIS(1)
      AXISO(2)=AXIS(2)
      CALL WRUSER('ENTER DIMENSIONS OF OUTPUT FRAME',STATUS)
   50 CALL RDKEYI('SIZEOFOU',.TRUE.,2,AXISO,I,STATUS)
      IF (STATUS.GT.2.OR.I.EQ.1.OR.I.GT.2.OR.AXISO(1).LT.2.
     1    OR.AXISO(2).LT.2) THEN
          CALL WRERR('HELL')
          CALL CNPAR('SIZEOFOU',STATUS)
          IF (STATUS.NE.0) THEN
             CALL EXIT
          ELSE
             GO TO 50
          END IF
       END IF
      CALL WRIMAG('OUTPUT',FMT_R,AXISO,2,POUT,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRERR('HELLOUT')
         CALL EXIT
      END IF
C
C      NOW PICK UP THE THREE PARAMETERS
C
      CEN(1)=AXIS(1)/2+1
      CEN(2)=AXIS(2)/2+1
C
C      SET DEFAULT CENTRE OF NEW IMAGE TO CENTRE OF OLD IMAGE
C
      CALL WRUSER('ENTER CO-ORDS OF POINT TO BE CENTRE OF NEW IMAGE',
     1               STATUS)
  100 CALL RDKEYI('XYOFCEN',.TRUE.,2,CEN,I,STATUS)
      IF ((STATUS.GT.1).OR.(I.EQ.1.OR.I.GT.2).
     1    OR.(CEN(1).LT.1.OR.CEN(1).GT.AXIS(1)).OR.
     2       (CEN(2).LT.1.OR.CEN(2).GT.AXIS(2)))
     3              THEN
                       CALL WRERR('HELL')
                       CALL CNPAR('XYOFCEN',STATUS)
                       IF (STATUS.NE.0) THEN
                          CALL EXIT
                       ELSE
                          GO TO 100
                       END IF
                    END IF
C
C      SET DEFAULT FACTOR AS 1.0
C
      FACTOR=1.0
      CALL WRUSER ('ENTER DUPLICATION FACTOR :- A +VE INTEGER',
     1             STATUS)
      CALL WRUSER('SAME FACTOR WILL BE USED IN X AND Y',STATUS)
  200 CALL RDKEYI('MAGN',.TRUE.,1,FACTOR,I,STATUS)
      IF (STATUS.GT.1.OR.FACTOR.LT.1.0) THEN
         CALL WRERR('HELL')
         CALL CNPAR('MAGN',STATUS)
         IF (STATUS.NE.0) THEN
            CALL EXIT
         ELSE
            GO TO 200
         END IF
      END IF
C
C      NOW CALL ALGORITHM
C
      CALL PIXDUPE(%VAL(PIN),%VAL(POUT),AXIS(1),AXIS(2),AXISO(1),
     1            AXISO(2),FACTOR,CEN(1),CEN(2))
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE PIXDUPE(IN,OUT,N,M,JN,JM,FACTOR,XC,YC)
C
C      PARAMETERS :-
C         IN IS THE ORIGINAL IMAGE , OUT THE NEW IMAGE
C         IN IS OF SIZE N BY M AND OF TYPE REAL
C         OUT IS OF SIZE JN BY JM AND ALSO OF TYPE REAL
C         FACTOR IS THE INTEGER MAGNIFICATION FACTOR
C         XC,YC IS THE CO-ORDINATES OF THE POINT IN THE OLD IMAGE
C         WHICH WILL BECOME THE CENTRE OF THE NEW IOMAGE.
C
      REAL IN(N,M),OUT(JN,JM)
      INTEGER*4 FACTOR,XC,YC
      LOGICAL*1 INSIDE,INOUT
C
C      NEW ELEMENTS CAN COME FROM JL,IL PIXELS OF OLD ARRAY
C
      IL=JN/FACTOR
      JL=JM/FACTOR
C
C      AND HENCE FROM INPUT PIXELS FROM
C      XC-IL/2 TO XC+IL/2 , AND YC-JL/2 TO YC+JL/2
C
      DO 400 JIN=YC-JL/2,YC+JL/2
         JOUT1=(JIN-(YC-JL/2))*FACTOR+1
         JOUT2=JOUT1+FACTOR-1
         DO 300 J=JOUT1,JOUT2
            DO 200 IIN=XC-IL/2,XC+IL/2
               IOUT1=(IIN-(XC-IL/2))*FACTOR+1
               IOUT2=IOUT1+FACTOR-1
               DO 100 I=IOUT1,IOUT2
             INSIDE=IIN.GT.0.AND.IIN.LE.N.AND.JIN.GT.0.AND.JIN.LE.M
             INOUT=I.GT.0.AND.I.LE.JN.AND.J.GT.0.AND.J.LE.JM
                IF (INOUT) THEN
                   IF (INSIDE) THEN
                      OUT(I,J)=IN(IIN,JIN)
                   ELSE
                      OUT(I,J)=0
                   END IF
               END IF
  100          CONTINUE
  200       CONTINUE
            IF (MOD(J,20).EQ.0) THEN
               CALL WRUSER('ANOTHER 20 ROWS DONE',ISTAT)
            END IF
  300    CONTINUE
  400 CONTINUE
      RETURN
      END
