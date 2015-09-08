      SUBROUTINE CLRQUADS (WORK,IXR,IYR)
C+
C  CLRQUADS
C
C	clears quadrant of args
C	uses work array 'WORK' which is filled with 0s and plotted to args
C
C  Given (arguments)
C    WORK (I*2A)    work array
C    IXY,IYR (I)      origin of quadrant
C
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER  IXR,IYR
      INTEGER*2 WORK(256,256)
      INTEGER I,J
C*  dummy array for low level plotting subroutine SRPXI2
      INTEGER*2 DUM(1)

      CALL SRINIT(0,.FALSE.,ISTATUS)
C*  fill WORK with 0s
      DO J=1,256
      DO I=1,256
        WORK(I,J)=0
      ENDDO
      ENDDO
C*  clear screen for quadrant
      CALL SRPXI2(WORK,256,256,256,IXR,IYR,16,.FALSE.,DUM,1)
      END
