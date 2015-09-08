      SUBROUTINE COPYIT(IN,NI,MI,LIMX,LIMY,NO,MO,OUT)
C
C      This subroutine copies part of an array from in to out.
C
C      Parameteers :-
C
C         Input
C 		IN	Real 2D input array
C 		NI	X-dimension of IN
C 		MI	Y-dimension of IN
C 		LIMX	X limits of selected region.
C 		LIMY	Y limits of selected region.
C 		NO	X dimension of OUT
C 		MO	Y dimension of OUT
C
C         Output
C 		OUT	Real 2D array to hold the output.
C
C      written by K F Hartley at RGO on 30/11/81
C
      REAL IN(0:NI-1,0:MI-1) , OUT(0:NO-1,0:MO-1)
      INTEGER LIMX(2),LIMY(2)
 
C
C   Do the copy.
C
      J1=0
      DO J=LIMY(1),LIMY(2)
         I1=0
         DO I=LIMX(1),LIMX(2)
            OUT(I1,J1)=IN(I,J)
            I1=I1+1
         END DO
         J1=J1+1
      END DO
 
      END
