      SUBROUTINE SWEEP1(CTRLC)
*+
*   SWEEP1
*
*   PERFORMS CONTINUOUS ROTATION OF COLOUR TABLE
*   STOPPED BY CONTROL-C
*
*   Given         (arguments)
*   CTRLC    L*2   = .FALSE. on entry, becomes .TRUE. if CONTROL-C
*                  typed
*
*   Returned      (arguments)
*   CTRLC    L*2   reset to .FALSE. on exit
*
*   Subroutines called :
*   SRCOLS,ARGS_COLR   : E2DLIB
*   WRUSER             : STARLINK
*
*   B.D.Kelly/ROE/18.12.1981
*-

      LOGICAL*2 CTRLC
      INTEGER JCOL(3,256),IDCOL(3,256)

      CALL WRUSER(' ************************************************',
     :            ISTAT)
      CALL WRUSER(' SWEEP CONTINUES UNTIL CONTROL-C IS TYPED',ISTAT)
 
      DO WHILE(.NOT.CTRLC)
*
*     Read colour table from ARGS
*
         CALL ARGS_COLR(IDCOL)
*
*     Rotate by one step and send to ARGS
*
         DO J=1,256
            K=J+1
            IF(K.GT.256) K=1
            DO I=1,3
               JCOL(I,J)=IDCOL(I,K)
            ENDDO
         ENDDO

         CALL SRCOLS(0,256,JCOL,IFAIL)
 
      ENDDO

      CTRLC=.FALSE.

      END
