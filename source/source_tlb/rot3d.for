C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   ROT3D *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ROT3D
C
C
C          FUNCTION:-
C               TO ROTATE A 3-D STARLINK IMAGE FROM (X,Y,Z) TO (Y,Z,X)
C
C
C          USE:-
C               FOLLOWING THE USE OF STK3D TO CREATE   A  3-D  IMAGE  WHERE
C               (X,Y,Z)  CORRESPONDS TO (WAVELENGTH,INCREMENT,FRAME NUMBER)
C               THEN  ROT3D  WILL  PRODUCE  A  3-D  IMAGE   WHERE   (X,Y,Z)
C               CORRESPONDS  TO  (INCREMENT,FRAME  NUMBER,WAVELENGTH)  IE A
C               PSEUDO-TAURUS DATA CUBE.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               INPUT FILE NAME
C
C         OUTPUT                              OUTPUT FILE NAME
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         CDP                      RGO                            17-MAR-82
C
C
C--------------------------------------------------------------------------



      INTEGER INAX(3),OAX(3),PIN,POUT,STATUS
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
*
*  PICK UP INPUT FRAME
*
      CALL RDIMAG('INPUT',FMT_R,3,INAX,NDIM,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 999

*
*  PICK UP OUTPUT IMAGE
*
      OAX(1) = INAX(2)
      OAX(2) = INAX(3)
      OAX(3) = INAX(1)

      CALL WRIMAG('OUTPUT',FMT_R,OAX,3,POUT,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 999
      CALL ROTATE(%VAL(PIN),INAX,%VAL(POUT),OAX)

  999 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END



      SUBROUTINE ROTATE(IN,INAX,OUT,OAX)
      INTEGER INAX(3),OAX(3)
      REAL IN(INAX(1),INAX(2),INAX(3)),OUT(OAX(1),OAX(2),OAX(3))

      DO K=1,INAX(1)
      DO J=1,INAX(3)
      DO I=1,INAX(2)

      OUT(I,J,K) = IN(K,I,J)

      ENDDO
      ENDDO
      ENDDO
      END

