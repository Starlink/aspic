C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   RHDC1 *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               RHDC1   [ R=n ] [ A=f ]
C
C
C          FUNCTION:-
C               It applies the odd order Hierarchical Discrete  Correlation
C               technique    of   Burt   (Computer   Graphics   and   Image
C               Processing,16,20-51) to a 1-D  Starlink  image.  Successive
C               levels  of smoothing are held in the rows of the output 2-D
C               image. In this case advantage  is  taken  of  the  loss  of
C               resolution  caused  by  the  smoothing  to  store a REDUCED
C               number of pixels in each row. Because a  rectangular  array
C               is  being  used  this  does not actually save storage space
C               unless one row is extracted by MANIC.
C
C
C          USE:-
C               It may be used as a VERY efficient way of performing  heavy
C               Gaussian-like  smoothing.  Several degrees of smoothing are
C               obtained at no extra cost. [  cf  RHDC2  ,  and  ODDHDC1  ,
C               ODDHDC2 ]
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input 1-D Starlink image.
C
C         MAXLEV                              This is the maximum number of
C                                             levels  of  smoothing  to  be
C                                             applied. The second dimension
C                                             of OUTPUT is MAXLEV+1.
C
C         OUTPUT                              The 2-D Starlink image  which
C                                             contains  the  original  data
C                                             and  the   results   of   the
C                                             smoothing.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         R               2                   Effectively this defines  the
C                                             factor   in   the  degree  of
C                                             smothing from  one  level  to
C                                             the next.
C
C
C
C         A               0.4                 This defines the shape of the
C                                             convolving     kernel.    The
C                                             default  is  chosen  to  make
C                                             this  as  close to a Gaussian
C                                             as possible.
C
C
C
C         C D Pike                 RGO                            12-JAN-82
C
C
C--------------------------------------------------------------------------



*       CDP/RGO  9-11-1981
*
      INTEGER PIN,POUT,STATUS,AXIN,AXOUT(2),R
      CHARACTER*72 TEXT
      LOGICAL CHANGED
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C    PICK UP INPUT IMAGE
C
      CALL RDIMAG('INPUT',FMT_R,1,AXIN,INDIM,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)   THEN
         CALL WRUSER('CANT FIND THAT IMAGE',STATUS)
         GO TO 9999
      ENDIF
C
C   Pick up smoothing parameters
C
      CALL RDKEYI('R',.TRUE.,1,R,NN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         CALL WRUSER(' ERROR READING R ',STATUS)
         GO TO 9999
      ENDIF

      CALL RDKEYR('A',.TRUE.,1,A,NN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         CALL WRUSER(' ERROR ON READING A',STATUS)
         GO TO 9999
      ENDIF

      CALL RDKEYI('MAXLEV',.FALSE.,1,MAXLEV,NN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C   CHECK VALIDITY OF MAXLEV
C
      CHANGED = .FALSE.
   10 IF(R**MAXLEV.GT.AXIN/4)  THEN
         MAXLEV = MAXLEV-1
         CHANGED = .TRUE.
         GO TO 10
      ENDIF
C
C    WARNING IF MAXLEV REQUESTED IS NOT POSSIBLE
C
      WRITE(TEXT,100) MAXLEV
  100 FORMAT(1X,' MAXIMUM LEVEL OF SMOOTHING IS ',I4)
      IF(CHANGED) CALL WRUSER(TEXT,STATUS)
      AXOUT(1) = AXIN
      AXOUT(2) = MAXLEV+1
      CALL WRIMAG('OUTPUT',FMT_R,AXOUT,2,POUT,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C    CALL ROUTINE TO DO WORK
C
      CALL RHDC(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,A,R)


C
C   TIDY UP AND GO HOME
C
      CALL FRDATA(' ',STATUS)

 9999 CONTINUE
      CALL EXIT
      END



      SUBROUTINE RHDC(DATA,N,TEMP,AXOUT,A,R)
      INTEGER N,AXOUT(2),R
      REAL DATA(N)
      REAL W(-2:2)
      REAL TEMP(0:AXOUT(1)-1,0:AXOUT(2)-1)
C
C  CLEAR ARRAY
C
      DO 50 I=0,AXOUT(1)-1
      DO 50 J=0,AXOUT(2)-1
   50 TEMP(I,J) = 0.0

C
C    SET UP KERNEL
C
      B = 0.25
      C = 0.25 - A/2.0

      W(-2) = C
      W(2) = W(-2)
      W(-1) = B
      W(1) = W(-1)
      W(0) = A

C
C   SET UP MAXIMUM OUTPUT SIZE
C
      MAXLEV = AXOUT(2)-1

C
C   PUT DATA INTO FIRST LEVEL
C
      DO 100 I=0,N-1
      TEMP(I,0) = DATA(I+1)
  100 CONTINUE

C
C   DO SMOOTHING FOR MAXLEV LEVELS
C
      DO 200 L=1,MAXLEV
      K = -1
      IRL = R**L
      IRL1 = N-(R**L)-1
      DO 200 I=R,N-3,R
      K = K + 1
      DO 200 J=-2,2
      TEMP(K,L) = TEMP(K,L) + W(J)*TEMP(I+J,L-1)

  200 CONTINUE

      END

