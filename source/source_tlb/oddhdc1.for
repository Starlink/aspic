C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   ODDHDC1 *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               ODDHDC1   [ R=n ] [ A=f ]
C
C
C          FUNCTION:-
C               It applies the odd order Hierarchical Discrete  Correlation
C               technique    of   Burt   (Computer   Graphics   and   Image
C               Processing,16,20-51) to a 1-D  Starlink  image.  Successive
C               levels  of smoothing are held in the rows of the output 2-D
C               image. In this case all the smoothed data are held with the
C               same number of pixels as the original data.
C
C
C          USE:-
C               It may be used as a VERY efficient way of performing  heavy
C               Gaussian-like  smoothing.  Several degrees of smoothing are
C               obtained at no extra cost. [ cf ODDHDC2, and RHDC1 ,RHDC2 ]
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
C                                             smoothing from one  level  to
C                                             the next.
C
C         A               0.4                 This defines the shape of the
C                                             convolving     kernel.    The
C                                             default  is  chosen  to  make
C
C
C                                             this as close to  a  Gaussian
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
C   SET UP OUTPUT IMAGE
C
      AXOUT(1) = AXIN
      AXOUT(2) = MAXLEV+1
      CALL WRIMAG('OUTPUT',FMT_R,AXOUT,2,POUT,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C    CALL THE SMOOTHING ROUTINE TO DO THE WORK
C
      CALL ODDHDC1(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,A,R)
C
C   TIDY UP AND GO HOME
C
      CALL FRDATA(' ',STATUS)

C
C   ERROR EXIT
C
 9999 CONTINUE
      CALL EXIT
      END



      SUBROUTINE ODDHDC1(DATA,N,TEMP,AXOUT,A,R)
      INTEGER N,AXOUT(2),R
      REAL DATA(N)
      REAL W(-2:2)
      REAL TEMP(0:AXOUT(1)-1,0:AXOUT(2)-1)
C
C   SET THE KERNAL WEIGHTS
C
      B = 0.25
      C = 0.25 - A/2.0

      W(-2) = C
      W(2) = W(-2)
      W(-1) = B
      W(1) = W(-1)
      W(0) = A

C
C   SET THE MAXIMUM LEVEL  TO BE CALCULATED
C
      MAXLEV = AXOUT(2)-1

C
C  SET THE DATA INTO THE FIRST LEVEL
C
      DO  I=0,N-1
         TEMP(I,0) = DATA(I+1)
      ENDDO

C
C   NOW DO THE SMOOTHING FOR MAXLEV LEVELS
C
      DO 201 L=1,MAXLEV
         DO 202 I=R**L,N-R**L-1
            TEMP(I,L) = 0.0
            DO 203 J=-2,2

               TEMP(I,L) = TEMP(I,L) + W(J)*TEMP(I+J*2**(L-1),L-1)

  203       CONTINUE
  202    CONTINUE
C
C  SET EDGE AFFECTED POINTS TO VALUE IN PREVIOUS ITERATION
C
      LR = R**L
      DO  K=0,L2-1
         TEMP(K,L) = TEMP(L2,L)
      ENDDO
      DO  K=N-L2,N-1
         TEMP(K,L) = TEMP(N-L2-1,L)
      ENDDO

  201 CONTINUE             ! MAIN LOOP TERMINATOR

      END

