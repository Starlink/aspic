C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   ODDHDC2 *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               ODDHDC2   [ R=n ] [ A=f ]
C
C
C          FUNCTION:-
C               It applies the odd order Hierarchical Discrete  Correlation
C               technique    of   Burt   (Computer   Graphics   and   Image
C               Processing,16,20-51) to a 2-D  Starlink  image.  Successive
C               levels  of smoothing are held in the z-planes of the output
C               3-D image.
C
C
C          USE:-
C               It may be used as a VERY efficient way of performing  heavy
C               Gaussian-like  smoothing.  Several degrees of smoothing are
C               obtained at no extra cost. [  cf  ODDHDC1  ,  and  RHDC1  ,
C               RHDC2 ]
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input 2-D Starlink image.
C
C         MAXLEV                              This is the maximum number of
C                                             levels  of  smoothing  to  be
C                                             applied. The third  dimension
C                                             of OUTPUT is MAXLEV+1.
C
C         OUTPUT                              The 3-D Starlink image  which
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
C         A               0.4                 This defines the shape of the
C                                             convolving     kernel.    The
C                                             default  is  chosen  to  make
C                                             this  as  close to a Gaussian
C
C
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
      INTEGER PIN,POUT,STATUS,AXIN(2),AXOUT(3),R
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C    PICK UP INPUT IMAGE
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIN,INDIM,PIN,STATUS)
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
C    SET UP THE OUTPUT IMAGE
C
      AXOUT(1) = AXIN(1)
      AXOUT(2) = AXIN(2)
      AXOUT(3) = MAXLEV+1
      CALL WRIMAG('OUTPUT',FMT_R,AXOUT,3,POUT,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  GO TO 9999
C
C    CALL THE SMOOTHING ROUTINE TO DO ALL THE WORK
C
      CALL ODDHDC2(%VAL(PIN),AXIN,%VAL(POUT),AXOUT,A,R)

C
C    TIDY UP AND GO HOME
C
      CALL FRDATA(' ',STATUS)

 9999 CONTINUE
      CALL EXIT
      END



      SUBROUTINE ODDHDC2(DATA,AXIN,TEMP,AXOUT,A,R)
      INTEGER AXIN(2),AXOUT(3),R
      REAL DATA(AXIN(1),AXIN(2))
      REAL W(-2:2,-2:2),WW(-2:2)
      REAL TEMP(0:AXOUT(1)-1,0:AXOUT(2)-1,0:AXOUT(3)-1)
C
C   SET THE KERNAL WEIGHTS
C
      B = 0.25
      C = 0.25 - A/2.0

      WW(-2) = C
      WW(2) = WW(-2)
      WW(-1) = B
      WW(1) = WW(-1)
      WW(0) = A


      DO 10 I=-2,2
      DO 10 J=-2,2
      W(I,J) = WW(I)*WW(J)
   10 CONTINUE
C
C   SET THE MAXIMUM LEVEL  TO BE CALCULATED
C
      MAXLEV = AXOUT(3)-1

C
C  SET THE DATA INTO THE FIRST LEVEL
C
      DO 100 I=0,AXIN(1)-1
      DO 100 J=0,AXIN(2)-1
      TEMP(I,J,0) = DATA(I+1,J+1)
  100 CONTINUE

C
C   NOW DO THE SMOOTHING FOR MAXLEV LEVELS
C
      DO 201 L=1,MAXLEV
      DO 202 IY=R**L,AXIN(2)-R**L-1
      DO 202 IX=R**L,AXIN(1)-R**L-1
      TEMP(IX,IY,L) = 0.0
      DO 203 JY=-2,2
      DO 203 JX=-2,2

      TEMP(IX,IY,L) = TEMP(IX,IY,L) +
     *  W(JX,JY)*TEMP(IX+JX*R**(L-1),IY+JY*R**(L-1),L-1)

  203 CONTINUE
  202 CONTINUE
C
C  SET EDGE AFFECTED POINTS TO PREVIOUS LEVEL'S VALUE
C
      L2 = R**L
      DO 204 KY=0,AXIN(2)-1
      DO 204 KX=0,L2-1
  204 TEMP(KX,KY,L) = TEMP(KX,KY,L-1)
      DO 205 KY = 0,AXIN(2)-1
      DO 205 KX=AXIN(1)-L2,AXIN(1)-1
  205 TEMP(KX,KY,L) = TEMP(KX,KY,L-1)

      DO 206 KY=0,L2-1
      DO 206 KX=0,AXIN(1)-1
  206 TEMP(KX,KY,L) = TEMP(KX,KY,L-1)
      DO 207 KY=AXIN(2)-L2,AXIN(2)-1
      DO 207 KX = 0,AXIN(1)-1
  207 TEMP(KX,KY,L) = TEMP(KX,KY,L-1)
  201 CONTINUE

      END

