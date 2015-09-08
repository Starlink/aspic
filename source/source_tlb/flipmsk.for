C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   FLIPMSK *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               FLIPMSK oldmask newmask
C
C
C          FUNCTION:-
C               It takes in a 2D Starlink image and replaces each pixel  by
C               1.0 minus the original pixel.
C
C
C          USE:-
C               It is designed to  handle  masks  created  by  the  program
C               POLYFILLA.  It  then  creates  a  new  mask  which  is  the
C               complement of the old one. Where the original was 1 the new
C               one  is  0  and  vice  versa.  It was designed for use with
C               ADDMSK so that all pixels EXCEPT those within the  defined
C               polygon are deemed to be good.
C
C
C
C         USER PARAMETERS:-
C
C         INMASK                              An input 2-D Starlink  image.
C                                             It only makes real sense if
C                                             it contains only 0's and 1's.
C
C         OUTMASK                             This is  the  output  of  the
C                                             form 1.0-INMASK
C
C
C         K F Hartley.             R G O                          22-DEC-81
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),STATUS
C
C   Pick the input and output images.
C
      CALL RDIMAG('INMASK',FMT_R,2,AX,I,IPIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL WRIMAG ('OUTMASK',FMT_R,AX,2,IPOUT,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
C
C   Now do the work.
C
      CALL FLIPMSK(%VAL(IPIN),AX(1),AX(2),%VAL(IPOUT))
C
C   Tidy up and go home.
C
  899 CONTINUE
      CALL FRDATA (' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE FLIPMSK(IN,N,M,OUT)
C
C   Subroutine FLIPMSK
C
C   It replaces each pixel of IN by 1.0-IN
C
C   Input parameters:-
C      IN      A 2-d Starlink image.
C      N       The first dimension of IN and OUT
C      M       The second dimension of IN and OUT
C
C   Output parameters:-
C      OUT     A 2-d Starlink Image which contains the results
C              of this trivial operation.
C
      REAL IN(N,M),OUT(N,M)
C
C   Loop through all the pixels.
C
      DO J=1,M
         DO I=1,N
            OUT(I,J)=1.0-IN(I,J)
         END DO
      END DO
      END
