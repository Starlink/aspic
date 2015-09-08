      PROGRAM LINCONV
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   LINCONV *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               LINCONV [input] [output]
C
C
C          FUNCTION:-
C               It reads a Starlink image (as  16  bit  integers)  together
C               with  the  descriptor  items BSCALE and BZERO, and converts
C               the data into REAL values using OUTPUT=INPUTxBSCALE+BZERO
C
C
C          USE:-
C               It may be used to store "correct" values when  images  have
C               been  read  by  FITSIN etc., which store scaling factors as
C               descriptor items.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The    input    frame    (any
C                                             dimension)
C
C         OUTPUT                              The   output   frame    (same
C                                             dimensions  as INPUT) holding
C                                             REAL values.
C
C
C         K F Hartley              RGO                            26-NOV-82
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(99),STATUS
      CHARACTER*80 TXT
C
C   Get the input image - I*2
C
      CALL RDIMAG('INPUT',FMT_SW,99,AX,NDIM,IPIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('ERRIN')
      ELSE
C
C      If that was OK, then get an output image. (REAL)
C
         CALL WRIMAG('OUTPUT',FMT_R,AX,NDIM,IPOUT,STATUS)
         IF (STATUS.NE.ERR_NORMAL) THEN
            CALL WRERR('ERROUT')
         ELSE
C
C         Find the values for the descriptor items BSCALE
C
            CALL RDDSCR('INPUT','BSCALE',1,TXT,I,ISTAT)
            IF (ISTAT.EQ.0) THEN
               CALL CTOR(TXT,BSCALE,ISTAT)
            ELSE
C
C            Note the default value if the item is not present.
C
               BSCALE=1.0
            END IF
C
C         ...and BZERO.
C
            CALL RDDSCR('INPUT','BZERO',1,TXT,I,ISTAT)
            IF (ISTAT.EQ.0) THEN
               CALL CTOR(TXT,BZERO,ISTAT)
            ELSE
C
C            The two default values (1,0) mean that the data values
C            do not change, but are converted into reals.
C
               BZERO=0.0
            END IF
C
C         This ensures that any dimension of frame can be handled.
C
            NVALS=1
            DO I=1,NDIM
               NVALS=NVALS*AX(I)
            END DO
C
C         Do th ework
C
            CALL COPY(%VAL(IPIN),NVALS,BSCALE,BZERO,%VAL(IPOUT))
         END IF
      END IF
C
C   Tidy up and go home.
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE COPY(IN,N,BSCALE,BZERO,OUT)
C
C      Performs the conversion
C
C         OUT    =    IN x BSCALE + BZERO
C
C      on a 1-D array.
C
C      Input and output arrays have size N.
C
C      Written by K F Hartley at RGO on 26 November 1982
C
      INTEGER*2 IN(N)
      REAL OUT(N)
      DO I=1,N
         OUT(I) = REAL(IN(I)) * BSCALE   + BZERO
      END DO
      END
