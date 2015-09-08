      SUBROUTINE MATINVI(A,N1,N)

C
 
C
 
C
 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C
 
C   SUBROUTINE MATINVI
 
C
 
C
 
C   Inversion of  a  matrix  up  to  100  x  100.   Enables  repeated
 
C   inversion  of  a  matrix  stored in a two-dimensional array.  The
 
C   matrix should be  non-singular  and  square.   Method:   Gaussian
 
C   pivotal    condensation.     No   checks   for   singularity   or
 
C   ill-conditioning.  MATINVI  is  simply  a  duplicate  of  MATINV,
 
C   produced  to  deal  with  cases  where  more  than  one matrix of
 
C   different dimensions is to be inverted during the  running  of  a
 
C   program.
 
C
 
C
 
C   A           R*8   In
 
C                             Two-dimensional square array  which  is
 
C                             to     be     inverted.     Dynamically
 
C                             dimensioned by N1, which must be  equal
 
C                             to  the  dimension in the main program.
 
C                             Actual size given  by  N.   Array  size
 
C                             limits are:- N.LE.N1.LE.100
 
C
 
C   N1          I*4   In
 
C                             Maximum dimension of A.  N1.LE.100
 
C
 
C   N           I*4   In
 
C                             Varying dimension of A.  N.LE.N1
 
C
 
C
 
C   BMH(DEH)        
 
C   RGO             
 
C    1-JUL-83
 
C
 
C--------------------------------------------------------------------
 
        
        
        

      DIMENSION IPIVOT(100),INDEX(100,2)
      DOUBLE PRECISION A(N1,N1),PIVOT(100),DETERM,AMAX,SWAP

      EQUIVALENCE (IROW,JROW), (ICOLUM,JCOLUM), (AMAX, T, SWAP)

C
C     INITIALIZATION
C

   10 DETERM=1.0
   15 DO 20 J=1,N
   20 IPIVOT(J)=0
   30 DO 555 I=1,N

C
C     SEARCH FOR PIVOT ELEMENT
C

   40 AMAX=0.0
   45 DO 105 J=1,N
   50 IF (IPIVOT(J)-1) 60, 105, 60
   60 DO 100 K=1,N
   70 IF (IPIVOT(K)-1) 80, 100, 740
   80 IF (ABS (AMAX)-ABS (A(J,K))) 85, 100, 100
   85 IROW=J
   90 ICOLUM=K
   95 AMAX=A(J,K)
  100 CONTINUE
  105 CONTINUE
  110 IPIVOT(ICOLUM)=IPIVOT(ICOLUM)+1

C
C     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL
C

  130 IF (IROW-ICOLUM) 140, 260, 140
  140 DETERM=-DETERM
  150 DO 200 L=1,N
  160 SWAP=A(IROW,L)
  170 A(IROW,L)=A(ICOLUM,L)
  200 A(ICOLUM,L)=SWAP
  260 INDEX(I,1)=IROW
  270 INDEX(I,2)=ICOLUM
  310 PIVOT(I)=A(ICOLUM,ICOLUM)
      DETERM=DETERM*PIVOT(I)

C
C     DIVIDE PIVOT ROW BY PIVOT ELEMENT
C

  330 A(ICOLUM,ICOLUM)=1.0
  340 DO 350 L=1,N
  350 A(ICOLUM,L)=A(ICOLUM,L)/PIVOT(I)

C
C     REDUCE NON-PIVOT ROWS
C

  380 DO 550 L1=1,N
  390 IF(L1-ICOLUM) 400, 550, 400
  400 T=A(L1,ICOLUM)
  420 A(L1,ICOLUM)=0.0
  430 DO 450 L=1,N
  450 A(L1,L)=A(L1,L)-A(ICOLUM,L)*T
  550 CONTINUE
  555 CONTINUE

C
C     INTERCHANGE COLUMNS
C

  600 DO 710 I=1,N
  610 L=N+1-I
  620 IF (INDEX(L,1)-INDEX(L,2)) 630, 710, 630
  630 JROW=INDEX(L,1)
  640 JCOLUM=INDEX(L,2)
  650 DO 705 K=1,N
  660 SWAP=A(K,JROW)
  670 A(K,JROW)=A(K,JCOLUM)
  700 A(K,JCOLUM)=SWAP
  705 CONTINUE
  710 CONTINUE
  740 RETURN
      END
