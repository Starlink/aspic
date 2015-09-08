      SUBROUTINE PRAN (S,SS,NRAN)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE PRAN 
C
C
C         It returns a Poisson randomized version of an input number.  That 
C         is,  it  returns  a random number from a population whose mean is 
C         the input value. 
C
C         S           Real  In    This is the input data value which is  to 
C                                 be randomized. 
C
C         SS          Real  Out   This  is  the  random  value   which   is 
C                                 returned. 
C
C         NRAN        I*4   I/O   This is the  seed  value.  It  should  be 
C                                 given  any  value for the first call. Its 
C                                 value will be changed after each call. 
C
C
C         A J Penny                RGO                            11-AUG-82 
C
C-------------------------------------------------------------------------- 
                                                                                
                                                                                
                                                                                
                                 
C   METHOD:
C      For numbers less than 20
C         choose a random number less between 0 and 1
C         calculate the probability P0 of 0 as exp(-S).
C         if NR is less than P0 then exit with answer of 0
C         if not then calculate probability P1 of 1 as
C         S*exp(-s)/1.
C         if NR bewteen P0 and P1 exit with answer of 1.
C         Calculate P2 as S*Sexp(-S)/!2 and so on.
C
C      For numbers greater than 20.0
C         Poisson is about the same as Gaussian and
C         the sum of six uniform random numbers is
C         nearly Gaussian.
C
      IF (S.LT.20.0) THEN
         B=RAN(NRAN)
         K=0
         R=0.0
         A=EXP(-1.0*S)
         C=A
   10    CONTINUE
            IF (B.LT.C) GO TO 11
            K=K+1
            R=R+1.0
            A=A*S/R
            C=C+A
            GO TO 10
   11    CONTINUE
         SS=REAL(K)
      ELSE
         A=0.0
         DO K=1,6
            A=A+RAN(NRAN)
         END DO
         A=(A-3.0)/0.709
         SS=A*SQRT(S)
         SS=AINT(S+SS+0.5)
      END IF
      END
