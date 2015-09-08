      SUBROUTINE CHEBNT (A,N,P,F)
C
 
C
 
C
 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C
 
C   SUBROUTINE CHEBNT 
 
C
 
C
 
C   Evaluation   of   a   Chebyshev   Polynomial   for   purpose   of
 
C   Interpolation.
 
C
 
C
 
C   A           R*8   In
 
C                             Array size  N  to  hold  the  Chebyshev
 
C                             coefficients     required    for    the
 
C                             interpolation.
 
C
 
C   N           I     In
 
C                             Number of Chebyshev coefficients to  be
 
C                             used
 
C
 
C   P           R*8   In
 
C                             Interpolating Factor
 
C
 
C   F           R*8   Out
 
C                             Interpolated Function
 
C
 
C
 
C   AEC (BMH)       
 
C   RGO             
 
C   28-SEP-82
 
C
 
C--------------------------------------------------------------------
 
        
        
        
C                                
C     Evaluates a Chebyshev Polynomial for the purposes of Interpolation
C
C     Parameters needed are:-
C
C         A(N) - Real*8 (Double precision) array for input of the
C                required Chebyshev coefficients.
C
C         N    - Integer for input of number of Chebyshev coefficients.
C
C         P    - Real*8 (Double precision) for input of Interpolating
C                Factor.
C
C         F    - Real*8 (Double precision) for output of Interpolated
C                Function.
C
C     Notes:-
C
C         (1) P must lie in the range 0.0 to 1.0 inclusive.
C
C         (2) The order of the Chebyshev interpolation is (N-1).
C
C         (3) In the general formula a{0} is represented by A(N).
C
C     General Formula:-
C
C         (subscripts are indicated by {}  and superscripts by [] )
C
C         f{p} = a{0} + a{1}T{1}(x) + a{2}T{2}(x) + ..... + a{n}T{n}(x)
C
C                   where      x = 2p - 1
C
C                   and  T{n}(x) = cos(ncos[-1]x)
C
C     Recursive technique used:-
C
C             b{r} = a{r} + 2xb{r+1} - b{r+2}
C
C         and b{n} = a{n}     (with b{n+2} = b{n+1} = 0)
C
C         and f{p} = a{0} + x{b1 - b2}
C
      DOUBLE PRECISION A(N),F,C,D,X,Y
      D = 0.0D0
      C = A(N-1)
      X = 2.0D0*P - 1.0D0
      Y = 2.0D0*X
      M = N-2
      DO 101 I = 1,M
        J = N - I - 1
        B = A(J) + Y*C - D
        D = C
        C = B
 101  CONTINUE
      F = A(N) + X*C - D
      RETURN
      END
