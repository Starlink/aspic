      subroutine per_fft(x,w,n)
 
*
*   This subroutine computes the Fourier Transform of array
*   X, storing the results back in X, using the NAG subroutine
*   C06FAF using the array W as workspace.
*
*   Written by K.F.Hartley at RGO on 7-2-84
*
      double precision x(n),w(n)
      ifail=0
      call c06faf(x,n,w,ifail)
 
      end
 
 
 
