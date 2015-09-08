      subroutine args_minv3 (a,b,status)

*+  Invert 3x3 matrix 'a' and put inverse in 'b'.
*
*   Status returns are 0 for success and 7 for "ill-conditioned".

      integer status
      real a(3,3),b(3,3),a11,a12,a13,a21,a22,a23,a31,a32,a33,
     :                   c11,c12,c13,c21,c22,c23,c31,c32,c33,det

*   calculate cofactors
      a11 = a(1,1)
      a12 = a(1,2)
      a13 = a(1,3)
      a21 = a(2,1)
      a22 = a(2,2)
      a23 = a(2,3)
      a31 = a(3,1)
      a32 = a(3,2)
      a33 = a(3,3)

      c11 =  (a22 * a33 - a23 * a32)
      c12 = -(a21 * a33 - a23 * a31)
      c13 =  (a21 * a32 - a22 * a31)
      c21 = -(a12 * a33 - a13 * a32)
      c22 =  (a11 * a33 - a13 * a31)
      c23 = -(a11 * a32 - a12 * a31)
      c31 =  (a12 * a23 - a13 * a22)
      c32 = -(a11 * a23 - a13 * a21)
      c33 =  (a11 * a22 - a12 * a21)

*   calculate determinant
      det = a11 * c11 + a12 * c12 + a13 * c13
      if (abs(det).le.1e-18) then
          status = 7
      else

*        calculate inverse (transpose of cofactors)/(determinant)
           b(1,1) = c11 / det
           b(1,2) = c21 / det
           b(1,3) = c31 / det
           b(2,1) = c12 / det
           b(2,2) = c22 / det
           b(2,3) = c32 / det
           b(3,1) = c13 / det
           b(3,2) = c23 / det
           b(3,3) = c33 / det

           status = 0

      endif

      end
