      subroutine fltfn2(xc,fc)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PROVIDE AN ALTERNATIVE CALLING SEQUENCE FOR THE ROUTINE
*       FLTFUN, FOR USE IN MAXIMISING FILTER SPECRAL RESPONSES IN
*       FLTSTS
*
*METHOD
*       RETURN THE NEGATIVE OF THE RESULT OBTAINED FROM FLTFUN
*
*ARGUMENTS
*       XC (IN)
*       DOUBLE PRECISION
*               THE WAVELENGTH AT WHICH THE FILTER RESPONSE IS REQUIRED
*       FC (OUT)
*       DOUBLE PRECISION
*               MINUS THE RESULT RETURNED BY FLTFUN
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      double precision xc,fc,fltfun
      fc=-fltfun(xc)
 
      end
 
 
 
