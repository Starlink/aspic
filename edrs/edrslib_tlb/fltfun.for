      double precision function fltfun(w)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO EVALUATE THE COMBINED RESPONSE OF A SET OF FILTERS, WHOSE
*       INDIVIDUAL RESPONSES ARE STORED IN A COMMON BLOCK (BY FILTSP).
*
*METHOD
*       THE COMMON BLOCK CONTAINS DENSITY DATA AS A FUNCTION OF
*       WAVELENGTH FOR EACH FILTER. THIS IS INTERPOLATED AND CONVERTED
*       BACK TO TRANSMISSION. THE RESPONSES OF ALL THE FILTERS ARE
*       MULTIPLIED TOGETHER AND FINALLY THE SOURCE SPECTRUM SPECIFIED
*       IN THE COMMON BLOCK /FLTBL2/ IS MULTIPLIED IN. IF NECESSARY,
*       THE RESULT IS FURTHER MULTIPLIED BY A POWER OF THE WAVELENGTH
*       AS SPECIFIED BY THE BLOCK /FLTBL2/.
*
*ARGUMENTS
*       FLTFUN (FUNCTION NAME)
*       DOUBLE PRECISION
*               RETURNS THE COMBINED RESPONSE
*       W (IN)
*       DOUBLE PRECISION
*               THE WAVELENGTH AT WHICH THE RESPONSE IS REQUIRED
*
*CALLS
*       THIS PACKAGE:
*               INTERP,BBSPEC
*
*NOTES
*       USES THE COMMON BLOCKS /FLTBL1/ AND /FLTBL2/. COMMON
*       VARIABLES ARE NOT ALTERED BY THIS ROUTINE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK CONTAINING FILTER INTERPOLATION DATA
*
      include 'edrslib(filt1com.inc)'
 
*
* INCLUDE COMMON BLOCK SPECIFYING HOW TO WEIGHT THE RESPONSE
*
      include 'edrslib(filt2com.inc)'
 
*
* DECLARATIONS...
*
      double precision w
 
*
* PROCESS EACH INDIVIDUAL FILTER IN TURN
*
      resp=1.0e0
 
      do 1 i=1,nfil
 
*
* INTERPOLATE THE FILTER RESPONSE
*
         call interp(real(w),wavel(1,i),dens(1,i),npts(i),dresp)
 
*
* CONVERT TO TRANSMISSION, INCLUDING THE ELEMENT THICKNESS
*
         dresp=10.0**(-dresp*elemts(i))
         resp=resp*dresp
1     continue
 
 
*
* IF NECESSARY, WEIGHT THE RESPONSE WITH THE SOURCE SPECTRUM
*
 
      if(isourc.eq.2)then
         resp=resp*bbspec(sconst,real(w)*1.0e-9)*1.0e-9
 
      else if(isourc.eq.3)then
         resp=resp*(w/550.0)**sconst
      endif
 
 
*
* WEIGHT THE RESULT WITH THE REQUIRED POWER OF THE WAVELENGTH
* BEFORE RETURNING
*
      resp=resp*(real(w)**iwgt)
      fltfun=resp
 
      end
 
 
 
