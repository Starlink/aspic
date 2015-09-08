      function bbspec(t,wavel)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE THE EMMITTED POWER (WATTS) FROM A UNIT AREA BLACK
*       BODY PER UNIT WAVELENGTH INTERVAL (METRES) AT A TEMPERATURE T
*
*ARGUMENTS
*       BBSPEC (FUNCTION NAME)
*       REAL
*               RETURNS THE BLACK BODY SPECTRUM IN WATTS/SQ. M/M
*       T (IN)
*       REAL
*               THE TEMPERATURE (KELVIN)
*       WAVEL (IN)
*       REAL
*               THE WAVELENGTH (METRES)
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      data c1/3.74185e-16/
      data c2/1.43883e-2/
 
*
*
      z5=c1**0.2
      g=c2/(t*wavel)
 
      if(g.gt.15.0)then
         bbspec=exp(-g)/((wavel/z5)**5)
 
      else
         bbspec=1.0/((exp(g)-1.0)*((wavel/z5)**5))
      endif
 
 
      end
 
 
 
