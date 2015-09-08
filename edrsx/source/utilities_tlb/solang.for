      function solang(det,band)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the effective solid angle of any detector in
*       units of 1.0E-7 steradians.
*
*SOURCE
*       SOLANG.FOR in UTILITIES.TLB
*
*METHOD
*       Just copy the value from an array initialised by a DATA
*       statement. Dead detectors return a value of zero. The current
*       values are just the values from the Explanatory Supplement
*       Table IV.A.1 (page IV-8), but these values may change since
*       they may not have actually been used by IPAC.
*
*ARGUMENTS
*   INPUTS:
*       band    integer         IRAS band no
*       det     integer         Detector cross scan position
*   OUTPUTS:
*       solang  real            Detector solid angle*1.0E7 in steradians
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/9/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   det,band
      real      solang

*
* INCLUDE COMMON BLOCK HOLDING INFO ABOUT IRAS (VARIABLES CALLED
* IR_xxx).
*
      include   'UTILITIES(IR_PAR)'

*
* DECLARE LOCAL VARIABLES
*
      real      sdata(IR_dts,IR_bns)    ! Solid angle data

*
* INITIALISE THE ARRAY
*
      data      sdata/ 0.77,  2.0,  2.7,  2.9,  3.1,  3.1,  2.5,  3.0,
     :                  2.9,  2.5,  2.8,  3.2,  3.0,  2.8,  2.0,  1.2,
     :                  1.4,  2.8,  3.2,  3.5,  3.1,  0.0,  3.2,  0.0,
     :                  3.1,  2.8,  3.2,  3.6,  3.4,  3.1,  2.4,  0.0,
     :                  2.1,  4.3,  6.3,  7.2,  6.4,  6.6,  0.0,  6.7,
     :                  5.9,  6.1,  6.6,  6.6,  6.5,  6.2,  3.9,  2.8,
     :                  7.1,11.53, 11.7, 14.5, 14.0, 12.0, 13.3, 12.7,
     :                 13.2, 12.4, 13.5, 13.0, 11.2, 12.6, 10.6,  0.0/

*
* RETURN THE APPROPRIATE VALUE IN STERADIANS
*
      solang=sdata(det,band)

*
* FINISH
*
      end
