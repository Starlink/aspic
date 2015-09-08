      function solang(det,band)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the effective solid angle of any detector in
*       units of 1.0E-7 steradians.
*
*SOURCE
*       SOLANG.FOR in MEMCRDD.TLB
*
*METHOD
*       Just copy the value from an array initialised by a DATA
*       statement. Dead detectors return a value of zero. NB, THIS
*       VERSION USES THE VALUES DERIVED BY MERHDAD MOSHIR AT IPAC, NOT
*       THE VALUES GIVEN IN THE IRAS EXPLANATORY SUPPLEMENT.
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
*       D.S. Berry (MAVAD::DSB) 9/1/90
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
      data      sdata/ 0.89, 2.41, 3.25, 3.21, 3.31, 2.95, 3.19, 3.22,
     :                 3.22, 3.25, 3.24, 3.17, 3.29, 3.25, 2.45, 0.92,
     :                 1.82, 3.40, 3.50, 3.39, 3.50,  0.0, 3.45,  0.0,
     :                 3.44, 3.48, 3.46, 3.46, 3.48, 3.38, 1.78,  0.0,
     :                 1.97, 4.85, 6.36, 6.10, 6.40, 6.10,  0.0, 6.04,
     :                 6.32, 6.19, 6.37, 6.07, 6.25, 6.56, 4.84, 1.92,
     :                  7.9, 13.1, 14.0, 14.1, 13.3, 13.2, 13.8, 13.7,
     :                 14.0, 13.2, 14.2, 12.9, 12.9, 13.6,  6.8,  0.0/

*
* RETURN THE APPROPRIATE VALUE IN STERADIANS
*
      solang=sdata(det,band)

*
* FINISH
*
      end
