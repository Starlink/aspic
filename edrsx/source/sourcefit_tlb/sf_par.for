*       module SF_PAR
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*
*SOURCE
*       SF_PAR.INC in SOURCEFIT.TLB
*
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/9/91
*-------------------------------------------------------------------
*
* DECLARE PARAMETER TYPES
*
      integer   maxsrc  
      integer	maxvar

*
* DEFINE PARAMETER VALUES
*
      parameter (maxsrc =          8,
     :           maxvar =   7*maxsrc+1 )
