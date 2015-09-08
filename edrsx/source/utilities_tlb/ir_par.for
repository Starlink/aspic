*       module IR_PAR
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Defines general parameters associtated with the IRAS
*       mission
*
*SOURCE
*       IR_PAR.INC in UTILITIES.TLB
*
*USED BY
*       CRDDTRACE
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
* DECLARE PARAMETER TYPES
*
      integer   IR_bns  ! No. of wavebands
      integer   IR_dts  ! Nominal no. of detectors per band
      real      IR_scr  ! Nominal scan rate in arcmins/s
*
* DEFINE PARAMETER VALUES
*
      parameter (IR_bns =        4,
     :           IR_dts =       16,
     :           IR_scr =     3.85)

