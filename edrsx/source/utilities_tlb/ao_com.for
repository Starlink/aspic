*      module AO_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Defines a common block to holds descriptor values for several
*       AO CRDD files in IPAC footprints format.
*
*SOURCE
*       AO_COM.INC in UTILITIES.TLB
*
*METHOD
*       The values of each descriptor are stored in an array as listed
*       below. Each element of the array contains the value for a
*       different AO CRDD data file (for multi-dimensional arrays, the 
*       last index selects the AO CRDD data file). As AO CRDD data files
*       are accessed their descriptors should be stored in the next 
*       available 'level' and AO_top should be made to point to the top 
*       level being used. The names of all arrays within the common 
*       block 'AO_DESCR' are prefixed by AO_ and have the following 
*	names:
*
*     nys -- maximum no. of samples per detector
*     nde -- no. of detectors (rows) in the data stack
*     bpx -- bad pixel value
*     bsc -- Scale factor for data values
*     bze -- Zero offset for data values
*     leg -- AO leg number
*     sop -- SOP number
*     obs -- Observation number
*     obj -- name of object
*     dir -- Leg direction, either 'SVY' or 'ANTI-SVY'
*     ra  -- RA of reference point
*     dec -- DEC of reference point
*     ang -- Clockwise angle from north to the scan direction in degrees
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/6/89
*-------------------------------------------------------------------
*
* DECLARE PARAMETERS CONTROLING DESCRIPTOR AQUISITION
*
      integer AO_mxf    ! The maximum no. of descriptor values
                        ! which can be stored in a component of AO_DESCR
      parameter (AO_mxf=20)

*
* DEFINE THE COMMON BLOCK TO HOLD THE DESCRIPTOR VALUES
*
      common /AO_DESCR/ AO_nys,AO_nde,AO_bpx,AO_obj,AO_dir,AO_ang,
     :                  AO_bsc,AO_bze,AO_leg,AO_sop,AO_obs,AO_top,
     :                  AO_ra,AO_dec

      integer           AO_nys(AO_mxf),AO_nde(AO_mxf),AO_bpx(AO_mxf),
     :                  AO_top,AO_leg(AO_mxf),AO_sop(AO_mxf),
     :                  AO_obs(AO_mxf)

      real              AO_bsc(AO_mxf),AO_bze(AO_mxf),AO_ang(AO_mxf),
     :                  AO_ra(AO_mxf),AO_dec(AO_mxf)

      character*8       AO_obj(AO_mxf),AO_dir(AO_mxf)

      data              AO_top /0/
