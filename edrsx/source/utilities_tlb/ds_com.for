*      module DS_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Defines a common block to holds descriptor values for several
*       CRDD files.
*
*SOURCE
*       DS_COM.INC in UTILITIES.TLB
*
*METHOD
*       The values of each descriptor are stored in an array as listed
*       below. Each element of the array contains the value for a
*       different CRDD data file (for multi-dimensional arrays, the last
*       index selects the CRDD data file). As CRDD data files are accessed
*       their descriptors should be stored in the next available 'level'
*       and DS_top should be made to point to the top level being used.
*       The names of all arrays within  the common block 'DESCR' are
*       prefixed by DS_ and have the following names (names in brackets
*       are the names used in IPMAF software):
*
*     nys (NYSAMP) -- no. of samples in-scan(NAXIS1)
*     nde (NDETS)  -- no. of detectors(NAXIS2)
*     bsc (CRDBSC) -- pixel value = DATUM*CRDBSC + CRDBZ
*     bze (CRDBZ)  -- zero offset
*     bru (CRDBU)  -- surface brightness units
*     bpx (CRDBPX) -- bad pixel value
*     con (CONTNT) -- contents of CRDD file
*     obj (OBJECT) -- name of object
*     rar (RAREQ)  -- requested RA
*     dcr (DECREQ) -- requested dec.
*     sop (SOP)    -- SOP no.
*     uex (UTCEXP) -- expected boresight crossing time(secs).
*     xex (XSCEXP) -- expected cross-scan position(arcmins).
*     ust (UTC1ST) -- boresight time(UTC secs) of first raw data sample
*     mjd (MJD)    -- Modified Julian Date (JD - 2400000.5)
*     bsa (BSAMPS) -- no. of boresight samples
*     but (BUTCS)  -- array containing UTC of boresight samples(secs).
*     sol (SOLONG) -- array containing samples of solar longitude(1950.0)(degs).
*     lam (LAMBDA) -- array containing ecliptic longitude samples(deg).
*     bet (BETA)   -- array containing ecliptic latitude samples(deg).
*     psi (PSI)    -- array containing scan angle samples(deg).
*     the (THETA)  -- array containing sun angle samples(deg)(=NU+90deg).
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
* DECLARE PARAMETERS CONTROLING DESCRIPTOR AQUISITION
*
      integer DS_mxf    ! The maximum no. of descriptor values
                        ! which can be stored in a component of DESCR
      integer DS_mxb    ! The maximum no. of boresight samples
                        ! which can be stored in DESCR
      parameter (DS_mxf=20,DS_mxb=100)
*
* DEFINE THE COMMON BLOCK TO HOLD THE DESCRIPTOR VALUES
*
      common /DESCR/    DS_nys,DS_nde,DS_bsc,DS_bze,
     :                  DS_bru,DS_bpx,DS_mjd,DS_bsa,
     :                  DS_but,DS_sol,DS_lam,DS_bet,
     :                  DS_psi,DS_the,DS_sop,DS_ust,
     :                  DS_obj,DS_con,DS_rar,DS_dcr,
     :                  DS_uex,DS_xex,DS_top

      integer           DS_nys(DS_mxf),DS_nde(DS_mxf),
     :                  DS_bpx(DS_mxf),DS_sop(DS_mxf),
     :                  DS_bsa(DS_mxf),DS_top

      real              DS_xex(DS_mxf),DS_bsc(DS_mxf),
     :                  DS_bze(DS_mxf)

      double precision  DS_mjd(DS_mxf),DS_ust(DS_mxf),
     :                  DS_uex(DS_mxf)

      real              DS_but(DS_mxb,DS_mxf),
     :                  DS_sol(DS_mxb,DS_mxf),
     :                  DS_lam(DS_mxb,DS_mxf),
     :                  DS_bet(DS_mxb,DS_mxf),
     :                  DS_psi(DS_mxb,DS_mxf),
     :                  DS_the(DS_mxb,DS_mxf)

      character*12      DS_obj(DS_mxf),DS_rar(DS_mxf),
     :                  DS_dcr(DS_mxf)
      character*30      DS_con(DS_mxf)
      character*8       DS_bru(DS_mxf)

      data              DS_top /0/
