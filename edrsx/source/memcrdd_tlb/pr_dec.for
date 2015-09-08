*      module PR_DEC
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares parameters describing the limitations of MEMCRDD
*
*SOURCE
*       PR_DEC.INC in MEMCRDD.TLB
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/88
*-------------------------------------------------------------------
*
* DECLARE PARAMETERS
*
      integer	PR_bad		! Max. allowable no. of bad values from user
      real	PR_bin		! Angular size of scan groups
      integer	PR_cla		! Max. no. of iterations between CLASSIC
				! calls when using METHOD=COMBINATION
      integer	PR_crd		! Max. no. of input crdd files
      real	PR_cut		! Threshold on normalisation values,
				! below which simulated data samples
				! are set invalid.
      logical	PR_fwd		! Macro for "forward FFT required"
      integer	PR_grp		! Max no. of sample groups allowed
      logical	PR_inv		! Macro for "inverse FFT required"
      integer	PR_mem		! Size of MEM internal storage in reals
      real	PR_rat		! Ratio of min to max stored data values
      real	PR_rin		! Flag for invalid REAL samples
      integer	PR_uni		! Fortran unit no. for MEMSYS3 diagnostics

*
* SET UP THE PARAMETER VALUES
*
      parameter (PR_bad = 3,
     :           PR_bin = 3.0,
     :           PR_cla = 8,
     :           PR_crd = 20,
     :           PR_cut = 0.3,
     :           PR_fwd = .false.,
     :           PR_grp = 50,
     :           PR_inv = .true.,
     :           PR_mem = 1600000,
     :           PR_rat = 0.005,
     :           PR_rin = 'FFFFFFFF'X,
     :           PR_uni = 10)
