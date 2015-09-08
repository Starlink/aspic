      subroutine gtradc(parid,ra,dec,deflt,cancel,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets an RA and DEC pair from user.
*
*METHOD
*       The hour, minutes and seconds fields of the RA are aquired
*       seperately. If a fractional value is given for any of these
*       fields then the later ones are not aquired. The degrees,
*       minutes and seconds fields of the dec are treated similarly.
*       The interim parameters used have a standard form suplemented
*       by a single character given by argument parid (see STARLINK
*       PARAMETERS).
*
*SOURCE
*       GTRADC.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       character       parid   A single character identifier to include
*                               in the name of the parameters used.
*       logical         cancel  If .true. then parameter associations
*                               are cancelled after use.
*	logical		deflt   If true, input values of ra and dec are
*				used as defaults.
*   OUTPUTS:
*       real            ra      RA given (in hours).
*       real            dec     DEC given (in degrees).
*       integer         ierr    Error status: 0 - Success
*
*SUBROUTINES CALLED
*       EDRS:
*               getpar
*       INTERIM:
*               cnpar
*
*STARLINK PARAMETERS
*       RAx_HRS/read/   Hours field of RA ("x" is replaced by the
*                       value of the argument parid).
*       RAx_MINS/read/  Minutes field of RA
*       RAx_SECS/read/  Seconds field of RA
*       DECx_DEG/read/  Degrees field of DEC
*       DECx_MIN/read/  Minutes field of DEC
*       DECx_SEC/read/  Seconds field of DEC
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 11/3/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      real      ra,dec
      integer   ierr
      logical   cancel,deflt
      character parid*1

*
* DECLARE LOCAL VARIABLES
*
      real      dcdeg   ! Degrees value of Dec
      real      dcmin   ! Minutes value of Dec
      real      dcsec   ! Seconds value of Dec
      integer   i       ! Loop count
      integer   ival    ! Dummy integer argument
      character parnam(6)*8 ! Parameter names used for aquiring values
      real      rahrs   ! Hours value of RA of
      real      ramns   ! Minutes value of RA
      real      rasec   ! Seconds value of RA

*
* CONSTRUCT PARAMETER NAMES
*
      if(parid.ne.' ') then
         parnam(1)='RA'//parid//'_HRS'
         parnam(2)='RA'//parid//'_MINS'
         parnam(3)='RA'//parid//'_SECS'
         parnam(4)='DEC'//parid//'_DEG'
         parnam(5)='DEC'//parid//'_MIN'
         parnam(6)='DEC'//parid//'_SEC'
      else
         parnam(1)='RA_HRS'
         parnam(2)='RA_MINS'
         parnam(3)='RA_SECS'
         parnam(4)='DEC_DEG'
         parnam(5)='DEC_MIN'
         parnam(6)='DEC_SEC'
      endif

*
* GET RA HOURS FIELD (OPTIONALLY FRACTIONAL)
*
      rahrs=ra
      call getpar(parnam(1),'REAL',1,0.0,24.0,deflt,ival,
     :             rahrs,ierr)
      if(ierr.ne.0) goto 999
      if(cancel) call cnpar(parnam(1),ierr)
      ra=rahrs

*
* IF A NON-INTEGER VALUE WAS GIVEN ASSUME THE COMPLETE RA IS SPECIFIED
*
      if(int(rahrs).eq.rahrs) then

*
* IF NOT, THEN GET RA MINUTES FIELD (OPTIONALLY FRACTIONAL)
*
         call getpar(parnam(2),'REAL',1,0.0,60.0,.false.,
     :                ival,ramns,ierr)
         if(ierr.ne.0) goto 999
         if(cancel) call cnpar(parnam(2),ierr)
         ra=ra+ramns/60.0

*
* IF A NON-INTEGER VALUE WAS GIVEN ASSUME THE COMPLETE RA IS SPECIFIED
*
         if(int(ramns).eq.ramns) then

*
* IF NOT THEN GET RA SECONDS OF FIRST PIXEL
*
            call getpar(parnam(3),'REAL',1,0.0,60.0,.false.,
     :                  ival,rasec,ierr)
            if(ierr.ne.0) goto 999
            if(cancel) call cnpar(parnam(3),ierr)
            ra=ra+rasec/3600.0
         endif
      endif

*
* GET DEGREES FIELD OF DEC (OPTIONALLY FRACTIONAL)
*
      dcdeg=dec
      call getpar(parnam(4),'REAL',1,-90.0,90.0,deflt,
     :             ival,dcdeg,ierr)
      if(ierr.ne.0) goto 999
      if(cancel) call cnpar(parnam(4),ierr)
      dec=dcdeg

*
* IF A NON-INTEGER VALUE WAS GIVEN ASSUME THE COMPLETE DEC IS SPECIFIED
*
      if(int(dcdeg).eq.dcdeg) then

*
* IF NOT, THEN GET DEC MINUTES  (OPTIONALLY FRACTIONAL)
*
         call getpar(parnam(5),'REAL',1,0.0,60.0,.false.,
     :               ival,dcmin,ierr)
         if(ierr.ne.0) goto 999
         if(cancel) call cnpar(parnam(5),ierr)
         dec=dec+sign(dcmin,dcdeg)/60.0

*
* IF A NON-INTEGER VALUE WAS GIVEN ASSUME THE COMPLETE DEC IS SPECIFIED
*
         if(int(dcmin).eq.dcmin) then

*
* IF NOT, THEN GET DEC SECONDS  (OPTIONALLY FRACTIONAL)
*
            call getpar(parnam(6),'REAL',1,0.0,60.0,.false.,
     :                  ival,dcsec,ierr)
            if(ierr.ne.0) goto 999
            if(cancel) call cnpar(parnam(6),ierr)
            dec=dec+sign(dcsec,dcdeg)/3600.0
         endif
      endif

*
* FINISH
*
  999 continue

      end
