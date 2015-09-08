      subroutine crddtr
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To produce graph plot displays of the data stream from
*       selected detectors. The data is taken from a standard
*       CRDD file as produced by CRABEX or DESTRIPE, and all
*       selected detector traces are displayed at once on the
*       graphics device, offset vertically from each other.
*
*SOURCE
*       CRDDTR.FOR in CRDDTRACE.TLB
*
*METHOD
*       Get input data and descriptors. Get all required parameters.
*       Perform shift to align all data streams. Calculate default
*       values for display parameters. Enter a loop to display data
*       and change display parameters until user requests an exit.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               CALCTC,GTCRDD,GTHELP,GTLIMS,GTWORK,LNSHFT,STRLEN,SWAPR,
*               GTINAM,CRTCMD,WRERR
*       THIS PACKAGE (CRDDTRACE.TLB):
*               GTPARS,GTPUPD,TRDISP,CURPOS,AVERAG,FITPRF
*       EDRS:
*               GETPAR,LBGONE
*       INTERIM:
*               FRDATA,WRUSER
*       SGS:
*               SGS_CLOSE,SGS_INIT,SGS_OPNWK,SGS_WIDEN
*       GKS:
*               GQCF
*
*STARLINK PARAMETERS
*       INPUT/read/     The file containing the CRDD data to display
*       NEXT/read/      A string containing the next command to perform
*       XLIMS/read/     The limits of the X axis in arcmins from object
*       YMAX/read/      The upper limit of the Y axis in Janskies
*       BADUNITS/error/ Accessed if units of input data are not W/M**2
*       TOOBAD/error/   Accessed if too many bad parameter values are given
*       NODEVICE/error/ Accessed if an error occured opening the
*                       specified graphics device
*
*VAX SPECIFICS
*       implicit none
*       do while
*       enddo
*       end of line comments
*       %val
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE IRAS MISSION PARAMETERS (PARAMETERS CALLED 'IR_xxx')
*
      include 'UTILITIES(IR_PAR)'
*
* INCLUDE DETECTOR DATA (VARIABLES CALLED 'DT_xxx')
*
      include 'UTILITIES(DT_DAT)'
*
* INCLUDE CRDD DESCRIPTOR COMMON BLOCK (VARIABLES CALLED 'DS_xxx')
*
      include 'UTILITIES(DS_COM)'
*
* LOCAL VARIABLES
*
      integer   anycol          ! 1 if colour graphics available, 0
                                ! otherwise
      real      avrges(IR_dts)  ! The 'average' value of the unscaled
                                ! data in each detector stream
      integer   band            ! Input data IRAS band no. (1-4)
      real      boxxlo          ! Lower limit of plotting box x axis in sgs
                                ! top zone co-ordinates
      real      boxxhi          ! Upper limit of plotting box x axis in sgs
                                ! top zone co-ordinates
      real      boxylo          ! Lower limit of plotting box y axis in sgs
                                ! top zone co-ordinates
      real      boxyhi          ! Upper limit of plotting box y axis in sgs
                                ! top zone co-ordinates
      integer   bzone           ! SGS base zone identifier
      integer   cmd             ! Position of command within list
      character crddnm*30       ! Name of input file containing CRDD
      logical   cursor          ! True if user wishes to use cursor for
                                ! selecting commands and parameters
      integer   detout(IR_dts)  ! Ball no.s of detectors to display
      character device*30       ! SGS device identifier
      logical   exit            ! Flag. Set true when no more plots required
      integer   extend          ! No. of samples by which time correction
                                ! extends the data array
      real      fscale          ! Scale factor for converting unscaled
                                ! data into Janskys
      real      fzero           ! Zero factor for data in Janskys
      logical   got             ! Flag set when parameters sucesfully
                                ! aquired
      integer   icon            ! SGS connection identifier for
                                ! graphics device
      integer   idet            ! Detector loop count
      integer   ierr            ! Status from most recently called subroutine
      integer   infrm           ! Identifier for accessing input descriptors
      real      intmax          ! Maximum of time corrected integer data values
      real      intmin          ! Minimum of time corrected integer data values
      logical   intrac          ! Flag for 'interactive' plotting mode
      integer   ipin            ! Pointer to the start of the input data
      integer   ipwork          ! Pointer to the start of the work space
      integer   istat           ! Temporary status value
      integer   ival            ! Dummy integer argument
      character lgfile*30       ! Name of text file for screen log
      integer   lgunit          ! Fortran unit no. attached to log file.
                                ! (0 implies no log file required).
                                ! (-ve value implies no data yet logged)
      logical   more            ! If true, then more commands are to be executed
      integer   nbad            ! No. of bad parameter values given by user
      integer   ncol            ! No. of colours available for graphics
      integer   ndtout          ! No. of detectors to display
      integer   nomdet          ! Identifier of detector nearest to object
      integer   npci            ! No. of preset colour indeces on
                                ! graphics device
      integer   nsmout          ! Width in samples of entire output
                                ! display
      real      offset(IR_dts)  ! Y axis offsets for data traces
      real      refpix          ! In-scan pixel location of the object centre
      real      samphi          ! Upper X (in-scan) axis limit pixel location
      real      samplo          ! Lower X (in-scan) axis limit pixel location
      character scndir*5        ! Direction of scan, north or south
      real      smpsiz          ! The size in arcmins of one data sample
      character spaces*8        ! Method to use for calculating offsets
      integer   strlen          ! The length of a string minus trailing blanks
      real      tcorrs(IR_dts)  ! No. of samples to shift data by to
                                        ! align it with the boresight
      real      temp            ! Temporary real storage (for swapping no. etc)
      character title*70        ! Title for display
      integer   tlen            ! Length of title
      real      tmperr          ! Temporary value of xscerr
      character units*8         ! String defining y axis units
      real      unsmax          ! Unscaled data value at y axis maximum value
      real      unsmin          ! Minimum unscaled data value to be displayed
      integer   wtype           ! GKS workstation type of graphics
                                ! device
      real      xlim(2)         ! Limits of displayed x in arcmins
                                ! north of the field centre
      real      xscerr          ! Difference in z between object and centre of
                                ! closest detector
      real      ymax            ! Upper limit of flux scale in scaled units
*
* SET FORTRAN LOGICAL UNIT NO. FOR SCREEN LOGGING
*
      lgunit=10
      lgfile=' '
*
* GET THE INPUT DATA AND DESCRIPTORS
*
      call gtcrdd('INPUT',band,infrm,ipin,ierr)
      if(ierr.ne.0) goto 999
*
* DETERMINE SCAN DIRECTION BY CONSIDERING ECLIPTIC LATITUDE LIMITS
*
      if(DS_bet(DS_bsa(infrm),infrm).gt.DS_bet(1,infrm)) then
         scndir='NORTH'
      else
         scndir='SOUTH'
      endif
*
* CHANGE SCALE AND ZERO FACTORS TO GIVE JANSKYS RATHER THAN W/M**2
* ALSO INTRODUCE CORRECTION FOR OCT 1984 IRAS CALIBRATION CHANGE.
* IF UNITS OF INPUT ARE NOT WATTS PER SQUARE METRE, GIVE A WARNING
* MESSAGE AND DO NOT SCALE DATA
*
      if(index(DS_bru(infrm),'W/M**2').ne.0) then
         fscale=DS_bsc(infrm)*DT_cvf(band)*DT_o84(band)
         fzero=DS_bze(infrm)*DT_cvf(band)*DT_o84(band)
         units='Jy'
      else
         call wrerr('BADUNITS')
         call wruser(' ',ierr)
         fscale=DS_bsc(infrm)
         fzero=DS_bze(infrm)
         units=DS_bru(infrm)
      endif
*
* CALCULATE WHICH DETECTOR IS CLOSEST TO THE CENTRE OF THE REQUIRED SOURCE
* NB THE VALUES OF THE XSC-EXP DESCRIPTOR (=DS_xex) SEEM TO BE STORED
* WITH THE WRONG SIGN. THEREFORE THEY ARE NEGATED BEFORE USE.
*
      nomdet=1
      xscerr=abs(-DS_xex(infrm)-DT_zpo(nomdet,band))
      do idet=1,DS_nde(infrm)
         tmperr=abs(-DS_xex(infrm)-DT_zpo(idet,band))
         if(tmperr.lt.xscerr) then
            xscerr=tmperr
            nomdet=idet
          endif
      enddo
*
* CONSTRUCT TITLE FOR THE DISPLAY
*
      title=DS_obj(infrm)
      call lbgone(title)
      tlen=strlen(title)
      title=title(:tlen)//' - '//DS_con(infrm)
      call lbgone(title(tlen+4:))
*
* GET OTHER REQUIRED USER PARAMETERS
*
      call gtpars(DT_bal(1,band),DS_nde(infrm),nomdet,detout,ndtout,
     :            device,intrac,title,cursor,spaces,offset,lgfile,
     :            lgunit,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
*
* IF LOGGING IS REQUIRED, WRITE THE HEADER, OTHERWISE SET FORTRAN UNIT
* NO. TO 0 TO INDICATE NO LOGGING REQUIRED. THEN MAKE THE LOG UNIT NO.
* NEGATIVE TO INICATE THAT NO DATA HAS YET BEEN LOGGED
*
      if(lgfile.ne.' ') then
         call gtinam('INPUT',crddnm,ierr)
         write(lgunit,*) ' '
         write(lgunit,*) '      *** CRDDTRACE LOG FILE ***'
         write(lgunit,*) ' '
         write(lgunit,*) ' Contains log of all values displayed '//
     :        'with the "GET DATA VALUE" and'
         write(lgunit,*) ' "DRAW POINT SOURCE" options.'
         write(lgunit,*) ' '
         write(lgunit,*) ' Input CRDD file: ',crddnm
         write(lgunit,*) ' '
      else
         lgunit=0
      endif
      lgunit=-lgunit
*
* CALCULATE THE SHIFTS NECESSARY TO ALIGN ALL DETECTORS WITH BORESIGHT
*
      call calctc(DT_ypo(1,band),DS_nde(infrm),nomdet,IR_scr,
     :            DT_srt(band),tcorrs,extend)
      if(ierr.ne.0) goto 999
*
* CALCULATE MAXIMUM LENGTH OF DATA ARRAY ONCE TIME CORRECTION HAS BEEN DONE
*
      nsmout=DS_nys(infrm)+extend
*
* GET WORK SPACE FOR TIME CORRECTED DATA
*
      call gtwork('WORK','REAL',nsmout*DS_nde(infrm),ipwork,ierr)
      if(ierr.ne.0) goto 999
*
* PERFORM TIME CORRECTION TO ALIGN ALL DETECTOR STREAMS
*
      call lnshft(%val(ipin),%val(ipwork),DS_nde(infrm),DS_nys(infrm),
     :            nsmout,DS_bpx(infrm),tcorrs,intmax,intmin,detout,
     :            DS_nde(infrm),ndtout)
      if(ierr.ne.0) goto 999
*
* CALCULATE 'AVERAGE' VALUE OF DATA IN EACH DETECTOR STREAM
*
      call averag(%val(ipwork),nsmout,DS_nde(infrm),avrges,
     :            DS_bpx(infrm),intmin,intmax)
*
* CALCULATE DEFAULT IN-SCAN SCALING SO THAT ENTIRE SCAN IS DISPLAYED
*
      smpsiz=IR_scr/DT_srt(band)
      refpix=(DS_uex(infrm)-DS_ust(infrm)+DT_ypo(nomdet,band)/
     :        IR_scr)*DT_srt(band)+tcorrs(nomdet)+1
      xlim(1)=(-refpix+0.5)*smpsiz
      xlim(2)=(nsmout-refpix+0.5)*smpsiz
*
* IF THE SCAN WENT FROM NORTH TO SOUTH THEN INVERT THE LIMITS SINCE
* THEY ARE CALCULATED ASSUMING THE SCAN WENT SOUTH TO NORTH
*
      if(scndir.eq.'SOUTH') then
         xlim(1)=-xlim(1)
         xlim(2)=-xlim(2)
         call swapr(xlim(1),xlim(2))
      endif
*
* PROMPT USER FOR ALTERNATIVE IN-SCAN LIMITS
*
      call gtlims('XLIMS',xlim,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
*
* CALCULATE DEFAULT FLUX SCALING SUCH THAT NO TRACES EVER OVERLAP
*
      ymax=fscale*(ndtout*(intmax-intmin)+intmin)+fzero
*
* PROMPT USER FOR ALTERNATIVE FLUX SCALE LIMITS
*
      call getpar('YMAX','REAL',1,0.0,1e30,.true.,ival,ymax,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
*
* OPEN SGS, SENDING SGS ERROR MESSAGES TO TERMINAL SCREEN
*
      call sgs_init(6,ierr)
      if(ierr.eq.0) call sgs_opnwk(device,bzone,ierr)
      if(ierr.ne.0) then
         call wrerr('NODEVICE')
         goto 999
      endif
*
* IF NO EXIT IS CURRENTLY REQUIRED,LOOP TO DISPLAY THE DATA AGAIN
*
      exit=.false.
      do while(.not.exit)
*
* SEE IF CURRENT GRAPHICS DEVICE HAS COLOUR AVAILABLE
*
         call sgs_widen(device,wtype,icon,ierr)
         if(ierr.eq.0) call gqcf(wtype,ierr,ncol,anycol,npci)
         if(ierr.ne.0) goto 999
*
* PRODUCE DATA DISPLAY
*
         call trdisp(ymax,xlim,title,'Arcmins from field centre',units,
     :               fscale,fzero,DS_bpx(infrm),refpix,smpsiz,ndtout,
     :               %val(ipwork),nsmout,DS_nde(infrm),detout,
     :               DT_bal(1,band),DS_nde(infrm),anycol,scndir,spaces,
     :               offset,avrges,boxxlo,boxxhi,boxylo,boxyhi,samplo,
     :               samphi,unsmax,unsmin,ierr)
         if(ierr.eq.0) then
*
* IF IN INTERACTIVE MODE THEN GET NEXT COMMAND AND DO IT
*
            if(intrac) then
               more=.true.
               do while(more)
                  cmd=3
                  call crtcmd('NEXT','Quit,Redraw display,Change '//
     :                        'parameters,Get data value,'//
     :                        'Draw point source,Help.',
     :                        0.1,0.9,0.0,0.15,cmd,'Select a command'//
     :                        ' by positioning cursor and pressing '//
     :                        'any key',cursor,ierr)
                  if(ierr.ne.0) goto 999
*
* IF REQUESTED, CHANGE PARAMETER VALUES
*
                  if(cmd.eq.3) then
                     call gtpupd(xlim,ymax,detout,ndtout,
     :                           DS_nde(infrm),device,DT_bal(1,band),
     :                           bzone,cursor,title,spaces,offset,
     :                           ierr)
                     more=.false.
*
* IF REQUESTED, REDRAW DISPLAY
*
                  else if(cmd.eq.2) then
                     more=.false.
*
* IF REQUESTED, QUIT
*
                  else if(cmd.eq.1) then
                     exit=.true.
                     more=.false.
*
* IF REQUESTED, GET DATA VALUES AND POSITIONS USING CURSOR
*
                  else if(cmd.eq.4) then
                     call curpos(boxxlo,boxxhi,boxylo,boxyhi,xlim,
     :                           samplo,samphi,unsmax,unsmin,offset,
     :                           detout,DS_nde(infrm),ndtout,
     :                           %val(ipwork),nsmout,DS_nde(infrm),
     :                           DT_bal(1,band),fscale,fzero,
     :                           DS_bpx(infrm),scndir,lgunit)
*
* IF REQUESTED, OVERLAY A POINT SOURCE TEMPLATE AT A POSITION DETERMINED
* BY A CURSOR
*
                  else if(cmd.eq.5) then
                     call fitprf(boxxlo,boxxhi,boxylo,boxyhi,xlim,
     :                           samplo,samphi,unsmax,unsmin,offset,
     :                           detout,DS_nde(infrm),ndtout,nsmout,
     :                           DS_nde(infrm),DT_bal(1,band),fscale,
     :                           fzero,DS_bpx(infrm),scndir,avrges,
     :                           ipwork,refpix,smpsiz,band,cursor,
     :                           lgunit)
*
* IF REQUESTED, GIVE HELP ON THE COMMANDS AVAILABLE
*
                  else if(cmd.eq.6) then
                     call gthelp('NEXT','HELP',istat)
                  endif
               enddo
           else
               exit=.true.
           endif
*
* IF AN ERROR HAS OCCURED DO NOT LOOP TO DISPLAY THE DATA AGAIN
*
         else
            exit=.true.
         endif
      enddo
*
* CLOSE SGS
*
 999  call sgs_close
*
* IF A LOG FILE WAS OPENED...
*
      if(lgfile.ne.' ') then
*
* AND IF SOME DATA HAS BEEN LOGGED....
*
         if(lgunit.gt.0) then
*
* GIVE MESSAGE AND CLOSE THE LOG FILE
*
            call wruser(' ',ierr)
            call wruser(' Log in file '//lgfile,ierr)
            call wruser(' ',ierr)
            close(lgunit,status='KEEP')
*
* OTHERWISE IF NO DATA HAS BEEN LOGGED, CLOSE FILE AND DELETE IT
*
         else if(lgunit.lt.0) then
            close(-lgunit,status='DELETE')
         endif
      endif
*
* FREE DATA AREAS
*
      call frdata(' ',istat)

      end
