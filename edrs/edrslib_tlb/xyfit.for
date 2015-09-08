      subroutine xyfit
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN A LEAST-SQUARES LINEAR POSITION TRANSFORMATION
*       BETWEEN 2 SETS OF X,Y POSITIONS
*
*METHOD
*       OBTAIN THE INPUT X,Y POSITION LISTS. ASSIGN WORKSPACE AND
*       EXTRACT THE X,Y POSITIONS AND IDENTIFIERS. OBTAIN PARAMETERS
*       CONTROLLING THE FITTING, THEN CALL FITLST TO OBTAIN THE
*       FIT AND PRINT THE RESULTS.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUTA
*               INPUT LIST OF POSITIONS
*       INPUTB
*               SECOND INPUT LIST OF POSITIONS
*       NOSPACE/ERROR/
*               ACCESSED IF DYNAMIC STORAGE SPACE IS NOT AVAILABLE
*       FITTYPE
*               CONTROLS NUMBER OF DEGREES OF FREEDOM IN FIT
*       NSIGMA
*               NUMBER OF STANDARD DEVIATIONS AT WHICH ABERRANT POINTS
*               ARE REJECTED
*       MXREJECT
*               MAX NUMBER OF REJECTION ITERATIONS
*       NOMATCH/ERROR/
*               ACCESSED IF INPUT LISTS HAVE NO POSITIONS WITH
*               MATCHING IDENTIFIERS
*       TRCOEFFS
*               OUTPUT PARAMETER, GIVES 6 COEFFICIENTS DEFINING THE
*               TRANSFORMATION
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GTXYLR,FITLST,TRCOUT
*       STARLINK:
*               GETDYN,WRERR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real c(6)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN FIRST INPUT DATA FRAME 'A'
*
      call gtxylr('INPUTA',.false.,nitema,llena,ipina,ierra)
 
      if(ierra.eq.0) then
 
*
* OBTAINED SUCCESSFULLY... GET SECOND FRAME 'B'
*
         call gtxylr('INPUTB',.false.,nitemb,llenb,ipinb,ierrb)
 
         if(ierrb.eq.0) then
 
*
* SECOND FRAME OK... GET WORKSPACE
*
            call getdyn('IDA',104,5*llena,ipida,istida)
            call getdyn('IDB',104,5*llenb,ipidb,istidb)
            call getdyn('XA',204,llena,ipxa,istxa)
            call getdyn('XB',204,llenb,ipxb,istxb)
            call getdyn('YA',204,llena,ipya,istya)
            call getdyn('YB',204,llenb,ipyb,istyb)
            call getdyn('VALID',104,min(llena,llenb),ipval,istval)
 
*
* IF WORKSPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if((istida.ne.0).or.(istidb.ne.0).or.(istxa.ne.0).or.(istxb
     :       .ne.0).or.(istya.ne.0).or.(istyb.ne.0).or.(istval.ne.0))
     :         then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* OBTAIN TYPE OF FIT (IFIT), NO. OF SIGMA REJECTION THRESHOLD (GAMMA)
* AND MAX NO. OF REJECTED POINTS (MAXIT)
*
            ifit=4
            call getpar('FITTYPE','INTEGER',1,1.0,4.0,.true.,ifit,rval
     :       ,ierr)
            gamma=1.0e10
            call getpar('NSIGMA','REAL',1,0.0,1.0e10,.true.,ival,gamma
     :       ,ierr)
            maxit=min(llena,llenb)
            call getpar('MXREJECT','INTEGER',1,0.0,1.0e7,.true.,maxit
     :       ,rval,ierr)
 
*
* CALL FITLST TO PERFORM THE FITTING AND DISPLAY THE RESULTS
*
            call fitlst(%val(ipina),nitema,llena,%val(ipinb),nitemb
     :       ,llenb,maxit,gamma,ifit,ilevel,c,%val(ipida),%val(ipxa)
     :        ,%val(ipya),%val(ipidb),%val(ipxb),%val(ipyb),%val(ipval)
     :         ,ierrf)
 
*
* IF NO MATCHES ARE FOUND BETWEEN THE TWO SETS OF POSITIONS, GIVE
* MESSAGE AND ABORT
*
 
            if(ierrf.eq.1) then
               call wrerr('NOMATCH')
               go to 99
 
            endif
 
 
*
* WRITE TRANSFORMATION COEFFICIENTS TO ENVIRONMENT
*
            call trcout('TRCOEFFS',c,6,istat)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
