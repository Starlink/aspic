      subroutine getpar(name,type,ireply,botlim,toplim,dflt,ival,rval
     : ,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN NUMERICAL PARAMETER VALUES FROM THE ENVIRONMENT, CHECK
*       THEIR VALIDITY AND HANDLE ANY ERRORS WHICH OCCUR
*
*METHOD
*       OBTAIN VALUE (EITHER REAL OF INTEGER) AND CHECK IT IS VALID.
*       IF NOT, GIVE A MESSAGE AND PROMPT FOR A NEW VALUE
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME
*       TYPE (IN)
*       CHARACTER*(*)
*               'REAL' OR 'INTEGER'... SPECIFIES THE PARAMETER TYPE
*       IREPLY (IN)
*       INTEGER
*               IF SET TO 1 OR MORE, THE ROUTINE ISSUES HELPFUL MESSAGES
*               IF INVALID VALUES ARE GIVEN
*       BOTLIM,TOPLIM (IN)
*       REAL
*               DEFINE THE VALID DATA RANGE. IF BOTLIM.LE.TOPLIM IT IS
*               AN ALLOWED RANGE, OTHERWISE IT IS AN EXCLUDED RANGE.
*       DFLT (IN)
*       LOGICAL
*               IF .TRUE., THE ROUTINE USES THE INPUT PARAMETER VALUES
*               AS RUN-TIME DEFAULTS
*       IVAL (IN/OUT)
*       INTEGER
*               THE INTEGER PARAMETER VALUE, IF USED
*       RVAL (IN/OUT)
*       REAL
*               THE REAL PARAMETER VALUE, IF USED
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       'NAME'
*               THE PARAMETER NAME IS GIVEN IN THE ARGUMENT 'NAME'
*       BADVALUE/ERROR/
*               ACCESSED IF AN INVALID VALUE IS ENTERED
*
*CALLS
*       THIS PACKAGE:
*               RNGERR
*       STARLINK:
*               RDKEYR,WRERR,CNPAR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*),type*(*)
      logical dflt,goodpr
      real rnum(1)
      integer inum(1),cnerr
 
*
* SET MAX. NUMBER OF WRONG ENTRIES ALLOWED
*
      parameter (maxwng=4)
      nwrong=0
      ierr=0
 
*
* IF PARAMETER IS REAL...
* --------------------
*
 
      if(type.eq.'REAL') then
 
*
* LOOP WHILE PARAMETER IS NOT OK
*
         goodpr=.false.
 
 
19       if(.not.goodpr) then
 
*
* SET DEFAULT VALUE, THEN OBTAIN A NEW VALUE FROM THE ENVIRONMENT
*
            rnum(1)=rval
            call rdkeyr(name,dflt,1,rnum,nval,istat)
            goodpr=.true.
 
 
*
* IF NULL WAS ENTERED AND NO DEFAULT WAS INDICATED, SET IERR=1 AND EXIT
*
 
            if(istat.ne.0) then
 
               if(.not.dflt) ierr=1
 
            else
 
*
* IF VALUE LIES OUTSIDE THE PERMITTED RANGE, GIVE MESSAGE AND RETURN
* FOR A NEW VALUE
*
 
               if(toplim.ge.botlim.and.((rnum(1).gt.toplim).or.(rnum(1)
     :          .lt.botlim))) then
                  call wrerr('BADVALUE')
 
                  if(ireply.ge.1) call rngerr('***REAL VALUE','REAL'
     :            ,botlim,toplim)
                  goodpr=.false.
 
                  call cnpar(name,cnerr)
 
*
* IF VALUE LIES INSIDE AN EXCLUDED RANGE, GIVE MESSAGE AND RETURN FOR
* A NEW VALUE
*
 
               else if(toplim.lt.botlim.and.((rnum(1).gt.toplim).and
     :          .(rnum(1).lt.botlim))) then
                  call wrerr('BADVALUE')
 
                  if(ireply.ge.1) call rngerr('***REAL VALUE','REAL'
     :            ,botlim,toplim)
                  goodpr=.false.
 
                  call cnpar(name,cnerr)
 
               else
 
*
* OTHERWISE VALUE IS OK
*
                  rval=rnum(1)
               endif
 
            endif
 
 
*
* IF PARAMETER WAS BAD, BUT MAX NUMBER OF BAD VALUES WAS NOT REACHED,
* RETURN FOR A NEW VALUE. OTHERWISE EXIT WITH INPUT VALUE UNCHANGED
*
 
            if(.not.goodpr) nwrong=nwrong+1
 
            if(nwrong.ge.maxwng) then
 
               if(.not.dflt) ierr=1
               goodpr=.true.
 
            endif
 
            go to 19
 
         endif
 
 
      else
 
*
* IF PARAMETER IS INTEGER...
* -----------------------
* REPEAT THE ABOVE WITH INTEGER VALUES
*
         goodpr=.false.
 
 
29       if(.not.goodpr) then
            inum(1)=ival
            call rdkeyi(name,dflt,1,inum,nval,istat)
            goodpr=.true.
 
 
            if(istat.ne.0) then
 
               if(.not.dflt) ierr=1
 
            else
 
               if(toplim.ge.botlim.and.((inum(1).gt.nint(toplim)).or
     :          .(inum(1).lt.nint(botlim)))) then
                  call wrerr('BADVALUE')
 
                  if(ireply.ge.1) call rngerr('***INTEGER VALUE','INTE'/
     :             / 'GER',botlim,toplim)
                  goodpr=.false.
 
                  call cnpar(name,cnerr)
 
               else if(toplim.lt.botlim.and.((inum(1).gt.nint(toplim))
     :          .and.(inum(1).lt.nint(botlim)))) then
                  call wrerr('BADVALUE')
 
                  if(ireply.ge.1) call rngerr('***INTEGER VALUE','INTE'/
     :             / 'GER',botlim,toplim)
                  goodpr=.false.
 
                  call cnpar(name,cnerr)
 
               else
                  ival=inum(1)
               endif
 
            endif
 
 
            if(.not.goodpr) nwrong=nwrong+1
 
            if(nwrong.ge.maxwng) then
 
               if(.not.dflt) ierr=1
               goodpr=.true.
 
            endif
 
            go to 29
 
         endif
 
      endif
 
      return
 
      end
 
 
 
