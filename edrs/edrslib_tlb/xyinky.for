      subroutine xyinky(id,x,y,maxlen,len,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO INTERACTIVELY OBTAIN A SET OF X,Y POSITIONS AND ATTACHED
*       CHARACTER IDENTIFIERS FROM THE KEYBOARD AND INSERT THEM IN
*       A LIST OF POSITIONS
*
*METHOD
*       OBTAIN X,Y AND IDENTIFIER FROM THE KEYBOARD AS CHARACTER STRINGS
*       USING STARLINK PARAMETER XYPOSN. IF A NULL IS GIVEN, RETURN WITH
*       THE CURRENT LIST, OTHERWISE CONVERT THE X AND Y POSITIONS TO
*       REAL NUMBERS, CHECKING AND RE-PROMPTING FOR INPUT IF AN ERROR
*       OCCURS. IF THE IDENTIFIER IS BLANK, CREATE ONE USING THE CURRENT
*       COUNT OF BLANK IDENTIFIERS ENTERED. IF THE IDENTIFIER IS IN THE
*       FORM #N, RESET THE BLANK COUNTER TO N. OTHERWISE USE THE
*       IDENTIFIER AS IT STANDS AND ADD IT TO THE LIST.
*         IF THE LIST IS FULL, CALL XYPRGG TO REMOVE ANY DUPLICATE
*       ENTRIES...IF STILL FULL RETURN. IN ANY CASE
*       CALL XYPRGG BEFORE RETURNING.
*
*ARGUMENTS
*       ID (IN/OUT)
*       BYTE(20,MAXLEN)
*               A LIST OF 20 BYTE ASCII IDENTIFIERS
*       X,Y (IN/OUT)
*       REAL(MAXLEN)
*               LISTS OF X,Y POSITIONS
*       MAXLEN (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*               LISTS
*       LEN (IN/OUT)
*       INTEGER
*               ON ENTRY, GIVES THE NUMBER OF ENTRIES ALREADY IN THE
*               LISTS ID,X AND Y. ON EXIT, GIVES THE NUMBER OF ENTRIES
*               IN THE OUTPUT LISTS.
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*               1: LEN .GT. MAXLEN ON ENTRY
*
*STARLINK PARAMETERS
*       XYPOSN
*               USED TO PROMP USER TO ENTER X,Y AND ID (CHARACTER
*               STRINGS) FOR NEXT LIST ENTRY. NULL TERMINATES INPUT
*               SEQUENCE.
*       WHAT/ERROR/
*               ACCESSED IF THE INPUT FROM XYPOSN CANNOT BE CONVERTED
*               TO REAL NUMBER POSITIONS.
*
*CALLS
*       THIS PACKAGE:
*               LBGONE,XYPRGG
*       STARLINK:
*               RDKEYC,CNPAR,CTOR,WRERR,CTOI
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character idbuf*20,inbuf(3)*80
      logical exit
      real x(maxlen),y(maxlen)
      byte id(20,maxlen)
 
*
* CHECK ARGUMENTS
*
 
      if(maxlen.lt.len) then
         ierr=1
 
      else
         ierr=0
 
*
* CHECK LENGTH OF LIST IS NOT -VE, INITIALLISE BLANK IDENTIFIER
* COUNT. ALSO INITIALIZE NO. OF BLANK IDENTIFIERS TO BE EQUAL TO
* THE NUMBER OIF ENTRIES IN THE INPUT LIST. THIS ENSURES THAT WHEN
* THE FINAL LIST IS PURGED AT THE END, THE INPUT LIST IS NOT DELETED.
*
         len=max(0,len)
         nblank=len+1
 
*
* LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
* ------------------------------------------------------------
*
         exit=.false.
 
67       if((.not.exit).and.(len.le.maxlen)) then
 
*
* SET DEFAULT INPUT TO BLANKS AND OBTAIN THE INPUT STRING CONTAINING
* THE X,Y POSITION AND IDENTIFIER
*
66          inbuf(1)=' '
            inbuf(2)=' '
            inbuf(3)=' '
            call rdkeyc('XYPOSN',.false.,3,inbuf,nval,iend)
 
*
* CANCEL INPUT PARAMETER FOR USE NEXT TIME
*
            call cnpar('XYPOSN',istat)
 
*
* INTERPRET USER INPUT LINE
*
            call lbgone(inbuf(1))
            call lbgone(inbuf(2))
            call lbgone(inbuf(3))
            call ctor(inbuf(1),x(len+1),ioerr1)
            call ctor(inbuf(2),y(len+1),ioerr2)
            idbuf=inbuf(3)
            ioerr=max(ioerr1,ioerr2)
 
*
* IF A NULL ENTRY WAS MADE, SET IOERR TO INDICATE END OF INPUT
*
 
            if(iend.ne.0) ioerr=-1
 
*
* IF INPUT COULD NOT BE READ, GIVE MESSAGE AND PROMPT FOR NEW INPUT
*
 
            if(ioerr.gt.0) then
               call wrerr('WHAT')
               go to 66
 
 
*
* END OF INPUT.. SET EXIT
*
 
            else if(ioerr.lt.0) then
               exit=.true.
 
            else
 
*
* OTHERWISE INPUT IS OK. TEST IF LIST OF INPUT HAS OVERFLOWED
*
 
               if(len.ge.maxlen) then
                  exit=.true.
 
               else
 
*
* INCREMENT LIST LENGTH IF IT WILL NOT OVERFLOW
*
                  len=len+1
                  exit=.false.
 
*
* TREAT THE SPECIAL CASES OF BLANK IDENTIFIER OR '#N'
* ----------------------------------------------------
*
* REMOVE LEADING BLANKS FROM IDENTIFIER AND TEST IF ALL BLANK
*
                  call lbgone(idbuf)
 
                  if(idbuf.eq.' ') then
 
*
* IF BLANK, GENERATE AN IDENTIFIER FROM THE BLANK COUNT IN THE FORM
* '#N' AND INCREMENT THE BLANK COUNT
*
                     write(idbuf,'(I20)')nblank
                     idbuf(1:1)='#'
                     call lbgone(idbuf(2:))
                     nblank=nblank+1
 
*
* IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
* IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
* RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
* SEQUENTIALLY NUMBERED '#N' FORM
*
 
                  else if(idbuf(1:1).eq.'#') then
                     call ctoi(idbuf(2:),nb,istatb)
 
                     if(istatb.eq.0) then
                        nblank=nb+1
                        write(idbuf,'(I20)')nb
                        idbuf(1:1)='#'
                        call lbgone(idbuf(2:))
                     endif
 
                  endif
 
 
*
* PUT ID INTO IDENTIFIER LIST
*
 
                  do 16 i=1,20
                     id(i,len)=ichar(idbuf(i:i))
16                continue
 
               endif
 
            endif
 
 
*
* IF LIST IS FULL, CALL XYPRGG TO REMOVE DUPLICATE ENTRIES
*
 
            if(len.ge.maxlen) then
               call xyprgg(x,y,id,len,nsave,ierr)
               len=nsave
            endif
 
 
*
* IF LIST IS STILL FULL, RETURN
*
 
            if(len.ge.maxlen) then
               exit=.true.
            endif
 
            go to 67
 
         endif
 
 
*
* PURGE THE LIST BEFORE LEAVING
*
 
         if(len.gt.1) then
            call xyprgg(x,y,id,len,nsave,ierr)
            len=nsave
         endif
 
      endif
 
      return
 
      end
 
 
 
