      subroutine xincr(id,x,y,maxlen,len,ident,plot,ilevel,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO INTERACTIVELY OBTAIN A SET OF X POSITIONS AND ATTACHED
*       CHARACTER IDENTIFIERS FROM THE ARGS AND INSERT THEM IN
*       A LIST OF POSITIONS
*
*METHOD
*       OBTAIN X POSITION FROM THE ARGS SCREEN USING ASP_PAN AND
*       IDENTIFIER (IF REQUIRED) FROM KEYBOARD USING STARLINK
*       PARAMETER 'IDENTITY'.
*       IF THE IDENTIFIER IS BLANK, CREATE ONE USING THE CURRENT
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
*               THE Y POSITIONS WILL ALL BE UNITY
*       MAXLEN (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*               LISTS
*       LEN (IN/OUT)
*       INTEGER
*               ON ENTRY, GIVES THE NUMBER OF ENTRIES ALREADY IN THE
*               LISTS ID,X AND Y. ON EXIT, GIVES THE NUMBER OF ENTRIES
*               IN THE OUTPUT LISTS.
*       IDENT (IN)
*       LOGICAL
*               IF TRUE, IDENTIFIERS ARE PROMPTED FOR
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF POSITIONS ON
*               SCREEN AS THEY ARE OBTAINED
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*               1: LEN .GT. MAXLEN ON ENTRY
*
*STARLINK PARAMETERS
*       IDENTITY
*               CHARACTER IDENTIFIER FOR POSITIONS
*
*CALLS
*       THIS PACKAGE:
*               LBGONE,XYPRGG
*       STARLINK:
*               RDKEYC,CNPAR,CTOR,WRERR,CTOI
*       ASPIC:
*               ASP_PAN
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character plot*(*),idbuf*20,inbuf(1)*80,prbuf*80
      logical exit,ident,finish
      real xpol(2),ypol(2),x(maxlen),y(maxlen)
      byte id(20,maxlen)
 
*
* CHECK ARGUMENTS
*
 
      if(maxlen.lt.len) then
         ierr=1
 
      else
         ierr=0
 
*
* OBTAIN ID OF LAST ARGS IMAGE AND OPEN OVERLAY PLANE FOR GRAPHICS
*
         call args_numim(imlast)
         call args_ovop(8,'RED')
 
*
*
* CHECK LENGTH OF LIST IS NOT -VE, INITIALLISE BLANK IDENTIFIER
* COUNT
*
         len=max(0,len)
         nblank=1
 
*
* LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
* ------------------------------------------------------------
*
         exit=.false.
 
67       if((.not.exit).and.(len.le.maxlen)) then
 
*
* CALL ASP_PAN TO GET COORDINATES FROM ARGS SCREEN
*
            call asp_pan(ix,iy,x(len+1),y(len+1))
            y(len+1)=1.0
 
*
* TEST IF USER WANTS TO STOP
*
            finish=(x(len+1).le.0.0)
 
*
* IF NOT, PRINT POSITION IF REQUIRED
*
 
            if(.not.finish) then
 
               if(ilevel.ge.2) then
                  write(prbuf,64) x(len+1)
64                format('   X=',g13.6)
                  call lbgone(prbuf(6:))
                  call wruser(prbuf,istat)
               endif
 
 
               if(plot.eq.'CROSS')then
                  call across(imlast,x(len+1),y(len+1),3,3)
 
               else if(plot.eq.'POLYGON')then
 
                  if(len.ge.1)call args_polyl(imlast,2,x(len),y(len)
     :             ,istat)
               endif
 
 
*
* IF IDENTIFIER REQUIRED, OBTAIN FROM ENVIRONMENT
*
               inbuf(1)=' '
 
               if(ident) then
                  call rdkeyc('IDENTITY',.false.,1,inbuf,nval,istat)
                  call cnpar('IDENTITY',istat)
                  call lbgone(inbuf(1))
               endif
 
               idbuf=inbuf(1)
            endif
 
 
*
* IF STOPPING, SET EXIT
*
 
            if(finish) then
               exit=.true.
 
*
* IF PLOTTING A POLYGON, CLOSE THE END UP
*
 
               if(plot.eq.'POLYGON'.and.len.ge.3)then
                  xpol(1)=x(len)
                  ypol(1)=y(len)
                  xpol(2)=x(1)
                  ypol(2)=y(1)
                  call args_polyl(imlast,2,xpol,ypol,istat)
               endif
 
 
            else
 
*
* TEST IF LIST OF INPUT HAS OVERFLOWED
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
 
 
*
* RE-ENABLE OVERLAY PLANES
*
         call args_ovcl(8,.false.)
      endif
 
      return
 
      end
 
 
 
