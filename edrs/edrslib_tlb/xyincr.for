      subroutine xyincr(id,x,y,len,minlen,maxlen,ident,plot,colour,
     :iplane,ilevel,tr,prefix,device,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO INTERACTIVELY OBTAIN A SET OF X,Y POSITIONS AND ATTACHED
*       CHARACTER IDENTIFIERS FROM THE ARGS OR IKON AND INSERT THEM IN
*       A LIST OF POSITIONS, ALLOWING THE USER TO PAN AND ZOOM AROUND
*       THE ARGS IMAGE (THE IKON DOES NOT YET SUPPORT THIS FACILITY).
*
*METHOD
*       OBTAIN X,Y POSITION FROM THE SCREEN USING XYSCRN AND
*       IDENTIFIER (IF REQUIRED) FROM KEYBOARD USING STARLINK
*       PARAMETER 'IDENTITY'. IF THE IDENTIFIER IS BLANK, CREATE ONE
*       USING THE CURRENT COUNT OF BLANK IDENTIFIERS ENTERED. IF THE ID
*       ENTIFIER IS IN THE FORM #N, RESET THE BLANK COUNTER TO N.
*       OTHERWISE USE THE IDENTIFIER AS IT STANDS AND ADD IT TO THE
*       LIST. IF THE LIST IS FULL, CALL XYPRGG TO REMOVE ANY DUPLICATE
*       ENTRIES...IF STILL FULL RETURN. IN ANY CASE CALL XYPRGG BEFORE
*       RETURNING.
*
*ARGUMENTS
*       ID (IN/OUT)
*       BYTE(20,MAXLEN)
*               A LIST OF 20 BYTE ASCII IDENTIFIERS
*       X,Y (IN/OUT)
*       REAL(MAXLEN)
*               LISTS OF X,Y POSITIONS
*       LEN (IN/OUT)
*       INTEGER
*               ON ENTRY, GIVES THE NUMBER OF ENTRIES ALREADY IN THE
*               LISTS ID,X AND Y. ON EXIT, GIVES THE NUMBER OF ENTRIES
*               IN THE OUTPUT LISTS.
*       MINLEN (IN)
*       INTEGER
*               THE MINIMUM NUMBER OF ENTRIES REQUIRED IN THE OUTPUT
*               LIST
*       MAXLEN (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*               LISTS
*       IDENT (IN)
*       LOGICAL
*               IF TRUE, IDENTIFIERS ARE PROMPTED FOR
*       COLOUR (IN)
*       CHARACTER*(*)
*               SPECIFIES THE COLOUR TO USE FOR PLOTTING ON THE OVERLAY
*               TAKES VALUES: RED, GREEN, BLUE, YELLOW, CYAN,
*               MAGENTA, BLACK, WHITE.
*       IPLANE (IN)
*       INTEGER
*               SPECIFIES WHICH OVERLAY PLANE TO PLOT INTO
*               TAKES VALUES 8-15
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF POSITIONS ON
*               SCREEN AS THEY ARE OBTAINED
*       TR (IN)
*       REAL(6)
*               SET OF TRANSFORMATION COEFFICIENTS TO BE APPLIED TO THE
*               COORDINATES
*       PREFIX (IN)
*       CHARACTER*(*)
*               A PREFIX TO BE ADDED TO EACH IDENTIFIER
*	DEVICE (IN)
*	CHARACTER*(*)
*		NAME OF DEVICE TO USE: IKON OR ARGS
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*               1: LEN.GT.MAXLEN OR MINLEN.GT.MAXLEN ON ENTRY
*
*STARLINK PARAMETERS
*       IDENTITY
*               CHARACTER IDENTIFIER FOR POSITIONS
*
*CALLS
*       THIS PACKAGE:
*               XYSCRN,LBGONE,XYPRGG,LENS,OVOPEN,OVCLOS,OVPOLY,OVCROS
*       STARLINK:
*               WRUSER,RDKEYC,CNPAR,CTOI,WRERR
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH (modified by DS Berry to use IKON)
*-----------------------------------------------------------------------
*
*
      character plot*(*),idbuf*20,inbuf(1)*80,prbuf*80,colour*(*),
     :prefix*(*),device*(*)
      logical exit,ident,finish
      real xpol(2),ypol(2),x(maxlen),y(maxlen),tr(6)
      byte id(20,maxlen)
 
*
* CHECK ARGUMENTS
*
 
      if(maxlen.lt.len.or.minlen.gt.maxlen) then
         ierr=1
 
      else
         ierr=0
 
*
* REMEMBER THE ORIGINAL LENGTH OF THE INPUT LIST
*
         len0=len
 
*
* INITIALLISE COUNTER FOR NUMBER OF ENTRIES MADE
*
         nentry=0
 
*
* OPEN THE DEVICE OVERLAYS FOR LINE GRAPHICS
*
         call ovopen(device,iplane,colour)
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
* CALL XYSCRN TO GET COORDINATES FROM SCREEN
*

            call xyscrn(ix,iy,x(len+1),y(len+1),finish,device,ierr)
*
* CHECK THAT THE MINIMUM NUMBER OF ENTRIES HAS BEEN MADE
* IF NOT, GO BACK FOR MORE
*
 
            if(finish.and.nentry.lt.minlen-len0)then
               minent=minlen-len0
               write(prbuf,106)minent
106            format('***AT LEAST ',i8,' POSITIONS ARE REQUIRED')
               call lbgone(prbuf(13:))
 
               if(minent.eq.1)prbuf='***AT LEAST 1 POSITION IS REQUIRED'
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               go to 67
 
            endif
 
 
*
* FINISH INDICATES IF THE USER WANTS TO QUIT THE PROGRAM
*
* IF NOT, TRANSFORM THE COORDINATES, REMEMBERING THE ORIGINAL
* VALUES
*
 
            if(.not.finish) then
               nentry=nentry+1
 
               if(nentry.eq.1)then
                  ax0=x(len+1)
                  ay0=y(len+1)
               endif
 
               ax1=ax2
               ay1=ay2
               ax2=x(len+1)
               ay2=y(len+1)
               call trnxy(x(len+1),y(len+1),1,tr,x(len+1),y(len+1))
 
*
* IF REQUIRED, PRINT COORDINATES
*
 
               if(ilevel.ge.2) then
                  write(prbuf,64) x(len+1),y(len+1)
64                format('   X=',g13.6,':Y=',g13.6)
                  call lbgone(prbuf(22:))
                  call lbgone(prbuf(15:))
                  call lbgone(prbuf(6:))
                  call wruser(prbuf,istat)
               endif
 
 
*
* IF REQUIRED, PLOT THE APPROPRIATE SYMBOL ON THE OVERLAY
*
 
               if(plot.eq.'CROSS')then
                  call ovcros(ax2,ay2,3,3,device)
 
               else if(plot.eq.'POLYGON')then
 
                  if(nentry.ge.2)then
                     xpol(1)=ax1
                     ypol(1)=ay1
                     xpol(2)=ax2
                     ypol(2)=ay2
                     call ovpoly(xpol,ypol,2,device)
 
                  else
                     call ovcros(ax2,ay2,1,1,device)
                  endif
 
 
               else if(plot.eq.'FRAME')then
                  call ovcros(ax2,ay2,1024,780,device)
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
               if(plot.eq.'POLYGON'.and.nentry.ge.3)then
                  xpol(1)=ax2
                  ypol(1)=ay2
                  xpol(2)=ax0
                  ypol(2)=ay0
                  call ovpoly(xpol,ypol,2,device)
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
* PUT ID INTO IDENTIFIER LIST, ADDING PREFIX IF REQUIRED
*
 
                  if(prefix.ne.' ')then
                     idbuf=prefix(:lens(prefix))//idbuf
                     call lbgone(idbuf)
                  endif
 
 
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
* CLOSE DOWN THE OVERLAY GRAPHICS
*
         call ovclos(device,iplane)
      endif
 
      return
 
      end
 
 
 
