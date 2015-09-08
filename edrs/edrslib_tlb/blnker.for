      subroutine blnker
      real wind(2)
      character title(1)*30,cval*1,dir*1
 
*
* OBTAIN  IMAGE FRAME
*
      call gt2dir('IMAGE',102,.false.,npix,nlin,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
*
* SET DEFAULT DESCRIPTOR ITEM VALUES
*
         title(1)=' '
         inval=-100000
         bscale=1.0
         bzero=0
 
*
* OBTAIN DESCRIPTOR VALUES
*
         call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title(1)
     :    ,istat)
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,istat)
         call gtdscr('IMAGE','BSCALE','REAL',ival,bscale,cval,istat)
         call gtdscr('IMAGE','BZERO','REAL',ival,bzero,cval,istat)
 
*
* SET OUTPUT INVALID FLAG
*
 
         if(abs(inval).le.32767) then
            invalo=inval
 
         else
            invalo=-32767
         endif
 
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY.OBTAIN THE WINDOW AND DIRECTION
*
            idir=1
            call getcmd('AXIS','X,Y.',1,idir,cval,lcom,istat)
            wind(1)=1
            wind(2)=npix
10          call rdkeyr('WINDOW',.false.,2,wind,nval,ierrb)
 
            if(ierrb.eq.4) goto 99
 
            if(nval.ne.2) goto 10
 
*
* IF WIND(1) IS LESS THAN WIND(2),THE WINDOW IS TO BE BLANKED OUT
*
 
            if(wind(1).le.wind(2)) then
               oper=1
 
*
* OTHERWISE,EVERYTHING BUT THE WINDOW IS TO BE BLANKED
*
 
            else
               oper=2
 
*
* SWAP WIND(1) AND WIND(2) TO GET WIND(1) LESS THAN WIND(2)
*
               temp=wind(1)
               wind(1)=wind(2)
               wind(2)=temp
            endif
 
 
*
* IF THE WINDOW REFERS TO THE X AXIX SET THE MAX CO-ORD VALUE=NPIX
*
 
            if(idir.eq.1) then
               cmax=npix
 
*
* OTHERWISE SET THE MAX CO-ORD VALUE TO NLIN
*
 
            else
               cmax=nlin
            endif
 
 
*
* RESTRICT THE WINDOW TO LIE WITHIN THE IMAGE
*
            wind(1)=max1(1.0,wind(1))
            wind(2)=min1(cmax,wind(2))
 
*
* IF ALL THE IMAGE IS TO BE REJECTED,SEND MESSAGE AND END
*
 
            if(((oper.eq.1).and.(wind(1).le.1.0).and.(wind(2).ge.cmax))
     :       .or.((oper.eq.2).and.(wind(1).eq.wind(2)))) then
               call wrerr('ALLREJ')
               goto 99
 
            endif
 
 
*
* OBTAIN INTERACTION LEVEL
*
            ilevel=2
            call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel
     :       ,rval,istat)
 
*
* CALL BLNKIT TO DO THE BLANKING
*
            call blnkit(%val(ipa),invalo,npix,nlin,wind,2,oper,idir
     :       ,%val(ipout),ilevel)
 
*
* OBTAIN OUTPUT TITLE
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT AND UPDATE ALTERED ITEMS
*
            call cydscr('IMAGE','OUTPUT',istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalo,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
