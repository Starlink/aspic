      subroutine cutter
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CUT ALL POINTS BELOW A THRESHOLD INTENSITY AND ALL THOSE
*       ABOVE ANOTHER THRESHOLD INTENSITY FROM THE INPUT IMAGE
*
*METHOD
*       OBTAIN INPUT IMAGE.OBTAIN THE CUTTING WINDOW AS PERCENTAGES OF
*       THE TOTAL DATA RANGE.IF NO VALUE IS GIVEN SET DEFAULT WINDOW
*       IN TERMS OF ACTUAL DATA VALUES USING THE LIMITS OF THE INPUT
*       INTENSITY OBTAINED FROM A CALL TO IMGLIM.OBTAIN A NEW VALUE
*       FOR THE DATA RANGE OF THE WINDOW.CONVERT ANY GIVEN PERCENTAGE
*       WINDOW TO A DATA WINDOW,OBTAIN INTERACTION LEVEL AND CALL
*       CUTIT TO PERFORM THE CUT.UPDATE DESCRIPTOR AND EXIT
*
*STARLINK PARAMETERS
*       IMAGE
*              THE INPUT IMAGE
*       OUTPUT
*              THE OUTPUT IMAGE
*       PCRANGE
*              THE POSITIONS OF THE LOWER AND UPPER THRESHOLDS OF THE
*              CUT GIVEN AS PERCENTAGES OF THE TOTAL DATA RANGE
*       DRANGE
*              THE POSITIONS OF THE LOWER AND UPPER THRESHOLDS OF THE
*              CUT GIVEN AS ACTUAL DATA VALUES
*       ILEVEL
*              THE INTERACTION LEVEL
*       TITLE
*              A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT
*       NONEVAL/ERROR/
*              ACCESSED IF THE INPUT IMAGE CONTAINS NO VALID PIXELS
*
*CALLS
*       SPRP:
*              IMGLIM,CUTIT
*       EDRS:
*              GT2DIR,GT2DIW,GETPAR,GTDSCR,PTDSCR
*       STARLINK:
*              WRERR,RDKEYR,RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
      real pcrng(2),drng(2)
      integer lim(2),irng(2)
      character title(1)*30,cval*1
 
*
* OBTAIN INPUT FRAME
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
         bzero=0.0
 
*
* OBTAIN DESCRIPTOR VALUES
*
         call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title(1)
     :    ,istat)
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,istat)
         call gtdscr('IMAGE','BSCALE','REAL',ival,bscale,cval,istat)
         call gtdscr('IMAGE','BZERO','REAL',ival,bzero,cval,istat)
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY.CALL IMGLIM TO OBTAIN MAX AND
* MIN VALUES OF THE INTEGER IMAGE
*
            call imglim(%val(ipa),npix,nlin,inval,lim,ierrb)
 
*
* IF IMGLIM ERROR FLAG INDICATES THAT THERE ARE NO VALID PIXELS
* IN THE INPUT,GIVE MESSAGE AND EXIT
*
 
            if(ierrb.eq.1) then
               call wrerr('NONEVAL')
               goto 99
 
            endif
 
 
*
* OBTAIN THRESHOLDS
*
            pcrng(1)=0.0
            pcrng(2)=100.0
            call rdkeyr('PCRANGE',.true.,2,pcrng,nval,ierrc)
 
*
* IF NO PCRANGE WAS GIVEN,SET DEFAULTS VALUES OF DRANGE AND OBTAIN
* NEW VALUES
*
 
            if(ierrc.eq.1) then
               drng(1)=bscale*lim(1)+bzero
               drng(2)=bscale*lim(2)+bzero
               call rdkeyr('DRANGE',.true.,2,drng,nval,ierrd)
 
*
* CONVERT DRANGE FROM DATA VALUES TO INTEGER VALUES
*
               irng(1)=(drng(1)-bzero)/bscale
               irng(2)=(drng(2)-bzero)/bscale
 
*
* IF NIETHER PCRANGE  OR DRANGE WERE GIVEN,THERE IS NO POINT IN
* CARRYING ON
*
 
               if(ierrd.ne.0) goto 99
 
*
* IF PCRANGE WAS GIVEN A VALUE,CONVERT TO DRANGE USING THE LIMITS
* OBTAINED FROM IMGLIM
*
 
            else if(ierrc.eq.0) then
               pcrng(1)=pcrng(1)/100.0
               pcrng(2)=pcrng(2)/100.0
               irng(1)=lim(1)+pcrng(1)*(lim(2)-lim(1))
               irng(2)=lim(1)+pcrng(2)*(lim(2)-lim(1))
            endif
 
 
*
* OBTAIN INTERACTION LEVEL
*
            ilev=2
            call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilev,rval
     :       ,istat)
 
*
* CALL CUTIT TO PERFORM THE INTENSITY CUT
*
            call cutit(%val(ipa),inval,npix,nlin,irng,2,ilev,
     :      %val(ipout),istat)
 
*
* OBTAIN OUTPUT TITLE
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT AND UPDATE ALTERED ITEMS
*
            call cydscr('IMAGE','OUTPUT',istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,istat)
            call ptdscr('OUTPUT','INVAL','INTEGER',inval,rval,cval,
     :      istat)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
