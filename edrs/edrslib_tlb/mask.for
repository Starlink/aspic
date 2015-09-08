      subroutine mask
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO VIEW ON IMAGE THROUGH A MASK COMPOSED OF THE VALID PIXELS
*       OF ANOTHER IMAGE
*
*METHOD
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       AIMAGE
*               THE FIRST INPUT IMAGE
*       BIMAGE
*               THE SECOND INPUT IMAGE
*       WRONGSIZ/ERROR/
*               ACCESSED IF INPUT IMAGES ARE NOT THE SAME SIZE
*       OUTPUT
*               OUTPUT IMAGE
*       TITLE
*               TITLE TO REPLACE INPUT TITLE IN OUTPUT IMAGE
*
*CALLS
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30,cval*1
 
*
* OBTAIN FIRST IMAGE FRAME
*
      call gt2dir('AIMAGE',102,.false.,npixa,nlinea,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY... GET SECOND IMAGE
*
         call gt2dir('BIMAGE',102,.false.,npixb,nlineb,ipb,ierrb)
 
         if(ierrb.eq.0) then
 
*
* SECOND IMAGE OK... CHECK IT IS THE SAME SIZE AS THE FIRST
* IF NOT, GIVE MESSAGE AND ABORT
*
 
            if((npixa.ne.npixb).or.(nlinea.ne.nlineb)) then
               call wrerr('WRONGSIZ')
               go to 99
 
            endif
 
 
*
* SET DEFAULT DESCRIPTOR ITEM VALUES
*
            title(1)=' '
            invala=-100000
            ascale=1.0
            azero=0.0
            invalb=-100000
            bscale=1.0
            bzero=0.0
 
*
* OBTAIN DESCRIPTOR VALUES
*
            call gtdscr('AIMAGE','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call gtdscr('AIMAGE','INVAL','INTEGER',invala,rval,cval
     :       ,ierr)
            call gtdscr('AIMAGE','BSCALE','REAL',ival,ascale,cval,ierr)
            call gtdscr('AIMAGE','BZERO','REAL',ival,azero,cval,ierr)
            call gtdscr('BIMAGE','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
            call gtdscr('BIMAGE','BSCALE','REAL',ival,bscale,cval,ierr)
            call gtdscr('BIMAGE','BZERO','REAL',ival,bzero,cval,ierr)
 
*
* SET INVALC TO INVALA, OR TO -32767 IF THERE ARE NO INVALID PIXELS
* IN IMAGE A
*
 
            if((invala.ge.-32767).and.(invala.le.32767)) then
               invalc=invala
 
            else
               invalc=-32767
            endif
 
 
*
*  SET OUTPUT SCALE AND ZERO TO THE A IMAGE VALUES
*
            cscale=ascale
            czero=azero
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
            call gt2diw('OUTPUT',102,.false.,npixa,nlinea,ipout,ierrou)
 
            if(ierrou.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY...CALL IMGMSK TO PERFORM THE
* MASKING
*
               call imgmsk(%val(ipa),ascale,azero,invala,%val(ipb),
     :         bscale,bzero,invalb,npixa,nlinea,cscale,czero,invalc
     :          ,%val(ipout))
 
*
* OBTAIN OUTPUT TITLE
*
               call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT AND UPDATE ALTERED ITEMS
*
               call cydscr('AIMAGE','OUTPUT',istat)
               call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
               call ptdscr('OUTPUT','INVAL','INTEGER',invalc,rval,cval
     :          ,ierr)
            endif
 
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
