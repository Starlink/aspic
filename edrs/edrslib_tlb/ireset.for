      subroutine ireset
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Clears the IKON image planes, and resets the
*	IKON entry in the AGI database. The colour table is also
*	reset to a greyscale table.
*
*METHOD
*	Open the IKON image planes using SGS (causing it to be cleared),
*	then set the colour table and close the display. Finally reset 
*	the AGI	database.
*------------------------------------------------------------------
      character wkname*15,device*30

*
* GET NAME AND TYPE OF DEVICE TO BE CLEARED
*
      call defdev(device)
      call getdev('DEVICE',device,.false.,istat)
      if(istat.ne.0) goto 999
      call sgs_widen(device,itype,iconid,istat)
      if(istat.ne.0) goto 999

*
* CHECK IT IS AN IKON (ARGS USE THE ARGS DATA BASE, NOT AGI. THEY
* SHOULD BE CLEARED WITH ASPIC PROGRAM ARESET)
*
      if(itype.ne.3200.and.itype.ne.3201) then
         call wruser('*** This program only resets IKONs',istat)
         goto 999
      endif

*
* OPEN THE MAIN IMAGE PLANE USING SGS
*
      call sgs_init(6,ierr)
      call sgs_clrfg(0)
      write(device,10) iconid
  10  format('3200,',I3)
      call lbgone(device(6:))
      call sgs_opnwk(device,izone,ierr)
      if(ierr.ne.0) then
         call wruser(' ',ierr)
         call wruser('***UNABLE TO RESET IKON',ierr)
         call wruser(' ',ierr)
         goto 999
      endif

*
* RESET THE COLOUR TABLE
*
      call sgs_icurw(iwkid)
      call gqcf(itype,ierr,ncol,icol,npci)
      factor=1.0/(npci-1.0)
      do i=0,npci-1
         cint=i*factor
         call gscr(iwkid,i,cint,cint,cint)
      enddo

*
* CLOSE THE IKON IMAGE PLANES
*
      call ags_gwnam(wkname,ierr)
      call sgs_close

*
* RESET THE AGI DATABASE ENTRY FOR THE IKON
*
      call agi_pdel(wkname,ierr)
      if(ierr.ne.0) then
         call wruser(' ',ierr)
         call wruser('***UNABLE TO RESET THE AGI DATABASE',ierr)
         call wruser(' ',ierr)
      endif

*
* FINISH
*
  999 continue
      end
