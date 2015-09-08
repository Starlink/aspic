      subroutine binner
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*        TO BIN DOWN THE INPUT IMAGE BY AVERAGEING THE PIXELS IN
*        THE INPUT BIN
*
*METHOD
*        OBTAIN INPUT IMAGE.OBTAIN SIZE OF BIN AND CALCULATE THE SIZE
*        OF THE OUTPUT IMAGE.OBTAIN OUTPUT IMAGE.CALL BINIT TO DO THE
*        BINNING.UPDATE OUTPUT DESCRIPTOR ITEMS AND EXIT
*
*ARGUMENTS
*        NONE
*
*STARLINK PARAMETERS
*        IMAGE
*             THE INPUT IMAGE TO BE BINNED
*        OUTPUT
*             THE OUTPUT IMAGE
*        XBIN
*             THE X DIMENSION OF THE BIN
*        YBIN
*             THE Y DIMENSION OF THE BIN
*        FRACVAL
*             THE MINIMUM FRACTION OF VALID PIXELS IN AN INPUT BIN,
*             REQUIRED TO PRODUCE A VALID OUTPUT PIXEL
*        MINPIX
*             THE ABSOLUTE MINIMUM NUMBER OF VALID PIXELS IN AN INPUT
*             BIN REQUIRED TO PRODUCE AN OUTPUT PIXEL
*        TITLE
*             A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT
*        ILEVEL
*             THE INTERACTION LEVEL
*        NONEVAL/ERROR/
*             ACCESSED IF THE INPUT IMAGE CONTAINS NO VALID PIXELS
*        ALLREJ/ERROR/
*             ACCESSED IF ALL OUTPUT PIXELS HAVE BEEN REJECTED
*
*CALLS
*        SPRP:
*             BINIT
*        EDRS:
*             GT2DIR,GT2DIW,GTDSCR,PTDSCR,GETPAR
*        STARLINK:
*             RDKEYC,CYDSCR,FRDATA,WRERR
*
*NOTES
*        USES VAX %VAL FACILITY
*
*WRITTEN BY
*        D.S. BERRY
*----------------------------------------------------------------------
*
*
      integer xbin,ybin
      character title(1)*30,cval*1
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('IMAGE',102,.false.,npixa,nlinea,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
* SET DEFAULT DESCRIPTOR ITEM VALUES
*
         title(1)=' '
         invala=-100000
 
*
* OBTAIN DESCRIPTOR VALUES
*
         call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call gtdscr('IMAGE','INVAL','INTEGER',invala,rval,cval,ierr)
 
*
* OBTAIN BINNING FACTORS
*
         call getpar('XBIN','INTEGER',1,1,1.0e08,.false.,xbin,rval,
     :   istat)
         call getpar('YBIN','INTEGER',1,1,1.0e08,.false.,ybin,rval,
     :   istat)
 
*
* OBTAIN MINPIX AND FRACVAL,THE FRACTION OF VALID INPUT PIXELS
* REQUIRED TO PRODUCE A VALID OUTPUT PIXEL,AND INTERACTION LEVEL
*
         fval=0.0
         call getpar('FRACVAL','REAL',1,0.0,1.0,.false.,ival,fval,
     :   istat)
         minpix=1
         uplim=xbin*ybin
         call getpar('MINPIX','INTEGER',1,1,uplim,.false.,minpix,rval
     :    ,istat)
         call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel,rval
     :    ,istat)
 
*
* CALCULATE SIZE OF OUTPUT IMAGE
*
         xbin=jmin0(xbin,npixa)
         ybin=jmin0(ybin,nlinea)
         xb=xbin
         yb=ybin
         npixo=jmax0(int(npixa/xb),1)
         nlino=jmax0(int(nlinea/yb),1)
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npixo,nlino,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY... CALL BINIT TO PERFORM BINNING
*
            call binit(%val(ipa),invala,npixa,nlinea,%val(ipout),npixo
     :       ,nlino,xbin,ybin,minpix,fval,ilevel,ierr)
 
*
* IF BINIT RETURNS AN ERROR CONDITION,GIVE MESSAGE AND END
*
 
            if(ierr.eq.1) then
               call wrerr('NONEVAL')
               goto 99
 
 
            else if(ierr.eq.2) then
               call wrerr('ALLREJ')
               goto 99
 
            endif
 
 
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
            call ptdscr('OUTPUT','INVAL','INTEGER',invala,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','NAXIS1','INTEGER',npixo,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','NAXIS2','INTEGER',nlino,rval,cval
     :       ,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
