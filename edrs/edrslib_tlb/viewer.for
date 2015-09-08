      subroutine viewer
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO WRITE OUT A SQUARE SECTION OF AN IMAGE DEFINED BY CENTRE
*       (X0,Y0) AND SIDE ISIZE,TO A FILE VIEW.LIS TO BE PRINTED ON
*       THE PRINTER.
*
*METHOD
*       GET THE INPUT IMAGE AND REQUIRED DESCRIPTOR ITEMS.GET X0,Y0
*       AND ISIZE.CALL VIEWIT TO WRITE OUT THE VALUES OF THE PIXELS
*       CONTAINED WITHIN THE INTERSECTION OF THE SQUARE AREA AND THE
*       IMAGE
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       IMAGE
*              THE NAME OF THE INPUT IMAGE
*       X0
*              THE X CO-ORDINATE OF THE CENTRE OF THE SQUARE AREA
*       Y0
*              THE Y CO-ORDINATE OF THE CENTRE OF THE SQUARE AREA
*       ISIZE
*              THE LENGTH OF A SIDE OF THE SQUARE AREA
*       NONEIN/ERROR/
*              ACCESSED IF THE SQUARE AREA DOES NOT INTERSECT THE IMAGE
*
*CALLS
*       SPRP:
*              VIEWIT
*       EDRS:
*              GT2DIR,GTDSCR,GETPAR
*       STARLINK:
*              FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
      character title*30,cval*1
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('IMAGE',102,.false.,npix,nlin,ip,ierra)
 
*
* IF INPUT IMAGE OBTAINED SUCCESSFULLY,CONTINUE
*
 
      if(ierra.eq.0) then
 
*
* SET DEFAULT DESCRIPTOR ITEMS
*
         inval=-100000
         bscale=1.0
         bzero=0.0
         title=' '
 
*
* OBTAIN INPUT DESCRIPTOR ITEMS
*
         call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title,istat)
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,istat)
         call gtdscr('IMAGE','BSCALE','REAL',ival,bscale,cval,istat)
         call gtdscr('IMAGE','BZERO','REAL',ival,bzero,cval,istat)
 
*
* OBTAIN CO-ORDINATES OF CENTRE AND SIZE OF THE SQUARE AREA TO BE
* PRINTED
*
         ix=0
         iy=0
         isize=11
         call getpar('X0','INTEGER',1,1.0,1.0e08,.false.,ix,rval,istat)
         call getpar('Y0','INTEGER',1,1.0,1.0e08,.false.,iy,rval,istat)
         call getpar('ISIZE','INTEGER',1,1.0,1.0e08,.true.,isize,rval
     :    ,istat)
 
*
* CALL VIEWIT TO PRINT OUT THE PIXEL VALUES CONTAINED WITHIN THE AREA
*
         call viewit(%val(ip),npix,nlin,inval,bscale,bzero,ix,iy,isize
     :    ,title,ierrb)
 
*
* IF IERRB INDICATES THAT GIVEN AREA DID NOT INTERSECT IMAGE,GIVE
* MESSAGE AND END
*
 
         if(ierrb.eq.1) then
            call wrerr('NONEIN')
         endif
 
 
*
* FREE DATA AREAS AND RETURN
*
      endif
 
      call frdata(' ',istat)
      return
 
      end
 
 
 
