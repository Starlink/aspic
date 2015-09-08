      subroutine collap
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COLLAPSE AN IMAGE OR PART OF AN IMAGE,IN A PARTICULAR
*       DIRECTION,TO PRODUCE A '1D' IMAGE.
*
*METHOD
*       OBTAIN INPUT IMAGE AND AN OPTIONAL XY LIST DESCRIBING A
*       POLYGONAL SUB-AREA.IF NO XYLIST IS GIVEN CREATE A DEFAULT XY
*       LIST. OBTAIN WORK SPACE AND EXTRACT X AND Y POSITIONS IF ANY
*       WERE GIVEN.OBTAIN THE SIZE OF THE OUTPUT IMAGE,THEN OBTAIN THE
*       OUTPUT IMAGE.OBTAIN DESCRIPTOR ITEMS FROM INPUT IMAGE,CALL
*       COMPRS TO COMPRESS THE IMAGE INTO A '1D' OUTPUT IMAGE AND
*       UPDATE DESCRIPTOR ITEMS IN THE OUTPUT.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       IMAGE
*               THE INPUT IMAGE TO BE COLLAPSED
*       OUTPUT
*               THE OUTPUT '1D' IMAGE
*       POLY
*               AN OPTIONAL XY LIST DESCRIBING A POLYGONAL SUB-AREA TO
*               BE COLLAPSED.BY DEFAULT THE WHOLE IMAGE IS COLLAPSED.
*       DIR
*               THE DIRECTION IN WHICH THE IMAGE IS TO BE COLLAPSED.
*               EITHER 'X' OR 'Y'.
*       OPER
*               THE METHOD OF OPERATION,EITHER TAKING THE AVERAGE OR
*               THE SUM OF THE INPUT PIXELS TO FORM THE OUTPUT PIXEL
*       FRACVAL
*               THE MINIMUM FRACTION OF VALID PIXELS REQUIRED IN EACH
*               INPUT BIN TO FORM A VALID OUTPUT PIXEL
*       MINPIX
*               THE MINIMUM NUMBER OF VALID PIXELS REQUIRED TO FORM A
*               VALID OUTPUT PIXEL
*       ILEVEL
*               THE INTERACTION LEVEL
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE.
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CONNOT BE OBTAINED.
*       2COMPLEX/ERROR/
*               ACCESSED IF THE POLYGON HAS TOO MANY SIDES TO HANDLE.
*       2FEWSIDES/ERROR/
*               ACCESSED IF THE POLYGON DOES NOT HAVE AT LEAST 3 SIDES.
*       ALLREJ/ERROR/
*               ACCESSED IF ALL OUTPUT PIXELS ARE INVALID
*       NONEVAL/ERROR/
*               ACCESSED IF ALL INPUT PIXELS ARE INVALID
*
*CALLS
*       SPRP:
*               MINMAX,COLLAP
*       EDSR:
*               GT2DIR,GT2DIW,GTDSCR,GTXYLR,EXTLST,PTDSCR,GETCMD
*       STARLINK:
*               GETDYN,WRERR,FRDATA,RDKEYC,CYDSCR
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
      character cval*1,title(1)*30,dir*1
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('IMAGE',102,.false.,npix,nlin,ipin,ierr)
 
*
* IF INPUT IMAGE OBTAINED SUCCESSFULLY,CONTINUE
*
 
      if(ierr.eq.0) then
 
*
* OBTAIN XY LIST DESCRIBING POLYGONAL AREA TO BE COLLAPSED.('NULL'
* VALUE IS ALLOWED)
*
         call gtxylr('POLY',.true.,nitem,len,ipol,ierra)
 
*
* IF POLYGON LIST OR NULL VALUE OBTAINED SUCCESSFULLY,CONTINUE
*
 
         if(ierra.le.1) then
 
*
* IF NO POLYGON WAS GIVEN ('NULL' VALUE RETURNED) SET NOPOLY=1 AND
* SET LENGTH OF DEFAULT XY LIST TO 4
*
            nopoly=0
 
            if(ierra.eq.1) then
               nopoly=1
               len=4
            endif
 
 
*
* GET DYNAMIC MEMORY SPACE X AND Y VALUES
*
            call getdyn('X',204,len,ipx,istx)
            call getdyn('Y',204,len,ipy,isty)
 
*
* IF SPACE NOT AVAILABLE,GIVE MESSAGE AND END
*
 
            if((istx.ne.0).or.(isty.ne.0)) then
               call wrerr('NOSPACE')
               goto 99
 
            endif
 
 
*
* OBTAIN DIRECTION FOR COLLAPSE
*
            idir=1
            call getcmd('DIR','X,Y.',1,idir,dir,lencom,istat)
 
*
* IF A POLYGON WAS SUPPLIED,EXTRACT X AND Y POSITIONS
*
 
            if(nopoly.eq.0) then
               call extlst(%val(ipol),nitem,len,%val(ipx),21,24)
               call extlst(%val(ipol),nitem,len,%val(ipy),25,28)
 
*
* THEN CALL MAXMIN TO OBTAIN EXTREMAL VALUES OF POLYGON IN DIRECTION
* PERPENDICULAR TO COLLAPSE
*
               call maxmin(%val(ipx),%val(ipy),len,idir,amax,amin,
     :         istat)
 
*
* AND SET THE NUMBER OF PIXELS IN THE OUTPUT '1D' IMAGE
*
               pixout=amax-amin
 
               if(int(pixout).ne.pixout) pixout=pixout+1
               npixout=int(pixout)
 
*
* IF NO POLYGON WAS SUPLIED,SET THE NUMBER OF OUTPUT PIXELS TO THE
* NUMBER OF INPUT LINES IF COLLAPSE DIRECTION IS X,OR THE NUMBER OF
* INPUT PIXELS IF COLLAPSE DIRECTION IS Y
*
 
            else
 
               if(idir.eq.1) then
                  npixout=nlin
 
               else
                  npixout=npix
               endif
 
            endif
 
 
*
* IN EITHER CASE OBTAIN OUTPUT IMAGE
*
            call gt2diw('OUTPUT',102,.false.,npixout,1,ipout,ierrb)
 
*
* IF OUTPUT IMAGE OBTAINED SUCCESSFULLY,CONTINUE
*
 
            if(ierrb.eq.0) then
 
*
* OBTAIN SPACE FOR THE TEMPORARY OUTPUT ARRAY TO BE USED IN COMPRS
*
               call getdyn('OUT',204,npixout,ipr,ierrn)
 
               if(ierrn.ne.0) then
                  call wrerr('NOSPACE')
                  goto 99
 
               endif
 
 
*
* SET DEFAULT DESCRIPTOR ITEMS
*
               title(1)=' '
               inval=-32767
               bscale=1.0
               bzero=0.0
 
*
* OBTAIN DESCRIPTOR ITEMS FROM INPUT IMAGE
*
               call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,
     :         title(1),istat)
               call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval
     :          ,istat)
               call gtdscr('IMAGE','BSCALE','REAL',ival,bscale,cval
     :          ,istat)
               call gtdscr('IMAGE','BZERO','REAL',ival,bzero,cval,
     :         istat)
 
*
* OBTAIN THE REQUIRED PARAMETERS FROM ENVOIREMENT
*
               call getpar('MINPIX','INTEGER',1,1.0,1.0e8,.false.,
     :         minpix,rval,istat)
               call getpar('FRACVAL','REAL',1,0.0,1.0,.false.,ival,fval
     :          ,istat)
               call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel
     :          ,rval,istat)
               icom=2
               call getcmd('OPER','AVERAGE,SUM.',1,icom,oper,iln,istat)
 
*
* CALL COMPRS TO COMPRESS THE INPUT IMAGE INTO A SINGLE LINE
*
               call comprs(%val(ipin),npix,nlin,inval,%val(ipx),
     :         %val(ipy),len,idir,nopoly,%val(ipout),minpix,fval,ilevel
     :          ,icom,npixout,bscale,bzero,%val(ipr),ierrc)
 
*
* IF IERRC IDICATES ERROR,GIVE MESSAGE AND END
*
 
               if(ierrc.eq.2) then
                  call wrerr('2COMPLEX')
                  goto 99
 
 
               else if(ierrc.eq.1) then
                  call wrerr('2FEWSIDES')
                  goto 99
 
 
               else if(ierrc.eq.3) then
                  call wrerr('NONEVAL')
                  goto 99
 
 
               else if(ierrc.eq.4) then
                  call wrerr('ALLREJ')
                  goto 99
 
               endif
 
 
*
* OBTAIN OUTPUT TITLE
*
               call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY INPUT IMAGE DESCRIPTOR TO OUTPUT
*
               call cydscr('IMAGE','OUTPUT',istat)
 
*
* UPDATE OUTPUT DESCRIPTOR ITEMS
*
               call ptdscr('OUTPUT','NAXIS1','INTEGER',npixout,rval
     :          ,cval,istat)
               call ptdscr('OUTPUT','NAXIS2','INTEGER',1,rval,cval,
     :         ierr)
               call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),istat)
               call ptdscr('OUTPUT','INVAL','INTEGER',inval,rval,cval
     :          ,istat)
               call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval
     :          ,istat)
               call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,
     :         istat)
            endif
 
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
