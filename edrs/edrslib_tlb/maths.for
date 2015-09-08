      subroutine maths
 
*
      character title(1)*30,cval*1,exprs(1)*70,oper(70)*6,imgid(70)
     : *2,varid(70)*1,errpar(5)*8,errmsg*70
      integer ip(26),inval(26),npix(26),nlines(26),imp(70),varp(70)
     : ,opcode(70)
      real var(26),con(70),scale(26),zero(26)
      logical parith
 
*
* SET UP NAMES OF ERROR PARAMETERS
*
      data errpar/'BADCHAR','BADNUM','BADOPER','BADBRKTS','BADARGNO'/

*
* PARITH INDICATES IF USER IS IS ONLY INTERESTED IN THE VALUE WRITTEN TO
* THE OUTPUT PARAMETER "VALUE", AND DOES NOT REQUIRE AN OUTPUT IMAGE.
* THE VALUE OF OUTPUT PIXEL (1,1) IS WRITTEN TO "VALUE" FOR USE BY OTHER
* PROGRAMS
*
      parith=.false.
 
*
* OBTAIN FORMULA FROM ENVIRONMENT
*
33    exprs(1)=' '
      call rdkeyc('FORMULA',.false.,1,exprs,nval,istat)
 
*
* TRANSLATE IT INTO REVERSE POLISH NOTATION
*
      call algtrp(exprs,oper,noper,imgid,nimg,imp,varid,nvar,varp,con
     : ,ncon,errmsg,ierr)
 
*
* IF AN ERROR WAS DETECTED IN THE FORMULA, GIVE APPROPRIATE
* ERROR MESSAGE AND RETURN FOR A NEW FORMULA
*
 
      if(ierr.ne.0) then
         call wrerr(errpar(ierr))
         call wruser(exprs(1),istat)
         call wruser(errmsg,istat)
         call cnpar('FORMULA',istat)
         go to 33
 
      endif
 
 
*
* IF AT LEAST ONE IMAGE IS REFERENCED IN THE FORMULA, OBTAIN
* THE REQUIRED IMAGES
*
 
      if(nimg.ge.1) then
         nx=1000000
         ny=1000000
 
         do 14 i=1,nimg
            call gt2dir(imgid(i),102,.false.,npix(i),nlines(i),ip(i)
     :       ,ierr)
 
*
* IF ANY IMAGE WAS NOT OBTAINED, ABORT
*
 
            if(ierr.ne.0) go to 99
 
*
* THE OUTPUT IMAGE SIZE IS THE AREA COMMON TO ALL INPUT IMAGES
*
            nx=min(nx,npix(i))
            ny=min(ny,nlines(i))
 
*
* OBTAIN THE REQUIRED DESCRIPTOR ITEMS FOR EACH IMAGE
*
            inval(i)=-100000
            scale(i)=1.0
            zero(i)=0.0
            call gtdscr(imgid(i),'INVAL','INTEGER',inval(i),rval,cval
     :       ,ierr)
            call gtdscr(imgid(i),'BSCALE','REAL',ival,scale(i),cval
     :       ,ierr)
            call gtdscr(imgid(i),'BZERO','REAL',ival,zero(i),cval,ierr)
14       continue
 
 
*
* OBTAIN WORKSPACE
*
         call getdyn('STACK',102,nx*ny*nimg,ipstk,ierr1)
         call getdyn('WORK',204,nx*ny,ipwk,ierr2)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
         if(max(ierr1,ierr2).ne.0) then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
 
*
* CALL IMGSTK TO PLACE THE COMMON AREA OF EACH IMAGE IN TURN INTO
* A SINGLE 3-D IMAGE STACK
*
 
         do 501 i=1,nimg
            call imgstk(%val(ip(i)),npix(i),nlines(i),inval(i),i,
     :      %val(ipstk),nx,ny)
501      continue
 
 
*
* IF THERE ARE NO IMAGES REFERENCED, OBTAIN THE REQUIRED OUTPUT
* IMAGE SIZE
*
 
      else
         nx=1
         ny=1
         call getpar('NX','INTEGER',1,1.0,1.0e6,.true.,nx,rval,ierr)
         call getpar('NY','INTEGER',1,1.0,1.0e6,.true.,ny,rval,ierr)
 
*
* OBTAIN REQUIRED WORKSPACE (IMAGE STACK IS A DUMMY 1 PIXEL IMAGE..
* IT IS NOT REFERENCED)
*
         call getdyn('STACK',102,1,ipstk,ierr1)
         call getdyn('WORK',204,nx*ny,ipwk,ierr2)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
         if(max(ierr1,ierr2).ne.0) then
            call wrerr(nospace)
            go to 99
 
         endif
 
*
* IF OUTPUT IMAGE IS CONTAINS ONLY ONE PIXEL, USER IS PROBABLY JUST
* INTERESTED IN THE VALUE OF THE EXPRESSION FOR USE AS INPUT TO ANOTHER
* PROGRAM. THE VALUE OF PIXEL (1,1) IS WRITTEN TO AN OUTPUT PARAMETER
* WHICH CAN BE USED AS INPUT TO OTHER PROGRAMS, AND USER IS GIVEN THE
* OPTION OF ENTERING A NULL OUTPUT IMAGE NAME
*
         if(nx*ny.eq.1) parith=.true.

      endif
 
 
*
* OBTAIN VALUES FOR ANY VARIABLES REFERENCED
*
 
      do 38 i=1,nvar
         var(i)=0.0
         call getpar(varid(i),'REAL',1,-1e20,+1e20,.false.,ival,var(i)
     :    ,ierr)
38    continue
 
 
*
* OBTAIN THE OUTPUT IMAGE
*
      call gt2diw('OUTPUT',102,parith,nx,ny,ipout,ierr)
 
*
* IF IT WAS THOUGHT THAT THE USER MAY ONLY BE INTERESTED IN THE 
* OUTPUT PARAMETER VALUE, BUT A GOOD OUTPUT IMAGE WAS GIVEN, THEN
* THE USER IS OBVIOUSLY INTERESTED IN THE IMAGE AS WELL AS THE PARAMETER
*
      if(ierr.eq.0.and.parith) parith=.false.

*
* IF USER DOES NOT REQUIRE AN OUTPUT IMAGE, GET WORKSPACE TO HOLD A
* DUMMY OUTPUT IMAGE
*
      if(parith) call getdyn('DUMOUT',102,nx*ny,ipout,ierr)

*
* IF NEITHER OUTPUT NOR WORKSPACE WAS OBTAINED, ABORT
*
 
      if(ierr.ne.0) go to 99
 
*
* SET THE INVALID PIXEL FLAG FOR OUTPUT
*
 
      if(nimg.ge.1) then
         inval2=inval(1)
 
         if(abs(inval2).gt.32767)inval2=-32767
 
      else
         inval2=-32767
      endif
 
 
*
* CALL IMGMTH TO PERFORM THE SPECIFIED MATHEMATICAL OPERATION
*
      call imgmth(%val(ipstk),scale,zero,inval,nx,ny,oper,noper,imp
     : ,var,varp,con,%val(ipout),inval2,scale2,zero2,opcode,%val(ipwk)
     :  ,ierr)
 
      if(ierr.ne.0)then
         call wrerr('BADMATHS')
         go to 99
 
      endif
 
 
*
* IF THERE IS AN INPUT IMAGE, COPY DESCRIPTOR ITEMS FROM THE
* FIRST INPUT IMAGE TO THE OUTPUT AND OBTAIN THE DEFAULT OUTPUT
* TITLE
*
 
      if(nimg.ge.1) then
         call cydscr(imgid(1),'OUTPUT',istat)
         call gtdscr(imgid(1),'TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
 
      else
         title(1)=' '
      endif
 
 
*
* IF AN OUTPUT IMAGE IS REQUIRED, UPDATE OUTPUT DESCRIPTORS
*
      if(.not.parith) then
         call rdkeyc('TITLE',.true.,1,title,nval,istat)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',nx,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',ny,rval,cval,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1),
     :                ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',inval2,rval,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,scale2,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,zero2,cval,ierr)
      endif
 
*
* WRITE VALUE OF PIXEL (1,1) TO OUTPUT PARAMETER "VALUE"
*
      call wrkeyr('VALUE',%val(ipwk),1,ierr)

*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
