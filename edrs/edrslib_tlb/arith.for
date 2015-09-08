      subroutine arith
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PERFORM 4 FUNCTION ARITHMETIC ON IMAGES USING CONSTANTS
*
*METHOD
*       OBTAIN IMAGE, FUNCTION REQUIRED AND CONSTANT. MODIFY IMAGE
*       SCALE AND ZERO LEVEL ACCORDINGLY.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       IMAGE
*               INPUT/OUTPUT IMAGE FRAME
*       FUNCTION
*               REQUIRED ARITHMETIC FUNCTION
*       CONSTANT
*               CONSTANT TO BE USED IN ARITHMETIC
*
*CALLS
*       THIS PACKAGE:
*               GETCMD,GTDSCR,GETPAR,GT2DIR,PTDSCR.
*       STARLINK:
*               FRDATA
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,fun*8
 
*
* OBTAIN IMAGE FRAME
*
      call gt2dir('IMAGE',102,.false.,npix,nlines,ipim,ierrim)
 
      if(ierrim.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY... OBTAIN SCALE FACTOR AND ZERO LEVEL
* FROM DESCRIPTOR
*
         scale=1.0
         zero=0.0
         call gtdscr('IMAGE','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('IMAGE','BZERO','REAL',ival,zero,cval,ierr)
 
*
* OBTAIN REQUIRED ARITHMETIC FUNCTION FROM ENVIRONMENT
*
         ifun=1
         call getcmd('FUNCTION','ADD,SUBTRACT,MULTIPLY,DIVIDE,+,-,*,/.'
     :   ,1,ifun,fun,lfun,ierr)
 
*
* TREAT EACH FUNCTION SEPARATELY: OBTAINING CONSTANT FROM ENVIRONMENT
* AND MODIFYING THE IMAGE SCALE AND ZERO ACCORDINGLY
*
 
         if((fun.eq.'ADD').or.(fun.eq.'+')) then
            const=0.0
            call getpar('CONSTANT','REAL',1,-1.0e20,1.0e20,.true.,ival
     :       ,const,ierr)
            zero=zero+const
 
         else if((fun.eq.'SUBTRACT').or.(fun.eq.'-')) then
            const=0.0
            call getpar('CONSTANT','REAL',1,-1.0e20,1.0e20,.true.,ival
     :       ,const,ierr)
            zero=zero-const
 
         else if ((fun.eq.'MULTIPLY').or.(fun.eq.'*')) then
            const=1.0
            call getpar('CONSTANT','REAL',1,-1.0e20,1.0e20,.true.,ival
     :       ,const,ierr)
            scale=scale*const
            zero=zero*const
 
         else if((fun.eq.'DIVIDE').or.(fun.eq.'/')) then
            const=1.0
            call getpar('CONSTANT','REAL',1,1.0e-20,-1.0e-20,.true.
     :       ,ival,const,ierr)
            scale=scale/const
            zero=zero/const
         endif
 
 
*
* PUT MODIFIED SCALE AND ZERO BACK INTO DESCRIPTOR
*
         call ptdscr('IMAGE','BSCALE','REAL',ival,scale,cval,ierr)
         call ptdscr('IMAGE','BZERO','REAL',ival,zero,cval,ierr)
      endif
 
 
*
* FREE IMAGE FRAME AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
