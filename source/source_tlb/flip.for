 
*+
*
*      STARLINK ENVIRONMENT PROGRAM   *** FLIP ***
*
*     WRITTEN BY K F HARTLEY AT RGO ON 6/1/81
*     Modified to ues REAL arrays by KFH on 7/1/85
*
*      VERSION #1
*
*      IT TAKES A 2D STARLINK IMAGE AND FLIPS IT ABOUT
*      HORIZONTAL OR VERTICAL AXES .
*
*      THE PARAMETERS ARE :
*         INPUT : 2D STARLINK INPUT FRAME
*         OUTPUT : 2D STARLINK OUTPUT FRAME
*         MODE : THE TYPE OF TRANSFORMATION
*              : H FOR LEFT TO RIGHT FLIPPING
*              : V FOR TOP TO BOTTOM FLIPPING
*              : B FOR BOTH
*
*-
      include 'INTERIM(ERRPAR)'
      include 'INTERIM(FMTPAR)'
      integer*4 pin,pout,ax(2),status
 
*
*       THE USER CAN TYPE IN UP TO 20 CHARCTERS (INTO TYPE)
*       BUT ONLY THE FIRST IS USED (IN MODE)
*
      character*1 mode
      character*20 type
 
*
*      FIRST GET THE INPUT AN OUTPUT FRAMES
*
      call rdimag('INPUT',fmt_r,2,ax,i,pin,status)
 
      if (status.ne.0.or.i.ne.2) then
         call wrerr('HELL',status)
         call exit
      end if
 
      call wrimag('OUTPUT',fmt_r,ax,2,pout,status)
 
      if (status.ne.0) then
         call wrerr('HELLOUT',status)
         call exit
      end if
 
 
*
*      THEN PROMPT THE USER AND GET THE MODE
*
      call wruser('H FOR LEFT/RIGHT , V FOR TOP/BOTTOM , B FOR BOTH'
     :,status)
100   call rdkeyc('MODE',.true.,1,type,i,status)
      mode=type(1:1)
      call str$upcase(mode,mode)
 
      if (mode.ne.'V'.and.mode.ne.'H'.and.mode.ne.'B') then
         call wruser('MUST BE H OR V OR B',status)
         call cnpar('MODE',status)
         go to 100
 
      end if
 
 
      if (status.ne.0) then
         call wrerr('HELLCHAR',status)
         call exit
      end if
 
 
*
*      FINALLY , DO THE WORK
*
      call flip(%val(pin),%val(pout),ax(1),ax(2),mode)
 
*
*     TIDY UP AND GO HOME
*
      call frdata(' ',status)
      call exit
 
      end
 
 
 
      subroutine flip(in,out,n,m,mode)
 
*
*      FLIPS AN ARRAY (IN) TO GIVE AN OUTPUT ARRAY (OUT) ,
*      BOTH OF SIZE N BY M
*      MODE ( CHAR*1) IS EITHER H OR V OR B
*      CORRESPONDING TO LEFT/RIGHT , TOP/BOTTOM AND BOTH
*
      real in(n,m),out(n,m)
      character*1 mode
 
*
*      EACH OPTION HAS IT'S OWN CODING
*
*      FIRST "HORIZONTAL"
*
 
      if (mode.eq.'H') then
 
         do 200 j=1,m
 
            do 100 i=1,n
               k=n+1-i
               out(k,j)=in(i,j)
100         continue
 
200      continue
 
      end if
 
 
*
*      THEN VERTICAL
*
 
      if (mode.eq.'V') then
 
         do 400 j=1,m
            k=m+1-j
 
            do 300 i=1,n
               out(i,k)=in(i,j)
300         continue
 
400      continue
 
      end if
 
 
*
*      THEN BOTH TOGETHER
*
 
      if (mode.eq.'B') then
 
         do 600 j=1,m
            k=m+1-j
 
            do 500 i=1,n
               l=n+1-i
               out(l,k)=in(i,j)
500         continue
 
600      continue
 
      end if
 
      return
 
      end
 
 
 
