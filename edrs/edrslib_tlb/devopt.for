      subroutine devopt(opts)
 
*
* SET UP THE NUMBER OF DIFFERENT DEVICES TO BE HANDLED
*
      parameter (ndev=9)
      character opts*(*),dev(ndev)*15
      integer len(ndev)
 
*
* SET UP THE NAMES AND NAME LENGTHS OF THE PLOTTING DEVICES
* WHICH GKS CURRENTLY RECOGNIZES IN THE ORDER OF THEIR WORK
* STATION TYPE
*
      data  ((dev(i),len(i)),i=1,ndev)/'ARGS           ',  4,
     :'TEKTRONIX4010  ', 13,'GOC            ',  3,'TNX4010        '
     : ,  7,'VERSATEC       ',  8,'HP7221         ',  6,
     : 'PRINTRONIX     ', 10,'SIGMAT5674     ', 10,'CALCOMP81      '
     : ,  9/
 
*
* CONCATANATE THESE OPTIONS WITH COMMAS BETWEEN EACH ONE AND
* A FULL STOP AT THE END
*
      i=1
 
      do 10 n=1,ndev
         j=i+len(n)-1
         opts(i:j)=dev(n)(1:len(n))
 
         if(n.ne.ndev) then
            opts(j+1:j+1)=','
 
         else
            opts(j+1:j+1)='.'
         endif
 
         i=j+2
10    continue
 
 
      end
 
 
 
