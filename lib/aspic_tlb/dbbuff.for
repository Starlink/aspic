*+  Stored current header record and data record
      integer stidmx,strpos(MAXREC),stid
      logical newpos,newrec
      character strec*(RECLEN)
      common /dbbuf1/stidmx,strpos,stid,newpos,newrec
      common /dbbuf2/strec
      save /dbbuf1/,/dbbuf2/
