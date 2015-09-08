      subroutine args_lvt (n,ioff,ion,iflip)

*+  Change entry 'n' of the video transformation table.
*   First of all any bits set in 'ioff' are switched off, then any in
*   'ion' are switched on and finally any in 'iflip' are inverted.
*   (cf 'args_vsr')

      integer n,maskn,i
      integer*2 ioff(2),ion(2),iflip(2),ilvt(2,'0000'X:'00FF'X)
      save ilvt

*   perform bit manipulations
      maskn = iand (n,'00FF'X)
      do i = 1,2
          ilvt(i,maskn)=ieor(ior(iand(ilvt(i,maskn),not(ioff(i))),
     :    ion(i)),iflip(i))
      enddo

*   send to ARGS
      call args_flush (4)
      call args_put1 ('1800'X)
      call args_put1 (maskn)
      call args_putn (ilvt(1,maskn),2)
      call srsend

      end
