      subroutine args_ovb (n,blink)

*+  Set/reset overlay blink bit in entry 'n' of lookup table, to give
*   blinking overlay. Overlay blink must have been set globally.

      integer n,blink,maskn
      integer*2 ioff(2),ion(2),iflip(2)
      data ioff,ion,iflip/6*0/

      maskn = iand (n,'00FF'X)
      ion(1) = ior (maskn,ishft (maskn,8))
*   the following seems necessary to avoid overflow
      if (iand (blink,'0001'X).eq.0) then
          ion(2) = '0000'X
      else
          ion(2) = '8000'X
      endif

      call args_lvt (n,ioff,ion,iflip)

      end
