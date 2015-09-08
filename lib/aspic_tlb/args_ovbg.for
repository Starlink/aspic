      subroutine args_ovbg (col)

*+  enable/disable global overlay blink of colour 'col' (ie enable 0, 1, 2 or
*   3 overlay blink outputs depending on first character of 'col' (see
*   'args_decol).

      integer args_decol,dcol
      integer*2 ioff(3),ion(3),iflip(3)
      character col
      data ioff,ion,iflip/2*0,'0038'X,6*0/

      dcol = args_decol (col)
      if (dcol.eq.0) then
          ion(2) = '0000'X
      else
          ion(2) = '0004'X
      endif
      ion(3) = ishft (dcol,3)

      call args_vsr (ioff,ion,iflip)

      end
