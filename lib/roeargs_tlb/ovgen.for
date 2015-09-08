      subroutine args_ovgen (col)
*+
*   ARGS_OVGEN
*
*   enable/disable global overlays of colour 'col' ie enable 0, 1, 2 or 3
*   overlay outputs depending on first character of 'col' (see 'args_decol').
*
*   was args_ovcg in W.F.Lupton's package from RGO.
*
*   Called:
*     args_vsr: ARGSLIB
*     args_decol: ROEARGS
*-

      integer args_decol
      integer*2 ioff(3),ion(3),iflip(3)
      character col
      data ioff,ion,iflip/2*0,'0007'X,6*0/

      ion(3) = args_decol (col)

      call args_vsr (ioff,ion,iflip)

      end
