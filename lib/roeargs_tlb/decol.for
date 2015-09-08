      function args_decol (col)

*+  Decode 'col' to give an integer in the range 0-7 depending on its
*   first character :-
*     R     -     red        1
*     G     -     green      2
*     Y     -     yellow     3
*     B     -     blue       4
*     M     -     magenta    5
*     C     -     cyan       6
*     W     -     white      7
*     ELSE  -     black      0
*
*   from W.F.Lupton/RGO/Nov81
*-

      integer args_decol
      character col

      args_decol = 0
      if (col.eq.'R') args_decol = '0001'X
      if (col.eq.'G') args_decol = '0002'X
      if (col.eq.'Y') args_decol = '0003'X
      if (col.eq.'B') args_decol = '0004'X
      if (col.eq.'M') args_decol = '0005'X
      if (col.eq.'C') args_decol = '0006'X
      if (col.eq.'W') args_decol = '0007'X

      end
