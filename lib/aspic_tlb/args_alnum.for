      subroutine args_alnum (in , out , lout)

*+  Remove all non alphanumeric characters from string

      integer lout , lin
      character in * ( * ) , out * ( * ) , inchar * 1

      lout = 0
      out = ' '

      do lin = 1 , len(in)
          inchar = in(lin:lin)
          if (('0' .le. inchar .and. inchar .le. '9') .or. 
     :        ('A' .le. inchar .and. inchar .le. 'Z') .or. 
     :        ('a' .le. inchar .and. inchar .le. 'z')) then
              if (lout .lt. len(out)) then
                  lout = lout + 1
                  out(lout:lout) = inchar
              endif
          endif
      enddo

      end

