      function per_simul(n,a,x,eps,indic,nrc)
      implicit  double precision(a-h, o-z)
      double precision  a, x, eps, per_simul
      dimension irow(61),jcol(61),jord(61),y(61),a(nrc,nrc),x(n)
 
*
      max = n
 
      if ( indic.ge.0 )   max = n + 1
 
*
*     .....IS N LARGER THAN 61 .....
 
      if ( n.le.61 )   go to 5
      call wruser('Too many parameters found',istat)
      per_simul = 0.
      return
 
*
*     ..... BEGIN ELIMINATION PROCEDURE .....
5     continue
      deter = 1.
 
      do  k = 1, n
         km1 = k - 1
 
*
*     ..... SEARCH FOR THE PIVOT ELEMENT .....
         pivot = 0.
 
         do i = 1, n
 
            do  j = 1, n
 
*     ..... SCAN IROW AND JCOL ARRAYS FOR INVALID PIVOT SUBSCRIPTS
*     ..... SCAN IROW AND JCOL ARRAYS FOR INVALID PIVOT SUBSCRIPTS .....
 
               if ( k.eq.1 )   go to 9
 
               do iscan = 1, km1
 
                  do  jscan = 1, km1
 
                     if ( i.eq.irow(iscan) )   go to 11
 
                     if ( j.eq.jcol(jscan) )   go to 11
                  end do
 
               end do
 
9              continue
 
               if (dabs(a(i,j)).le.dabs(pivot) )   go to 11
               pivot = a(i,j)
               irow(k) = i
               jcol(k) = j
11             continue
            end do
 
         end do
 
 
*
*     ..... INSURE THAT SELECTED PIVOT IS LARGER THAN EPS .....
 
         if ( dabs(pivot).gt.eps )   go to 13
         per_simul = 0.
         return
 
*
*     ..... UPDATE THE DETERMINANT VALUE .....
13       continue
         irowk = irow(k)
         jcolk = jcol(k)
 
*     DETER = DETER*PIVOT
*
*     ..... NORMALIZE PIVOT ROW ELEMENTS .....
 
         do  j = 1, max
            a(irowk,j) = a(irowk,j)/pivot
         end do
 
 
*
*     ..... CARRY OUT ELIMINATION AND DEVELOP INVERSE .....
         a(irowk,jcolk) = 1./pivot
 
         do  i = 1, n
            aijck = a(i,jcolk)
 
            if ( i.eq.irowk )   go to 18
            a(i,jcolk) = - aijck/pivot
 
            do  j = 1, max
 
               if ( j.ne.jcolk )   a(i,j) = a(i,j) - aijck*a(irowk,j)
            end do
 
18          continue
         end do
 
      end do
 
 
*
*     ..... ORDER SOLUTION VALUES (IF ANY) AND CREATE JORD ARRAY .....
 
      do i = 1, n
         irowi = irow(i)
         jcoli = jcol(i)
         jord(irowi) = jcoli
 
         if ( indic.ge.0 )   x(jcoli) = a(irowi,max)
      end do
 
 
*
*     ..... ADJUST SIGN OF DETERMINANT
*     ..... ADJUST SIGN OF DETERMINANT .....
      intch = 0
      nm1 = n - 1
 
      do i = 1, nm1
         ip1 = i + 1
 
         do j = ip1, n
 
            if ( jord(j).ge.jord(i))   go to 22
            jtemp = jord(j)
            jord(j) = jord(i)
            jord(i) = jtemp
            intch = intch + 1
22          continue
         end do
 
      end do
 
 
      if ( intch/2*2.ne.intch )   deter = - deter
 
*
*     ..... IF INDIC IS POSITIVE RETURN WITH RESULTS .....
 
      if ( indic.le.0 )   go to 26
      per_simul = deter
      return
 
*
*     ..... IF INDIC IS NEGATIVE OR ZERO, UNSCRAMBLE THE INVERSE
*        FIRST BY ROWS .....
26    continue
 
      do j = 1, n
 
         do i = 1, n
            irowi = irow(i)
            jcoli = jcol(i)
            y(jcoli) = a(irowi,j)
         end do
 
 
         do i = 1, n
            a(i,j) = y(i)
         end do
 
      end do
 
 
*     ..... THEN BY COLUMNS .....
 
      do i = 1, n
 
         do j = 1, n
            irowj = irow(j)
            jcolj = jcol(j)
            y(irowj) = a(i,jcolj)
         end do
 
 
         do j = 1, n
            a(i,j) = y(j)
         end do
 
      end do
 
 
*
*     ..... RETURN FOR INDIC NEGATIVE OR ZERO .....
      per_simul = deter
 
      end
 
 
 
