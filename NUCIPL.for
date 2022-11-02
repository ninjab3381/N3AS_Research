
        subroutine interp(nsize,x,y,xval,yval)


!-------linkages.

!       called by - []
!       calls     - [subroutine] polint
!                   [function]   indpolint

!-------remarks.

!       Subroutine to interpolate on input arrays


        implicit none


!-------throughput variables.

        integer, parameter nsize !size of input arrays
        real, dimension(nsize) x !input x values
        real, dimension(nsize) y !input y values
        real xval !input x value
        real yval !interpolated y value


!-------local variables.

        integer, parameter ordpolint = 5
        integer j, nintpl
        real errory !error used in evaluating polint


!-------procedure.


!-------interpolated values.


        j = indpolint(nsize,x,xval,ordpolint)
        nintpl = ordpolint + 1
        call polint(nintpl,x(j:j+ordpolint),y(j:j+ordpolint),
     a              xval,yval,errory)


        return


        end subroutine interp


!-----------------------------------------------------------

        subroutine polint(nintpl,xa,ya,x,y,dy)

!-------linkages.

!       called by - [subroutine] interp
!       calls - none


!-------remarks.

!       Performs polynomial interpolation using input points.
!       Maximum order: 9


        implicit none


!-------throughput variables.

        integer, parameter nintpl !size of input arrays
        real xa(nsize) !input x array
        real ya(nsize) !input y array
        real x !input x value
        real y !output y value
        real dy !error in y


!-------local variables.

        integer i,m,ns,n !indicies
        real dif, dift !differences between x and xa(i)
        real ho, hp, w, den !quantities for c and d

        real c(10) !coefficients for interp
        real d(10) !coefficients for interp


!-------procedure.

        n = nintpl

        ns = 1
        dif = abs(x - xa(1))
        do i=1,n
          dift = abs(x - xa(i))
          if (dift.lt.dif) then
            ns = i
            dif = dift
          end if
          c(i) = ya(i)
          d(i) = ya(i)
        end do

        y = ya(ns)
        ns = ns - 1
        do m=1,n-1
          do i=1,n-m
            ho = xa(i) - x
            hp = xa(i+m) - x
            w = c(i+1) - d(i)
            den = ho - hp
            den = w/den
            c(i) = ho*den
            d(i) = hp*den
          end do
          if ((2*ns).lt.(n-m)) then
            dy = c(ns + 1)
          else
            dy = d(ns)
            ns = ns - 1
          end if
          y = y + dy
        end do


        return


        end subroutine polint


!-----------------------------------------------------------

        integer function indpolint(nsize,xa,x,ord)
       

!-------linkages.

!       called by - [subroutine] interp
!       calls     - [none]


!-------remarks.

!       Finds the index needed for the interpolating arrays of polint.
!       Uses bisection method.


        implicit none


!-------throughput variables.

        integer, parameter nsize !size of input array
        real  xa(nsize) !abscissas
        real x !input x value
        integer ord !order of interpolating polynomial


!-------local variables.

        integer i, n !indices
        integer jl, jm, ju !indicies


!-------procedure

        n = nsize

        jl = 0
        ju = n + 1

        do i=1,n
          if ((ju-jl).eq.1) exit
          jm = (ju + jl)/2
          if ((xa(n).ge.xa(1)).eqv.(x.ge.xa(jm))) then
            jl = jm
          else
            ju = jm
          end if
        end do !i

        if (jl.lt.(1 + ord/2)) then
          indpolint = 1
        else if (jl.gt.(n - 1 - ord/2)) then
          indpolint = n - ord
        else
          indpolint = jl - ord/2
        end if       


        return


        end function indpolint
