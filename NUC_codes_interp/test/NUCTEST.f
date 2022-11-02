        program NUCTEST
            !implicit none 
            PARAMETER (nsize=10)
            real x(nsize)
            real y(nsize)
            real xval, yval
            integer i




            do i = 1, nsize 
                x(i) = real(i)*2.0*acos(-1.0)/real(nsize)
                y(i) = sin(x(i))

            end do

            xval = 2.0*acos(-1.0)*0.42
            
            call interp(nsize, x, y, xval, yval)

            print *,xval,yval, sin(xval), acos(-1.0)

        end program