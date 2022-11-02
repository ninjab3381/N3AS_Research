program kja

    real, dimension(3,3) :: A
    real, dimension(3) :: v
    real, dimension(3) :: w

    integer :: i
    integer :: N

    N = size(v)

    w = 0.0          !! clear whole vector          

    DO i = 1, N
    w = w + A( :, i ) * v(i)
    END DO

end program
