program ragged
    type ragged_array
        integer,allocatable::v(:)
    end type ragged_array


    type(ragged_array),allocatable::r(:)
    allocate(r(3))
    allocate(r(1)%v(5))
    allocate(r(2)%v(10))
    allocate(r(3)%v(15))

    r(1)%v = (/ 1,2,3,4,5 /)

    write(*, *) r(1)%v

end program ragged
