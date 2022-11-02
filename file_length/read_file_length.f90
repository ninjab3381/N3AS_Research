program read_file_length
    integer i 
    real, dimension(:), allocatable :: arr
    real rand 
    integer ierror, nlines

    open(55,file = "num.dat", status = 'old')
    nlines = 0
    do 
        read(55, *, iostat=ierror) rand
        if (ierror.ne.0) exit
        nlines = nlines + 1
    end do

    rewind(55)

    allocate(arr(nlines))

    do i=1, nlines
        read(55,*) arr(i)
    end do

    do i = 1, nlines
        write(*,*) i, arr(i)
    end do

end program

