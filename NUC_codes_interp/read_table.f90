program read_table

    !https://stackoverflow.com/questions/8892741/listing-the-contents-of-a-directory-in-fortran

    real :: r
    integer :: n,reason,NstationFiles,iStation, n1
    character(LEN=100), dimension(:), allocatable :: stationFileNames


    !https://stackoverflow.com/questions/18316592/multidimensional-array-with-different-lengths
    type ragged_array
        real,allocatable::v(:)
    end type ragged_array
    type(ragged_array),allocatable::S(:), E(:)

  
    ! get the files
    call system('ls ./S_v_E_files > fileContents.txt')
    open(31,FILE='fileContents.txt',action="read")
    !how many
    n = 0
    do
     read(31,FMT='(a)',iostat=reason) r
     if (reason/=0) EXIT
     n = n+1
    end do
    NstationFiles = i
    write(verb,'(a,I0)') "Number of station files: " , NstationFiles
    allocate(stationFileNames(NstationFiles))
    rewind(31)

    do i = 1,NstationFiles
     read(31,'(a)') stationFileNames(i)
        
    end do

    do i = 1, NstationFiles
        OPEN(1, FILE = stationFileNames(i), FORM = "unformatted")
        read(31,FMT='(a)',iostat=reason) r
        if (reason/=0) EXIT
        n = n+1

        allocate(S(n), E(n))



    
    

    allocate(ra(1)%v(5))
    allocate(ra(2)%v(10))



    real, dimension

    read(*,*)

end program read_table
