program main
    implicit none

    integer :: t
    t = -6684
    t = ABS(t)

    print *, "Absolute value of t: ", t

    character(len=20) :: str
    read(*, *) str
    print *, "You entered: ", trim(adjustl(str))
end program main
