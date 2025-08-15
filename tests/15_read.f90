! echo: -6969
! echo: "   HELLO"
! echo: "   WORLD"
program main
    implicit none

    character(len=20) :: hello, world
    integer :: t
    read(*, *) t
    t = ABS(t)

    print *, "Absolute value of t: ", t

    read(*, *) hello
    read(*, *) world
    print *, "You entered: ", trim(adjustl(hello)), " and ", trim(adjustl(world))
end program main
