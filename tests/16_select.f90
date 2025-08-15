! echo: option2
! echo: option3
! echo: option1
! echo: invalid
program main
    implicit none
    integer :: dummy
    character(len=20) :: option
    do dummy = 1, 4
        read(*, *) option
        select case (option)
        case ("option1")
            print *, "You selected option 1"
        case ("option2")
            print *, "You selected option 2"
        case ("option3")
            print *, "You selected option 3"
        case default
            print *, "Invalid option"
        end select
    end do
end program main