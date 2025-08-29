program main
    implicit none
    block
        integer :: i
        do i = 1, 10
            print *, "Inside block: ", i
        end do
    end block
    print *, "Exited block\ni: ", i
end program main