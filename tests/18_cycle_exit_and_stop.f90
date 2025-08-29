program main
    implicit none
    integer :: i, j
    do i = 1, 3
        do j = 1, 3
            if (j == 2) cycle  ! skip to next i when j = 2
            print *, "i = ", i, " j = ", j
        end do
    end do
end program main