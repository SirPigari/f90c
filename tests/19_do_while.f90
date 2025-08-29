program test_do_while
    implicit none
    integer :: a, b, tmp
    a = 0
    b = 1
    do while (b < 100)
        tmp = a + b
        a = b
        b = tmp
        print *, b
    end do
end program test_do_while
