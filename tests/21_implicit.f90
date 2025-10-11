program test_implicit
    implicit integer (i, j), real (x, y)
    integer :: i
    real :: x
    i = 42
    j = 98
    x = 3.14
    y = 1
    print *, i, j
    print *, x, y
end program test_implicit