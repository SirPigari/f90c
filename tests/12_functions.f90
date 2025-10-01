program test_functions
    implicit none
    integer :: i4, j4

    i4 = foo()
    j4 = bar()

    print *, "i4: ", i4
    print *, "j4: ", j4
contains

function foo()
    print *, "in foo"
    foo = 1
end function foo

function bar()
    integer :: x
    x = 5
    print *, "bar x: ", x
    bar = x
end function bar

end program test_functions
