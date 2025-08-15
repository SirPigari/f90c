program test_subroutine_and_args
    implicit none
    integer :: add_res, mix_res
    real :: rf
    double precision :: rd

    add_res = add(10, 32)
    print *, "add(10,32): ", add_res

    mix_res = mix(2, 5, 3)
    print *, "mix(2,5,3): ", mix_res

    call show3(1, 2, 3)

    rf = real_add(1.5, 2.5)
    rd = dp_add(1.25, 2.75)
    print *, "real_add: ", rf
    print *, "dp_add: ", rd

contains

function add(a, b)
    integer :: add, a, b
    add = a + b
end function add

function mix(a, b, c)
    integer :: mix, a, b, c
    mix = a * b + c
end function mix

subroutine show3(a, b, c)
    integer :: a, b, c
    print *, "show3 args: ", a, b, c
end subroutine show3

function real_add(x, y)
    real :: real_add, x, y
    real_add = x + y
end function real_add

function dp_add(x, y)
    double precision :: dp_add, x, y
    dp_add = x + y
end function dp_add

end program test_subroutine_and_args
