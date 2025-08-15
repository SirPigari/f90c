program test_integer_kinds
    implicit none
    integer(1) :: i1
    integer(2) :: i2
    integer(4) :: i4
    integer(8) :: i8
    integer(16) :: i16

    i1 = 1
    i2 = 2
    i4 = 42
    i8 = 9000000000
    i16 = 12345678901234567890

    print *, i1, i2, i4, i8, i16
end program test_integer_kinds
