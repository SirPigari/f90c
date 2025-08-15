program test_operators
    implicit none
    print *, "ops add: ", 2 + 3
    print *, "ops sub: ", 10 - 4
    print *, "ops mul: ", 6 * 7
    print *, "ops div: ", 8 / 2
    print *, "ops pow: ", 2 ** 8
    print *, "ops eq: ", 5 == 5
    print *, "ops ne: ", 5 /= 4
    print *, "ops lt: ", 3 < 4
    print *, "ops gt: ", 3 > 4
    print *, "ops le: ", 3 <= 3
    print *, "ops ge: ", 4 >= 3
    print *, "ops and: ", .true. .and. .false.
    print *, "ops or: ", .true. .or. .false.
    print *, "ops not: ", .not. .false.
    print *, "ops eqv: ", .true. .eqv. .true.
    print *, "ops neqv: ", .true. .neqv. .false.
    print *, "ops concat: ", "hello " // "world"
end program test_operators
