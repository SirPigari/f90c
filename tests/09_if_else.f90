program test_if_else
    implicit none
    logical :: cond
    cond = .true.

    if (cond) then
        print *, "if-then works"
    else
        print *, "if-else works (unexpected)"
    end if

    if (.false.) then
        print *, "should not see this"
    else
        print *, "else branch"
    end if
end program test_if_else
