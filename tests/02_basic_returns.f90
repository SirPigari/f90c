function foo()
  integer :: foo
  foo = 42
end function foo

function bar()
  real :: bar
  bar = 3.5
end function bar

program main
  implicit none
  integer :: i
  real :: r
  i = foo()
  r = bar()
  print *, "i: ", i
  print *, "r: ", r
end program main
