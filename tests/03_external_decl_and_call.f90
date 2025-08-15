integer :: foo
function foo()
  foo = 5
end function foo

program main
  implicit none
  integer :: x
  x = foo()
  print *, "x: ", x
end program main
