! link-with: math_module.f90
program main
  use math_module
  implicit none
  integer :: result

  result = add(3, 5)
  print *, "Result is ", result
end program main
