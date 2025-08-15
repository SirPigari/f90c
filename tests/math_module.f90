module math_module
  implicit none
contains
  integer :: add
  function add(a, b)
    integer, intent(in) :: a, b
    integer :: add
    add = a + b
  end function add
end module math_module
