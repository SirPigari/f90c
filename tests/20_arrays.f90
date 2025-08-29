program test_arrays
	implicit none
	integer :: a(5)
	integer :: i, sum

	do i = 1, 5
		a(i) = i * 2
	end do

	sum = 0
	do i = 1, 5
		sum = sum + a(i)
	end do

    print *, a

	print *, "a1: ", a(1)
	print *, "a5: ", a(5)
	print *, "sum: ", sum
end program test_arrays
