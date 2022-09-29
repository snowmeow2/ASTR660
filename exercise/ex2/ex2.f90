program sum100
integer :: arr(100)

do i = 1,100
    arr(i) = i
end do

result = sum(arr)
print *, "The sum from 1 to 100 is", result

end program sum100