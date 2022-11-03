module Solver 

    implicit none
    contains

    subroutine rk2(n, yin, ynext, t, h, func)
        implicit none
        integer, intent(in) :: n      ! number of ODEs
        real, intent(in)    :: t, h
        real, dimension(n), intent(in)  :: yin
        real, dimension(n), intent(out)  :: ynext
        external      :: func
        integer            :: i
        real, dimension(n) :: k1, k2
        real,dimension(n)  :: y2

        ! compute k1 = func(t, yin)
        call func(n,t,yin,k1)

        ! compute y2 = yin + h*k1
        do i=1,n
            y2(i) = yin(i) + h*k1(i)
        end do

        ! compute k2 = func(t+h, y2)
        call func(n, t+h, y2, k2)

        ! compute ynext 
        do i=1,n
            ynext(i) = yin(i) + h/2*(k1(i) + k2(i))
        end do

        return
    end subroutine rk2

end module Solver
