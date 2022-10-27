module Solver 

    implicit none
    contains

    subroutine euler(n, yin, ynext, t, h, func)
        implicit none
        integer, intent(in) :: n      ! number of ODEs
        real, intent(in)    :: t, h
        real, dimension(n), intent(in)  :: yin
        real, dimension(n), intent(out)  :: ynext
        external      :: func
        integer            :: i
        real, dimension(n) :: k1

        ! call func to obtain the values of dydt
        call func(n,t,yin,k1)

        ! compute ynext using the Euler's method
        do i=1, n
            ynext(i) = yin(i) + h * k1(i)
        enddo

    end subroutine euler

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

    end subroutine rk2

    subroutine rk4(n, yin, ynext, t, h, func)
        implicit none
        integer, intent(in) :: n      ! number of ODEs
        real, intent(in)    :: t, h
        real, dimension(n), intent(in)  :: yin
        real, dimension(n), intent(out)  :: ynext
        external      :: func

        integer :: i
        real              :: h2
        real,dimension(n) :: k1,k2,k3,k4
        real,dimension(n) :: y2,y3,y4



    end subroutine rk4
end module Solver
