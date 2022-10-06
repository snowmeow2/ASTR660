program pi
    implicit none
    integer :: i
    integer :: N = 10000
    real*8 :: area
    real*8 :: err

    real*8, parameter :: pi_ = 4.0*atan(1.0)
    integer, parameter :: NMAX = 8
    integer, dimension(NMAX) :: n_iteration

    open(unit=1, file='pi_error.dat', status='replace')
    write(1, *) "N ", "Rel_err"
    do i = 1,NMAX
        n_iteration(i) = 10**i
        area = 0

        call compute_integral(n_iteration(i), area)
        err = abs(pi_-2*area)/pi_
        write(1, *), n_iteration(i), err
        print *, "PI = ", 2.*area
    end do

    close(1)

end program pi

subroutine compute_integral(N, area)
    implicit none
    integer, intent(in) :: N
    real*8, intent(out) :: area

    integer :: i
    real*8 :: x, dx
    real :: radius = 1
    real*8 :: my_func ! function of a circle

    dx = radius*2/N
    
    print *, "Total bin = ", N

    do i = 1,N
        x = radius - i*dx + dx/2
        area = area + dx * my_func(x)
    enddo

    return
end subroutine compute_integral

real*8 function my_func(x)
    real*8 :: x
    my_func = sqrt(1-x**2)
    return 
end function