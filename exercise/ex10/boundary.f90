subroutine boundary(v)
    !
    ! apply BC on array v
    !
    use Simulation_data
    implicit none
    real, dimension(istart-ibuf:iend+ibuf), intent(inout) :: v
    integer :: i

    ! apply boundary condition

    ! left boundary (periodic)
    do i = 1, ibuf
        v(istart-i) = v(iend-i+1)
    enddo
    ! right boundary (periodic)
    do i = 1, ibuf
        v(iend+i) = v(istart+i-1)
    enddo

end subroutine boundary
