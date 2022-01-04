#define NEBLK 10
#define LEBLK 32

module mod_grid
  implicit none
  type elem_t
    integer             :: rhs
    contains
      procedure, NOPASS :: compute
  end type elem_t
  type grid_t
#ifdef NOT_WORKING
    type(elem_t), allocatable  :: elem(:)
#else
    type(elem_t)               :: elem(NEBLK)
#endif
  end type grid_t
  contains
  subroutine compute( this, ib )
!$omp declare target
    class (elem_t), target, intent(inout)    :: this
    integer,                intent(in)       :: ib
    this%rhs = ib
  end subroutine
end module mod_grid

program tbp
  use mod_grid
  implicit none
  type (grid_t)   :: grid
  integer         :: ib
#ifdef NOT_WORKING
  allocate(grid%elem(NEBLK))
#endif
!$omp target enter data map(to:grid)
  do ib = 1, NEBLK
!$omp target enter data map(to:grid%elem(ib))
  enddo
!$omp target 
  do ib = 1, NEBLK
    call grid%elem(ib)%compute(grid%elem(ib), ib ) 
  enddo
!$omp end target
!$omp target update from(grid)
  do ib = 1, NEBLK
!$omp target update from(grid%elem(ib))
  enddo
  do ib = 1, NEBLK
      write(0,*)ib,grid%elem(ib)%rhs
  enddo
  stop
end program tbp

