#define NEBLK 10
#define LEBLK 32

module mod_domain
  implicit none
  type diffusion_t
    double precision,dimension(LEBLK,64,6) :: rhs
    type(elemblk_t), pointer :: elemblk
    contains
      procedure :: part2 => compute_diffusion_part2
  end type diffusion_t
  type elemblk_t
    integer :: esblk
    type(diffusion_t)  :: diffusion
  end type elemblk_t
  type grid_t
    integer :: grid_id   ! identifier for this grid, global domain is grid 1
    type(elemblk_t)    :: elemblk(NEBLK)
  end type grid_t
  type domain_t
    type( grid_t ) :: grid
  end type domain_t
  interface
    module subroutine compute_diffusion_part2( this, es_task, ee_task )
      class (diffusion_t), target, intent(inout)    :: this
      integer,                     intent(in)       :: es_task, ee_task
    end subroutine compute_diffusion_part2
  end interface
end module mod_domain

module mod_neptune_model
  use mod_domain
  implicit none
  type neptune_t
    type ( domain_t ) :: domain
  end type neptune_t
end module mod_neptune_model

program tbp
  use mod_neptune_model
  implicit none
  type (neptune_t), target   :: model
!  integer, parameter         :: leblk = 32
!  integer, parameter         :: neblk = 10
  integer                    :: ie,ib
!  allocate(model%domain%grid%elemblk(neblk))
  do ib = 1, NEBLK
    model%domain%grid%elemblk(ib)%esblk = (ib-1)*LEBLK+1
    model%domain%grid%elemblk(ib)%diffusion%elemblk => model%domain%grid%elemblk(ib)
!    allocate(model%domain%grid%elemblk(ib)%diffusion%rhs(LEBLK,64,6))
    model%domain%grid%elemblk(ib)%diffusion%rhs(1:LEBLK,64,6) = 0.
  enddo
!$omp target if(.true.)
  do ib = 1, NEBLK
    do ie = 1, LEBLK
      call model%domain%grid%elemblk(ib)%diffusion%part2(ie,ie)
    enddo
  enddo
!$omp end target
  print*,model%domain%grid%elemblk(1)%diffusion%rhs(1,1,1)
  print*
  print*,'  DONE  '
  print*
  stop

end program tbp
