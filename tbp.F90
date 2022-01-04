!#define NEBLK 10
!#define LEBLK 32

module mod_domain
  implicit none
  type diffusion_t
    integer :: iamhere
    double precision,dimension(LEBLK,64,6) :: rhs
    type(elemblk_t), pointer :: elemblk
    contains
      procedure, NOPASS :: part2 => compute_diffusion_part2
  end type diffusion_t
  type elemblk_t
    integer :: ib
    integer :: esblk
    type(diffusion_t)  :: diffusion
  end type elemblk_t
  type grid_t
    integer :: grid_id   ! identifier for this grid, global domain is grid 1
    type(elemblk_t), allocatable    :: elemblk(:)
  end type grid_t
  type domain_t
    type( grid_t ) :: grid(1)
  end type domain_t
  interface
    module subroutine compute_diffusion_part2( this, ib, es_task, ee_task )
      class (diffusion_t), target, intent(inout)    :: this
      integer,                     intent(in)       :: ib
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

module mod_diffusion_driver
  implicit none
  contains
    subroutine diffusion_driver( d )
      use mod_domain, only : domain_t
      implicit none
      type(domain_t), target, intent(inout) :: d
      integer                    :: ie,ib
      logical, external :: omp_is_initial_device 
write(0,*)'entering target region'
!$omp target 
!!$omp teams distribute parallel do private(ib,ie)
      do ib = 1, NEBLK
        do ie = 1, LEBLK
          call d%grid(1)%elemblk(ib)%diffusion%part2(d%grid(1)%elemblk(ib)%diffusion,      &
                        ib,                                                                &
                        d%grid(1)%elemblk(ib)%esblk-1+ie,                                  &
                        d%grid(1)%elemblk(ib)%esblk-1+ie                                   )
        enddo
      enddo
!$omp end target
write(0,*)'back from target region'
    end subroutine diffusion_driver
end module mod_diffusion_driver

program tbp
  use mod_neptune_model
  use mod_diffusion_driver
  implicit none
  type (neptune_t), target   :: model
  integer                    :: ie,ib
  logical, external :: omp_is_initial_device 
  allocate(model%domain%grid(1)%elemblk(NEBLK))
  do ib = 1, NEBLK
    model%domain%grid(1)%elemblk(ib)%ib    = ib
    model%domain%grid(1)%elemblk(ib)%esblk = (ib-1)*LEBLK+1
    model%domain%grid(1)%elemblk(ib)%diffusion%rhs(1:LEBLK,:,:) = -ib
  enddo
  write(0,*)'entering data map model'
!$omp target enter data map(to:model)
  write(0,*)'entering data map domain'
!$omp target enter data map(to:model%domain)
  write(0,*)'entering data map grid'
!$omp target enter data map(to:model%domain%grid)
  do ib = 1, NEBLK
  write(0,*)'entering data map elemblk',ib
!$omp target enter data map(to:model%domain%grid(1)%elemblk)
  enddo
  write(0,*)'hooking pointers '
!$omp target 
  do ib = 1, NEBLK
    model%domain%grid(1)%elemblk(ib)%diffusion%elemblk => model%domain%grid(1)%elemblk(ib)
  enddo
!$omp end target
  write(0,*)'done hooking pointers '
  write(0,*)'calling diffusion_driver'
  call diffusion_driver(model%domain)
  write(0,*)'back from diffusion_driver'
!$omp target update from(model%domain)
  write(0,*)'back from update'
  do ib = 1, NEBLK
!$omp target update from(model%domain%grid(1)%elemblk(ib)%diffusion%rhs)
  enddo
#if 1
!$omp target exit data map(from:model%domain)

  do ib = 1, NEBLK
    do ie = 1, LEBLK
      write(0,*)ib,ie,model%domain%grid(1)%elemblk(ib)%diffusion%rhs(ie,1,1)
    enddo
  enddo

  write(0,*)
  write(0,*)'  DONE  '
  write(0,*)
  stop
#endif

end program tbp
