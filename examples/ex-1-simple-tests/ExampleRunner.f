!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module ExampleRunnerUnit

    use FUnit

    use ExampleUnit

    implicit none
    private

    type, extends(UnitRunner), public :: ExampleRunner
    private
        class(UnitSuite), pointer :: exampleSuite
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(ExampleRunner), intent(in out)   :: self
        character(len=*), optional, intent(in) :: name

        type(ExampleSuite), pointer :: exampleSuite

        call self%UnitRunner%init('An example for Unit testing library')

        allocate(exampleSuite)
        self%exampleSuite => exampleSuite

        call self%add(self%exampleSuite)
    end subroutine

    subroutine clean(self)
        class(ExampleRunner), intent(in out) :: self

        ! deallocates unit cases
        call self%UnitRunner%clean()
    end subroutine
end module
