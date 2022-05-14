!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module ExampleUnit

    use FUnit

    implicit none
    private

    type, extends(UnitSuite), public :: ExampleSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
  contains
    subroutine init(self, name)
        class(ExampleSuite),    intent(in out) :: self
        character(len=*), optional, intent(in) :: name

        call self%UnitSuite%init('Example suite')

        call self%add(assertTrue_trueConstant_true, 'Positive assert case')
        call self%add(expectTrue_trueConstant_true, 'Positive expect case')
        ! call self%add(assertTrue_trueConstant_fail, 'Failing assert case')
        ! call self%add(expectTrue_trueConstant_fail, 'Failing expect case')
    end subroutine

    subroutine clean(self)
        class(ExampleSuite), intent(in out) :: self

        print *, 'ExampleSuite / CLEAN'
        call self%UnitSuite%clean()
    end subroutine

    subroutine assertTrue_trueConstant_true(self)
        class(UnitSuite), intent(in out) :: self

        ! succesfull assert call
        call Assert%true(.true.)
    end subroutine

    subroutine expectTrue_trueConstant_true(self)
        class(UnitSuite), intent(in out) :: self

        ! succesfull expect call
        call Expect%true(.true.)
    end subroutine expectTrue_trueConstant_true

    subroutine assertTrue_trueConstant_fail(self)
        class(UnitSuite), intent(in out) :: self

        ! failure assert call
        call Assert%true(.false., 'True condition')
    end subroutine assertTrue_trueConstant_fail

    subroutine expectTrue_trueConstant_fail(self)
        class(UnitSuite), intent(in out) :: self

        ! failure assert call
        call Expect%true(.false., 'True condition')
    end subroutine
end module
