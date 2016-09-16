!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

module ConditionsSuiteUnit

    use Unit
    use FalseConditionUnit
    use TrueConditionUnit

    implicit none

    type, extends(UnitSuite), public :: ConditionsSuite
    private
        class(UnitCase), pointer :: falseCase
        class(UnitCase), pointer :: trueCase
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(ConditionsSuite), intent(in out) :: self
        character(len=*), optional, intent(in) :: name

        ! a list of condition cases
        type(FalseConditionCase), pointer :: falseCase
        type(TrueConditionCase),  pointer :: trueCase

        call self%UnitSuite%init('A unit testing library for Fortran : Conditions')

        allocate(falseCase)
        allocate(trueCase)

        self%falseCase => falseCase
        self%trueCase  => trueCase

        call self%add(self%falseCase)
        call self%add(self%trueCase)
    end subroutine

    subroutine clean(self)
        class(ConditionsSuite), intent(in out) :: self

        call self%UnitSuite%clean()

        deallocate(self%falseCase)
        deallocate(self%trueCase)
    end subroutine
end module
