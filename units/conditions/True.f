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

module TrueConditionUnit

    use Unit
    use Conditions

    implicit none
    private

    type, extends(UnitCase), public :: TrueConditionCase
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(TrueConditionCase),   intent(in out) :: self
        character(len=*), optional, intent(in)     :: name

        call self%UnitCase%init('True condition')

        call self%add(true_normal,   'Normal case')
        call self%add(true_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(TrueConditionCase), intent(in out) :: self

        call self%UnitCase%clean()
    end subroutine

    subroutine true_normal(self)
        class(UnitCase), intent(in out) :: self
        type(Asserts) asserts

        if (.not. true(.true.)) then
            call asserts%fail()
        end if
    end subroutine

    subroutine true_negative(self)
        class(UnitCase), intent(in out) :: self
        type(Asserts) asserts

        if (true(.false.)) then
            call asserts%fail()
        end if
    end subroutine
end module
