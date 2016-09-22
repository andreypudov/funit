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

module FalseConditionUnit

    use Unit
    use Conditions

    implicit none
    private

    type, extends(UnitCase), public :: FalseConditionCase
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(FalseConditionCase),  intent(in out) :: self
        character(len=*), optional, intent(in)     :: name

        call self%UnitCase%init('False condition')

        call self%add(false_normal,   'Normal case')
        call self%add(false_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(FalseConditionCase), intent(in out) :: self

        call self%UnitCase%clean()
    end subroutine

    subroutine false_normal(self)
        class(UnitCase), intent(in out) :: self
        type(Asserts) asserts

        if (false(.true.)) then
            call asserts%fail()
        end if
    end subroutine

    subroutine false_negative(self)
        class(UnitCase), intent(in out) :: self
        type(Asserts) asserts

        if (.not. false(.false.)) then
            call asserts%fail()
        end if
    end subroutine
end module
