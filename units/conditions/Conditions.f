!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module ConditionsRunnerUnit

    use Unit

    use ArrayEqualsConditionUnit
    use EqualsConditionUnit
    use FalseConditionUnit
    use NotNullConditionUnit
    use NotSameConditionUnit
    use NullConditionUnit
    use SameConditionUnit
    use TrueConditionUnit

    implicit none

    type, extends(UnitRunner), public :: ConditionsRunner
        private
        class(UnitSuite), pointer :: arrayEqualsSuite
        class(UnitSuite), pointer :: equalsSuite
        class(UnitSuite), pointer :: falseSuite
        class(UnitSuite), pointer :: notNullSuite
        class(UnitSuite), pointer :: notSameSuite
        class(UnitSuite), pointer :: nullSuite
        class(UnitSuite), pointer :: sameSuite
        class(UnitSuite), pointer :: trueSuite
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(ConditionsRunner), intent(in out) :: self
        character(len=*), optional, intent(in)  :: name

        ! a list of condition suites
        type(ArrayEqualsConditionSuite), pointer :: arrayEqualsSuite
        type(EqualsConditionSuite),      pointer :: equalsSuite
        type(FalseConditionSuite),       pointer :: falseSuite
        type(NotNullConditionSuite),     pointer :: notNullSuite
        type(NotSameConditionSuite),     pointer :: notSameSuite
        type(NullConditionSuite),        pointer :: nullSuite
        type(SameConditionSuite),        pointer :: sameSuite
        type(TrueConditionSuite),        pointer :: trueSuite

        call self%UnitRunner%init('A unit testing library for Fortran')

        allocate(arrayEqualsSuite)
        allocate(equalsSuite)
        allocate(falseSuite)
        allocate(notNullSuite)
        allocate(notSameSuite)
        allocate(nullSuite)
        allocate(sameSuite)
        allocate(trueSuite)

        self%arrayEqualsSuite => arrayEqualsSuite
        self%equalsSuite      => equalsSuite
        self%falseSuite       => falseSuite
        self%notNullSuite     => notNullSuite
        self%notSameSuite     => notSameSuite
        self%nullSuite        => nullSuite
        self%sameSuite        => sameSuite
        self%trueSuite        => trueSuite

        call self%add(self%arrayEqualsSuite)
        call self%add(self%equalsSuite)
        call self%add(self%falseSuite)
        call self%add(self%notNullSuite)
        call self%add(self%notSameSuite)
        call self%add(self%nullSuite)
        call self%add(self%sameSuite)
        call self%add(self%trueSuite)
    end subroutine

    subroutine clean(self)
        class(ConditionsRunner), intent(in out) :: self

        ! deallocates unit cases
        call self%UnitRunner%clean()
    end subroutine
end module
