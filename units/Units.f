!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

program Units

    use ConditionsRunnerUnit

    implicit none

    type(ConditionsRunner) conditions

    call conditions%init()
    call conditions%run()
    call conditions%clean()
end program
