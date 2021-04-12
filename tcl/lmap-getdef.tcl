#! /usr/bin/env tclsh
# This code uses [dict getdef], currently a cutting edge feature.  It has been
# tested in Tcl 8.7a3 and Jim Tcl 0.80.  See
# https://core.tcl-lang.org/tips/doc/trunk/tip/342.md

set sections {
    {
        title {Getting started}
        reset_lesson_position false
        lessons {
            {name Welcome}
            {name Installation}
        }
    }
    {
        title {Basic operator}
        reset_lesson_position false
        lessons {
            {name {Addition / Subtraction}}
            {name {Multiplication / Division}}
        }
    }
    {
        title {Advanced topics}
        reset_lesson_position true
        lessons {
            {name Mutability}
            {name Immutability}
        }
    }
}

set lessonCounter 1
set sectionCounter 1

puts [lmap section $sections {
    if {[dict getdef $section reset_lesson_position false]} {
        set lessonCounter 1
    }

    dict set section position $sectionCounter
    incr sectionCounter

    dict set section lessons [lmap lesson [dict get $section lessons] {
        dict set lesson position $lessonCounter
        incr lessonCounter

        set lesson
    }]

    set section
}]
