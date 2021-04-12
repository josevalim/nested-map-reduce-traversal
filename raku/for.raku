#!/usr/bin/env raku

my @sections = (
    {
        title => 'Getting started',
        reset_lesson_position => False,
        lessons => (
            { :name<Welcome> },
            { :name<Installation> },
        ),
    },
    {
        title => 'Basic operator',
        reset_lesson_position => False,
        lessons => (
            { name => 'Addition / Subtraction' },
            { name => 'Multiplication / Division' },
        ),
    },
    {
        title => 'Advanced topics',
        reset_lesson_position => True,
        lessons => (
            { :name<Mutability> },
            { :name<Immutability> },
        ),
    },
);

for @sections {
    state $lesson_counter = 1;

    $lesson_counter = 1 if .<reset_lesson_position>;

    .<position> = ++$;

    .<position> = $lesson_counter++ for .<lessons><>
}

put @sections.raku;
