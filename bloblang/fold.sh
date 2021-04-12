#!/bin/bash

# https://www.benthos.dev/docs/guides/bloblang/about

echo \
[ \
    { \
        \"title\": \"Getting started\", \
        \"reset_lesson_position\": false, \
        \"lessons\": [{ \"name\": \"Welcome\" }, { \"name\": \"Installation\" } ] \
    }, \
    { \
        \"title\": \"Basic operator\", \
        \"reset_lesson_position\": false, \
        \"lessons\": [{ \"name\": \"Addition / Subtraction\" }, { \"name\": \"Multiplication / Division\" }] \
    }, \
    { \
        \"title\": \"Advanced topics\", \
        \"reset_lesson_position\": true, \
        \"lessons\": [{ \"name\": \"Mutability\" }, { \"name\": \"Immutability\" } ] \
    } \
] | \

benthos blobl --pretty 'root = this.fold({
    "section_position": 0,
    "lesson_position": 0,
    "sections": []
}, section -> {
    "section_position": section.tally.section_position + 1,
    "lesson_position": if section.value.reset_lesson_position {
        section.value.lessons.length()
    } else {
        section.tally.lesson_position + section.value.lessons.length()
    },
    "sections": section.tally.sections.append(section.value.without("lessons").merge({
       "position": section.tally.section_position + 1,
       "lessons": section.value.lessons.enumerated().map_each(lesson -> lesson.value.merge({
           "position": if section.value.reset_lesson_position {
               lesson.index + 1
           } else {
               lesson.index + section.tally.lesson_position + 1
           }
       }))
    }))
}).sections'
