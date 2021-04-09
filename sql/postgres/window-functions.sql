-- https://dbfiddle.uk/?rdbms=postgres_13&fiddle=a3c679296f4c05171db99d7a9aaecb70

with input(value) as (values(
  '[
  {
    "title": "Getting started",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Welcome"},
      {"name": "Installation"}
    ]
  },

  {
    "title": "Basic operator",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Addition / Subtraction"},
      {"name": "Multiplication / Division"}
    ]
  },

  {
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      {"name": "Mutability"},
      {"name": "Immutability"},
      {"name": "Whatever"}
    ]
  },

  {
    "title": "Advanced topics2",
    "reset_lesson_position": true,
    "lessons": [
      {"name": "Mutability2"},
      {"name": "Immutability2"}
    ]
  },

  {
    "title": "Advanced topics3",
    "reset_lesson_position": false,
    "lessons": [
      {"name": "Mutability3"},
      {"name": "Immutability3"}
    ]
  }
]'::json
))
, disassembled as (
  select (section.json->>'title')::text as title
       , (section.json->>'reset_lesson_position')::boolean as reset_lesson_position
       , section.position as section_position
       , (lesson.json->>'name')::text as name
       , row_number() over (order by section.position, lesson.position) as absolute_position
  from input
  cross join lateral json_array_elements(input.value) with ordinality as section(json, position)
  cross join lateral json_array_elements((section.json->>'lessons')::json) with ordinality as lesson(json, position)
) --select * from disassembled;
, found_reset_points as (
  select *, (reset_lesson_position and lag(section_position) over (order by absolute_position) != section_position)::int as reset_point
  from disassembled
) --select * from found_reset_points;
, found_lesson_position_groups as (
  select *, sum(reset_point) over (order by absolute_position) as lesson_position_group
  from found_reset_points
) --select * from found_lesson_position_groups;
, renumbered as (      
  select *, row_number() over (partition by lesson_position_group) as lesson_position
  from found_lesson_position_groups
) -- select * from renumbered;
select json_agg(section order by section_position) 
from (
  select json_build_object(
           'title', title,
           'reset_lesson_position', reset_lesson_position,
           'position', section_position,
           'lessons', array_agg(json_build_object(
             'name', name,
             'position', lesson_position
           ) order by lesson_position)
         ) as section
       , section_position
  from renumbered
  group by title, reset_lesson_position, section_position
) rows;
