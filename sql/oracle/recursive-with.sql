-- Oracle 21c

WITH j AS (SELECT json('
[
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
      {"name": "Immutability"}
    ]
  }
]
') d FROM dual),
sections AS (SELECT jt.*,rownum rn FROM j,
	json_table(d, '$[*]' COLUMNS(
		title,reset_lesson_position, lessons FORMAT JSON)) jt),
r(title, reset_lesson_position, position, lessons, lpos) AS (
	SELECT title, reset_lesson_position, 1, 
		(SELECT json_arrayagg(json_object(name, 'position' VALUE rownum)) 
		FROM json_table(lessons, '$[*]' COLUMNS(name))),
		json_value(lessons, '$.size()')
	FROM sections WHERE rn = 1
	UNION ALL
	SELECT s.title, s.reset_lesson_position, position + 1, 
		(SELECT json_arrayagg(json_object(name, 
			'position' VALUE CASE s.reset_lesson_position 
				WHEN 'true' THEN 0 ELSE lpos END + rownum)) 
			FROM json_table(s.lessons, '$[*]' COLUMNS(name))),
		CASE s.reset_lesson_position 
			WHEN 'true' THEN 0 ELSE lpos END + 
				json_value(s.lessons, '$.size()')
	FROM sections s,r WHERE rn = position + 1
)
SELECT json_serialize(json_arrayagg(json_object
	(title, reset_lesson_position, position, lessons)) PRETTY) FROM r