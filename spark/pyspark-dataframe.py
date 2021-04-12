import json
from pyspark.sql import functions as f
from pyspark.sql import Window


json_val = '''
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
'''

json_schema = '''
    ARRAY<
      STRUCT<
        title: STRING,
        reset_lesson_position: BOOLEAN,
        lessons: ARRAY<
          STRUCT<name: STRING>
        >
      >
    >
'''

df = spark.createDataFrame([(json_val,)], ['j'])

df = (
  df.select(f.from_json('j', json_schema).alias('j'))
    .select(f.posexplode('j').alias('section_idx', 'section'))
    .select('section_idx', 'section.title', 'section.reset_lesson_position', 'section.lessons')
    .withColumn('reset', f.when(f.expr('section_idx = 0 or reset_lesson_position'), 1).otherwise(0))
    .withColumn('unit', f.sum('reset').over(Window.orderBy('section_idx')))
    .select('unit', 'title', 'reset_lesson_position', 'section_idx', f.posexplode('lessons').alias('lesson_idx', 'lesson'))
    .withColumn('lesson_position', f.row_number().over(Window.partitionBy('unit').orderBy('section_idx', 'lesson_idx')))
    .withColumn('lesson', f.struct(f.col('lesson.name').alias('name'), f.col('lesson_position').alias('position')))
    .select('title', 'reset_lesson_position', (f.col('section_idx') + 1).alias('position'), 'lesson')
    .groupby('title', 'reset_lesson_position', 'position').agg(f.collect_list('lesson').alias('lessons'))
)

print(json.dumps(df.toJSON().map(json.loads).collect(), indent=4))
