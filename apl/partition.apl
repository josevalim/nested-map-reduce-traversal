⍝ Try It Online on https://tio.run/##RVDBSsNAEL3nK@a2CVhrP8BDbUUEe6oiIh42m7UMTHdDsikW8SJS2khFEcGzXnoXf6Cfsj9SJ6mkl2Xmvcd7b1am1Eqmkuxo418@elcil@QE@Ocn4H14vH/RG/RFbO/AGmjdHlojfPmtjYxJD7tn5xs0aeH87DUUJ9o5NCPIncycTgQcQCguNSk71gLEqWGCSDpkjyhi7kjmqMCmOpPOZlt9N0mwUkAbhkXsMqlqPYhBQQ5TQiX/6T5OMG@8uslEGqUTcDZFlQvoMDgonIyR0E2r/PF4t0ZRwOdxbT97qy8IDGRANfDul4sGVhW0@DJBdt254bkT5JhU2PJHBVSPYciC9YqiNlORL2d@XobM@/knE/zusVu0XmW@fKQAq5D7JsUvfx/Wq2ZlR6DAFm77qQ3OzlA1RNr13qo2mz8

input←('Getting started' 0 ('Welcome' 'Installation')) ('Basic operator' 0 ('Addition / Subtraction' 'Multiplication / Division')) ('Advanced topics' 1 ('Mutability' 'Immutability'))
⎕←↑input
n r l←↓⍉↑input
c←≢n
r[1]←1
sid←⍳c
lid←((≢¨l)/sid)⊆∊(⍳∘≢∘,↑)¨r⊂l
il←{↓⍉↑⍵}¨↓⍉↑lid l
output←↓⍉↑sid n r il
⎕←↑output
