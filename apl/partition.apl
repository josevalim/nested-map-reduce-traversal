⍝ Try It Online on https://tio.run/##RZDBSgMxFEX38xVvlwmotR8gWFsRwa6qiMtMJpbAazLMZEqLuBEp7UhFEcF13XQv/kA/JT8yvpmR6Sbk3nO5Ly8iwcN4LtCOS//62b9jmUDHwL88A@nR@dFNfzhgkZ2BNXB4f2IN88W3MiJCNepdXZfaJLnzi7eQXSjntBlD5kTqVMzgGEJ2q1DaiWLALg0BROE0dXBO7ExkWoJNVCqcTZt8L451lYAOjPLIpULWeWDDHJ1OUEvxjwd6qrO2qxdPhZEqBmcTLTMGXTKHuRORRu3m1fzJZC85D2g9erZfvNcbBAZSwNr48OtVa8vKWm0akc5IdU@7vtikQabjiq1/ZID1NQwpuNsi7xDivlj4ZRES98svAnQeUCvfbamleMJAV9Me2nF@/fu427aSKgEDm7vmd1ufqqF6qsb9Ak2qLP8A

input←('Getting started' 0 ('Welcome' 'Installation')) ('Basic operator' 0 ('Addition / Subtraction' 'Multiplication / Division')) ('Advanced topics' 1 ('Mutability' 'Immutability'))
⎕←↑input
n r l←↓⍉↑input
c←≢input
rx←1@1⊢r
sid←⍳c
lid←((≢¨l)/sid)⊆∊(⍳∘≢∘,↑)¨rx⊂l
il←{↓⍉↑⍵}¨↓⍉↑lid l
output←↓⍉↑sid n r il
⎕←↑output

⍝ A commented version with some explanations: https://tio.run/##nVRNbxMxEL37V8xt11KbEr4OSEiUtkJIrZBIEeI42XUSS157ZXtLIsQFoagNKgIhJM7l0jviD/Sn7B8Js950NyUNFHLYyPb4zXvzxoO52kwnqMxwXn78uvMqcqh8BOWH90Dr3l7nxc7BbtQ3YzAaNgcPjY7K2Xehsa9Eb3v/cC51Xvhy@imOngjvpR6C82i9SCO4BXH0UqjEZCKC6KmmA6XQS8LgnM4eo5MJmFxY9MbW8dtpKqsI2IJe0fcWkxAP0UGhvMyVTHBxvCuPpGuwttMj1IlIwZtcJi6CLm0eFB77Ukk/qfJnWbvknDHSR7yJFwl4EC2W5fRzUMSYBgsq7HwpT0@a/cWty/hobxxIUubEqCLTroFqY3oiqACNmWiO9UrYc@GEByWco9jcuFCIJt6uxO@HyDahYiypCJ@c6XUUHLEsdCs3YcyO6b/7qFvOzuz1lNwGWIHpBAbGQk7mBmJkdcttzJiTaZX89EeyVr/UqRgLB7G3ZB4q3gDQZcZUQIhjEnBxrvgWbfJyNi2PZzHBlsff6IC@G@QFvzgn3rN3al1R2lwDqZczUZKVO5v/9WuuH44k5XOQGi2gP4Euv1KlhaOuOrKhnhATdb5UvWuV3OYwoOfiRQDxI5GRKKqJAIHJqIaCoTVF3iDFf67NHV7bX@GhTmEodPX2qqXUHizqoXB/T3JzM@7@LmEZsG3cf3M4VgKPajz0gIFs1ZkIGU0a7TudDm@C75EXVpAdtWZAa3ECDifV@vVIksbFg@sLZfTQ0QBZ7Lu6a1vdVxpzhdZ9XstqHa96UCZUUUwSY9NA2RBrapUM8xBY06F2mPI1eW76AP63hxmT1ZR704y58vTn24vzZknvBRQzha/HfLNP3KAakfIag3DghQVHbgDpFDpIN4PLCXnFoGcBeXn@1rnm818
