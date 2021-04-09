-- A solution for
--      https://github.com/josevalim/nested-data-structure-traversal
-- in Agda using fold. Basically it is identical to its Haskell sibling except
-- fancy unicode symbols. The `refl` proof at the end is pretty nice though.
--
-- Coded against agda-2.6.1.3 and agda-stdlib-1.5

module Fold where

open import Data.Nat using (ℕ; _+_)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.String using (String)
open import Data.List using (List; []; _∷_; map; foldl; reverse; length; zipWith; upTo)
open import Data.Product using (_×_; _,_; proj₂)
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

record Lesson : Set where
    constructor lesson
    field
        name : String

record Chapter : Set where
    constructor chapter
    field
        title : String
        reset : Bool
        lessons : List Lesson

record PosLesson : Set where
    constructor pos_lesson
    field
        name : String
        pos : ℕ

record PosChapter : Set where
    constructor pos_chapter
    field
        title : String
        reset : Bool
        pos : ℕ
        lessons : List PosLesson


input : List Chapter
input = chapter "Getting started" false
            (lesson "Welcome" ∷ lesson "Installation" ∷ [])
        ∷ chapter "Basic operator" false
            (lesson "Addition / Subtraction" ∷ lesson "Multiplication / Division" ∷ [])
        ∷ chapter "Advanced topics" true
            (lesson "Mutability" ∷ lesson "Immutability" ∷ [])
        ∷ []

expected : List PosChapter
expected = pos_chapter "Getting started" false 1
            (pos_lesson "Welcome" 1 ∷ pos_lesson "Installation" 2 ∷ [])
        ∷ pos_chapter "Basic operator" false 2
            (pos_lesson "Addition / Subtraction" 3 ∷ pos_lesson "Multiplication / Division" 4 ∷ [])
        ∷ pos_chapter "Advanced topics" true 3
            (pos_lesson "Mutability" 1 ∷  pos_lesson "Immutability" 2 ∷ [])
        ∷ []


solve : List Chapter → List PosChapter
solve i = (reverse ∘ proj₂ ∘ proj₂) (foldl go (1 , 1 , []) i)
    where
    go : ℕ × ℕ × List PosChapter → Chapter → ℕ × ℕ × List PosChapter
    go (nc , nl , acc) (chapter title reset lessons) =
        let n = length lessons
            nl' = if reset then 1 else nl
            ps = map (_+ nl') (upTo n)
            plessons = zipWith (λ {(lesson name) p → pos_lesson name p}) lessons ps
        in (nc + 1 , nl' + n , pos_chapter title reset nc plessons ∷ acc)

_ : solve input ≡ expected
_ = refl
