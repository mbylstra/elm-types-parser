-- how do you distinguish between


module Main exposing (..)


type MyType
    = MyHtml (Html Msg)
type MyType
    = MyHtml Html Msg



-- The problem is that if we do


type MyType
    = A Int -> Int

    -- I reckon it will get tripped up on functions too.
    -- So, the args to Type constructors either need to be
    -- UpperVar (which will become a simple type) OR tuple OR a record OR a lowercase type var
    -- OR a bracks separated thing
    -- As a workaround we could assume people don't put any wierd shit in type constructors that would require brackets



-- I think you'll have the same problem with functions:
-- it gets parsed as MyType getting an Int with an Int filled in, rather than
-- I *think* it's because when you're in a union type, a type can't have a list of "filled in type vars"
-- Or maybe it has to do some crazy shit to figure out that Html Msg already needs to be grouped!
-- OR, I think in the case of a union type a type has to either have not "filled in type vars", or it needs brackets!
-- For now lets assume union types can't have fancy types in them, but we can always add it later!
