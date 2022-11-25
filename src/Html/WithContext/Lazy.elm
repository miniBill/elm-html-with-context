module Html.WithContext.Lazy exposing (apply1, apply2, apply3, apply4, apply5, apply6, lazy, lazy2, lazy3, lazy4, lazy5, lazy6)

{-| 
@docs apply6, apply5, apply4, apply3, apply2, apply1, lazy6, lazy5, lazy4, lazy3, lazy2, lazy
-}


import Html
import Html.Lazy
import Html.WithContext
import Html.WithContext.Internal as Internal


{-| A performance optimization that delays the building of virtual DOM nodes.

Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
it. Calling `(lazy view model)` delays the call until later. During diffing, we
can check to see if `model` is referentially equal to the previous value used,
and if so, we just stop. No need to build up the tree structure and diff it,
we know if the input to `view` is the same, the output must be the same!
-}
lazy :
    (a -> Html.WithContext.Html context msg)
    -> a
    -> Html.WithContext.Html context msg
lazy ctor a =
    Internal.Html (\context -> Html.Lazy.lazy3 apply1 context ctor a)


{-| Same as `lazy` but checks on two arguments. -}
lazy2 :
    (a -> b -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> Html.WithContext.Html context msg
lazy2 ctor a b =
    Internal.Html (\context -> Html.Lazy.lazy4 apply2 context ctor a b)


{-| Same as `lazy` but checks on three arguments. -}
lazy3 :
    (a -> b -> c -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> Html.WithContext.Html context msg
lazy3 ctor a b c =
    Internal.Html (\context -> Html.Lazy.lazy5 apply3 context ctor a b c)


{-| Same as `lazy` but checks on four arguments. -}
lazy4 :
    (a -> b -> c -> d -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> Html.WithContext.Html context msg
lazy4 ctor a b c d =
    Internal.Html (\context -> Html.Lazy.lazy6 apply4 context ctor a b c d)


{-| Same as `lazy` but checks on five arguments. -}
lazy5 :
    (a -> b -> c -> d -> e -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> Html.WithContext.Html context msg
lazy5 ctor a b c d e =
    Internal.Html (\context -> Html.Lazy.lazy7 apply5 context ctor a b c d e)


{-| Same as `lazy` but checks on six arguments. -}
lazy6 :
    (a -> b -> c -> d -> e -> f -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> Html.WithContext.Html context msg
lazy6 ctor a b c d e f =
    Internal.Html (\context -> Html.Lazy.lazy8 apply6 context ctor a b c d e f)


apply1 :
    context -> (a -> Html.WithContext.Html context msg) -> a -> Html.Html msg
apply1 context fn a =
    Internal.runHtml context (fn a)


apply2 :
    context
    -> (a -> b -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> Html.Html msg
apply2 context fn a b =
    Internal.runHtml context (fn a b)


apply3 :
    context
    -> (a -> b -> c -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> Html.Html msg
apply3 context fn a b c =
    Internal.runHtml context (fn a b c)


apply4 :
    context
    -> (a -> b -> c -> d -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> Html.Html msg
apply4 context fn a b c d =
    Internal.runHtml context (fn a b c d)


apply5 :
    context
    -> (a -> b -> c -> d -> e -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> Html.Html msg
apply5 context fn a b c d e =
    Internal.runHtml context (fn a b c d e)


apply6 :
    context
    -> (a -> b -> c -> d -> e -> f -> Html.WithContext.Html context msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> f
    -> Html.Html msg
apply6 context fn a b c d e f =
    Internal.runHtml context (fn a b c d e f)


