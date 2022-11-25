module Html.WithContext.Internal exposing (Attribute(..), Html(..), runAttribute, runHtml, withContext, withContextAttribute)

import Html


type Html context msg
    = Html (context -> Html.Html msg)


type Attribute context msg
    = Attribute (context -> Html.Attribute msg)


runHtml : context -> Html context msg -> Html.Html msg
runHtml context (Html f) =
    f context


runAttribute : context -> Attribute context msg -> Html.Attribute msg
runAttribute context (Attribute f) =
    f context


withContext : (context -> Html context msg) -> Html context msg
withContext f =
    Html (\context -> runHtml context <| f context)


withContextAttribute : (context -> Attribute context msg) -> Attribute context msg
withContextAttribute f =
    Attribute (\context -> runAttribute context <| f context)
