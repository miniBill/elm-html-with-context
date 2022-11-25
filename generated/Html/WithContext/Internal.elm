module Html.WithContext.Internal exposing (Attribute(..), Html(..), runAttribute, runHtml)

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
