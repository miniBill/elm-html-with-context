module Html.WithContext.Internal exposing (Attribute(..), Html(..), runAttribute, runHtml, withContext, withContextAttribute, html, htmlAttribute)

import Html
import VirtualDom


type Html context msg
    = Html (context -> VirtualDom.Node msg)


type Attribute context msg
    = Attribute (context -> VirtualDom.Attribute msg)


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


html : Html.Html msg -> Html context msg
html = always >> Html


htmlAttribute : Html.Attribute msg -> Attribute context msg
htmlAttribute = always >> Attribute
