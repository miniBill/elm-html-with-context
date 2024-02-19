module Html.WithContext.Keyed exposing (node, ol, ul)

{-| 
@docs node, ol, ul
-}


import Html
import Html.WithContext
import Html.WithContext.Internal as Internal
import VirtualDom


{-| Works just like `Html.node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.
-}
node :
    String
    -> List (Html.WithContext.Attribute context msg)
    -> List ( String, Html.WithContext.Html context msg )
    -> Html.WithContext.Html context msg
node name attrs children =
    Internal.Html
        (\context ->
            VirtualDom.keyedNode
                name
                (List.map (Internal.runAttribute context) attrs)
                (List.map
                    (\(key, child) -> ( key, Internal.runHtml context child ))
                    children
                )
        )


{-|-}
ol :
    List (Html.WithContext.Attribute context msg)
    -> List ( String, Html.WithContext.Html context msg )
    -> Html.WithContext.Html context msg
ol attrs children =
    node "ol" attrs children


{-|-}
ul :
    List (Html.WithContext.Attribute context msg)
    -> List ( String, Html.WithContext.Html context msg )
    -> Html.WithContext.Html context msg
ul attrs children =
    node "ul" attrs children