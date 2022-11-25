module Html.WithContext.Events exposing (custom, keyCode, on, onBlur, onCheck, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver, onMouseUp, onSubmit, preventDefaultOn, stopPropagationOn, targetChecked, targetValue)

{-| 
@docs keyCode, targetChecked, targetValue, custom, preventDefaultOn, stopPropagationOn, on, onFocus, onBlur, onSubmit, onCheck, onInput, onMouseOut, onMouseOver, onMouseLeave, onMouseEnter, onMouseUp, onMouseDown, onDoubleClick, onClick
-}


import Html.Events
import Html.WithContext
import Html.WithContext.Internal as Internal
import Json.Decode as Json


onClick : msg -> Html.WithContext.Attribute context msg
onClick arg =
    Internal.Attribute (\_ -> Html.Events.onClick arg)


onDoubleClick : msg -> Html.WithContext.Attribute context msg
onDoubleClick arg =
    Internal.Attribute (\_ -> Html.Events.onDoubleClick arg)


onMouseDown : msg -> Html.WithContext.Attribute context msg
onMouseDown arg =
    Internal.Attribute (\_ -> Html.Events.onMouseDown arg)


onMouseUp : msg -> Html.WithContext.Attribute context msg
onMouseUp arg =
    Internal.Attribute (\_ -> Html.Events.onMouseUp arg)


onMouseEnter : msg -> Html.WithContext.Attribute context msg
onMouseEnter arg =
    Internal.Attribute (\_ -> Html.Events.onMouseEnter arg)


onMouseLeave : msg -> Html.WithContext.Attribute context msg
onMouseLeave arg =
    Internal.Attribute (\_ -> Html.Events.onMouseLeave arg)


onMouseOver : msg -> Html.WithContext.Attribute context msg
onMouseOver arg =
    Internal.Attribute (\_ -> Html.Events.onMouseOver arg)


onMouseOut : msg -> Html.WithContext.Attribute context msg
onMouseOut arg =
    Internal.Attribute (\_ -> Html.Events.onMouseOut arg)


{-| Detect [input](https://developer.mozilla.org/en-US/docs/Web/Events/input)
events for things like text fields or text areas.

For more details on how `onInput` works, check out [`targetValue`](#targetValue).

**Note 1:** It grabs the **string** value at `event.target.value`, so it will
not work if you need some other information. For example, if you want to track
inputs on a range slider, make a custom handler with [`on`](#on).

**Note 2:** It uses `stopPropagationOn` internally to always stop propagation
of the event. This is important for complicated reasons explained [here][1] and
[here][2].

[1]: /packages/elm/virtual-dom/latest/VirtualDom#Handler
[2]: https://github.com/elm/virtual-dom/issues/125
-}
onInput : (String -> msg) -> Html.WithContext.Attribute context msg
onInput arg =
    Internal.Attribute (\_ -> Html.Events.onInput arg)


{-| Detect [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)
events on checkboxes. It will grab the boolean value from `event.target.checked`
on any input event.

Check out [`targetChecked`](#targetChecked) for more details on how this works.
-}
onCheck : (Bool -> msg) -> Html.WithContext.Attribute context msg
onCheck arg =
    Internal.Attribute (\_ -> Html.Events.onCheck arg)


{-| Detect a [submit](https://developer.mozilla.org/en-US/docs/Web/Events/submit)
event with [`preventDefault`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault)
in order to prevent the form from changing the page’s location. If you need
different behavior, create a custom event handler.
-}
onSubmit : msg -> Html.WithContext.Attribute context msg
onSubmit arg =
    Internal.Attribute (\_ -> Html.Events.onSubmit arg)


onBlur : msg -> Html.WithContext.Attribute context msg
onBlur arg =
    Internal.Attribute (\_ -> Html.Events.onBlur arg)


onFocus : msg -> Html.WithContext.Attribute context msg
onFocus arg =
    Internal.Attribute (\_ -> Html.Events.onFocus arg)


{-| Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Decode

    onClick : msg -> Attribute msg
    onClick message =
      on "click" (Decode.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.

The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.

If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really helps!

[aEL]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
[decoder]: /packages/elm/json/latest/Json-Decode
[tutorial]: https://github.com/evancz/elm-architecture-tutorial/

**Note:** This creates a [passive][] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
on : String -> Json.Decoder msg -> Html.WithContext.Attribute context msg
on arg1 arg2 =
    Internal.Attribute (\_ -> Html.Events.on arg1 arg2)


{-| Create an event listener that may [`stopPropagation`][stop]. Your decoder
must produce a message and a `Bool` that decides if `stopPropagation` should
be called.

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation

**Note:** This creates a [passive][] event listener, enabling optimizations for
touch, scroll, and wheel events in some browsers.

[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md
-}
stopPropagationOn :
    String
    -> Json.Decoder ( msg, Bool )
    -> Html.WithContext.Attribute context msg
stopPropagationOn arg1 arg2 =
    Internal.Attribute (\_ -> Html.Events.stopPropagationOn arg1 arg2)


{-| Create an event listener that may [`preventDefault`][prevent]. Your decoder
must produce a message and a `Bool` that decides if `preventDefault` should
be called.

For example, the `onSubmit` function in this library *always* prevents the
default behavior:

[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

    onSubmit : msg -> Attribute msg
    onSubmit msg =
      preventDefaultOn "submit" (Json.map alwaysPreventDefault (Json.succeed msg))

    alwaysPreventDefault : msg -> ( msg, Bool )
    alwaysPreventDefault msg =
      ( msg, True )
-}
preventDefaultOn :
    String
    -> Json.Decoder ( msg, Bool )
    -> Html.WithContext.Attribute context msg
preventDefaultOn arg1 arg2 =
    Internal.Attribute (\_ -> Html.Events.preventDefaultOn arg1 arg2)


{-| Create an event listener that may [`stopPropagation`][stop] or
[`preventDefault`][prevent].

[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation
[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault

**Note:** If you need something even more custom (like capture phase) check
out the lower-level event API in `elm/virtual-dom`.
-}
custom :
    String
    -> Json.Decoder { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }
    -> Html.WithContext.Attribute context msg
custom arg1 arg2 =
    Internal.Attribute (\_ -> Html.Events.custom arg1 arg2)


{-| A `Json.Decoder` for grabbing `event.target.value`. We use this to define
`onInput` as follows:

    import Json.Decode as Json

    onInput : (String -> msg) -> Attribute msg
    onInput tagger =
      stopPropagationOn "input" <|
        Json.map alwaysStop (Json.map tagger targetValue)

    alwaysStop : a -> (a, Bool)
    alwaysStop x =
      (x, True)

You probably will never need this, but hopefully it gives some insights into
how to make custom event handlers.
-}
targetValue : Json.Decoder String
targetValue =
    Html.Events.targetValue


{-| A `Json.Decoder` for grabbing `event.target.checked`. We use this to define
`onCheck` as follows:

    import Json.Decode as Json

    onCheck : (Bool -> msg) -> Attribute msg
    onCheck tagger =
      on "input" (Json.map tagger targetChecked)
-}
targetChecked : Json.Decoder Bool
targetChecked =
    Html.Events.targetChecked


{-| A `Json.Decoder` for grabbing `event.keyCode`. This helps you define
keyboard listeners like this:

    import Json.Decode as Json

    onKeyUp : (Int -> msg) -> Attribute msg
    onKeyUp tagger =
      on "keyup" (Json.map tagger keyCode)

**Note:** It looks like the spec is moving away from `event.keyCode` and
towards `event.key`. Once this is supported in more browsers, we may add
helpers here for `onKeyUp`, `onKeyDown`, `onKeyPress`, etc.
-}
keyCode : Json.Decoder Int
keyCode =
    Html.Events.keyCode


