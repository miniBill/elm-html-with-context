module Html.WithContext.Attributes exposing (accept, acceptCharset, accesskey, action, align, alt, attribute, autocomplete, autofocus, autoplay, checked, cite, class, classList, cols, colspan, contenteditable, contextmenu, controls, coords, datetime, default, dir, disabled, download, draggable, dropzone, enctype, for, form, headers, height, hidden, href, hreflang, id, ismap, itemprop, kind, lang, list, loop, manifest, map, max, maxlength, media, method, min, minlength, multiple, name, novalidate, pattern, ping, placeholder, poster, preload, property, pubdate, readonly, rel, required, reversed, rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src, srcdoc, srclang, start, step, style, tabindex, target, title, type_, usemap, value, width, wrap)

{-| 
@docs style, classList, property, attribute, map, class, hidden, id, title, accesskey, contenteditable, contextmenu, dir, draggable, dropzone, itemprop, lang, spellcheck, tabindex, src, height, width, alt, autoplay, controls, loop, preload, poster, default, kind, srclang, sandbox, srcdoc, type_, value, checked, placeholder, selected, accept, acceptCharset, action, autocomplete, autofocus, disabled, enctype, list, minlength, maxlength, method, multiple, name, novalidate, pattern, readonly, required, size, for, form, max, min, step, cols, rows, wrap, ismap, usemap, shape, coords, align, cite, href, target, download, hreflang, media, ping, rel, datetime, pubdate, reversed, start, colspan, headers, rowspan, scope, manifest
-}


import Html
import Html.Attributes
import Html.WithContext
import Html.WithContext.Internal as Internal
import Json.Encode as Json
import VirtualDom


{-| Specify a style.

    greeting : Node msg
    greeting =
      div
        [ style "background-color" "red"
        , style "height" "90px"
        , style "width" "100%"
        ]
        [ text "Hello!"
        ]

There is no `Html.Styles` module because best practices for working with HTML
suggest that this should primarily be specified in CSS files. So the general
recommendation is to use this function lightly.
-}
style : String -> String -> Html.WithContext.Attribute context msg
style arg1 arg2 =
    Internal.Attribute (\_ -> Html.Attributes.style arg1 arg2)


{-| This function makes it easier to build a space-separated class attribute.
Each class can easily be added and removed depending on the boolean value it
is paired with. For example, maybe we want a way to view notices:

    viewNotice : Notice -> Html msg
    viewNotice notice =
      div
        [ classList
            [ ("notice", True)
            , ("notice-important", notice.isImportant)
            , ("notice-seen", notice.isSeen)
            ]
        ]
        [ text notice.content ]

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!
-}
classList : List ( String, Bool ) -> Html.WithContext.Attribute context msg
classList arg =
    Internal.Attribute (\_ -> Html.Attributes.classList arg)


{-| Create *properties*, like saying `domNode.className = 'greeting'` in
JavaScript.

    import Json.Encode as Encode

    class : String -> Attribute msg
    class name =
      property "className" (Encode.string name)

Read more about the difference between properties and attributes [here][].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md
-}
property : String -> Json.Value -> Html.WithContext.Attribute context msg
property arg1 arg2 =
    Internal.Attribute (\_ -> Html.Attributes.property arg1 arg2)


{-| Create *attributes*, like saying `domNode.setAttribute('class', 'greeting')`
in JavaScript.

    class : String -> Attribute msg
    class name =
      attribute "class" name

Read more about the difference between properties and attributes [here][].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md
-}
attribute : String -> String -> Html.WithContext.Attribute context msg
attribute arg1 arg2 =
    Internal.Attribute (\_ -> Html.Attributes.attribute arg1 arg2)


{-| Transform the messages produced by an `Attribute`. -}
map :
    (a -> b)
    -> Html.WithContext.Attribute context a
    -> Html.WithContext.Attribute context b
map f attr =
    Internal.Attribute
        (\context ->
            VirtualDom.mapAttribute f (Internal.runAttribute context attr)
        )


{-| Often used with CSS to style elements with common properties.

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!
-}
class : String -> Html.WithContext.Attribute context msg
class arg =
    Internal.Attribute (\_ -> Html.Attributes.class arg)


{-| Indicates the relevance of an element. -}
hidden : Bool -> Html.WithContext.Attribute context msg
hidden arg =
    Internal.Attribute (\_ -> Html.Attributes.hidden arg)


{-| Often used with CSS to style a specific element. The value of this
attribute must be unique.
-}
id : String -> Html.WithContext.Attribute context msg
id arg =
    Internal.Attribute (\_ -> Html.Attributes.id arg)


{-| Text to be displayed in a tooltip when hovering over the element. -}
title : String -> Html.WithContext.Attribute context msg
title arg =
    Internal.Attribute (\_ -> Html.Attributes.title arg)


{-| Defines a keyboard shortcut to activate or add focus to the element. -}
accesskey : Char -> Html.WithContext.Attribute context msg
accesskey arg =
    Internal.Attribute (\_ -> Html.Attributes.accesskey arg)


{-| Indicates whether the element's content is editable. -}
contenteditable : Bool -> Html.WithContext.Attribute context msg
contenteditable arg =
    Internal.Attribute (\_ -> Html.Attributes.contenteditable arg)


{-| Defines the ID of a `menu` element which will serve as the element's
context menu.
-}
contextmenu : String -> Html.WithContext.Attribute context msg
contextmenu arg =
    Internal.Attribute (\_ -> Html.Attributes.contextmenu arg)


{-| Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl
(Right-To-Left).
-}
dir : String -> Html.WithContext.Attribute context msg
dir arg =
    Internal.Attribute (\_ -> Html.Attributes.dir arg)


{-| Defines whether the element can be dragged. -}
draggable : String -> Html.WithContext.Attribute context msg
draggable arg =
    Internal.Attribute (\_ -> Html.Attributes.draggable arg)


{-| Indicates that the element accept the dropping of content on it. -}
dropzone : String -> Html.WithContext.Attribute context msg
dropzone arg =
    Internal.Attribute (\_ -> Html.Attributes.dropzone arg)


{-|-}
itemprop : String -> Html.WithContext.Attribute context msg
itemprop arg =
    Internal.Attribute (\_ -> Html.Attributes.itemprop arg)


{-| Defines the language used in the element. -}
lang : String -> Html.WithContext.Attribute context msg
lang arg =
    Internal.Attribute (\_ -> Html.Attributes.lang arg)


{-| Indicates whether spell checking is allowed for the element. -}
spellcheck : Bool -> Html.WithContext.Attribute context msg
spellcheck arg =
    Internal.Attribute (\_ -> Html.Attributes.spellcheck arg)


{-| Overrides the browser's default tab order and follows the one specified
instead.
-}
tabindex : Int -> Html.WithContext.Attribute context msg
tabindex arg =
    Internal.Attribute (\_ -> Html.Attributes.tabindex arg)


{-| The URL of the embeddable content. For `audio`, `embed`, `iframe`, `img`,
`input`, `script`, `source`, `track`, and `video`.
-}
src : String -> Html.WithContext.Attribute context msg
src arg =
    Internal.Attribute (\_ -> Html.Attributes.src arg)


{-| Declare the height of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.
-}
height : Int -> Html.WithContext.Attribute context msg
height arg =
    Internal.Attribute (\_ -> Html.Attributes.height arg)


{-| Declare the width of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.
-}
width : Int -> Html.WithContext.Attribute context msg
width arg =
    Internal.Attribute (\_ -> Html.Attributes.width arg)


{-| Alternative text in case an image can't be displayed. Works with `img`,
`area`, and `input`.
-}
alt : String -> Html.WithContext.Attribute context msg
alt arg =
    Internal.Attribute (\_ -> Html.Attributes.alt arg)


{-| The `audio` or `video` should play as soon as possible. -}
autoplay : Bool -> Html.WithContext.Attribute context msg
autoplay arg =
    Internal.Attribute (\_ -> Html.Attributes.autoplay arg)


{-| Indicates whether the browser should show playback controls for the `audio`
or `video`.
-}
controls : Bool -> Html.WithContext.Attribute context msg
controls arg =
    Internal.Attribute (\_ -> Html.Attributes.controls arg)


{-| Indicates whether the `audio` or `video` should start playing from the
start when it's finished.
-}
loop : Bool -> Html.WithContext.Attribute context msg
loop arg =
    Internal.Attribute (\_ -> Html.Attributes.loop arg)


{-| Control how much of an `audio` or `video` resource should be preloaded. -}
preload : String -> Html.WithContext.Attribute context msg
preload arg =
    Internal.Attribute (\_ -> Html.Attributes.preload arg)


{-| A URL indicating a poster frame to show until the user plays or seeks the
`video`.
-}
poster : String -> Html.WithContext.Attribute context msg
poster arg =
    Internal.Attribute (\_ -> Html.Attributes.poster arg)


{-| Indicates that the `track` should be enabled unless the user's preferences
indicate something different.
-}
default : Bool -> Html.WithContext.Attribute context msg
default arg =
    Internal.Attribute (\_ -> Html.Attributes.default arg)


{-| Specifies the kind of text `track`. -}
kind : String -> Html.WithContext.Attribute context msg
kind arg =
    Internal.Attribute (\_ -> Html.Attributes.kind arg)


{-| A two letter language code indicating the language of the `track` text data. -}
srclang : String -> Html.WithContext.Attribute context msg
srclang arg =
    Internal.Attribute (\_ -> Html.Attributes.srclang arg)


{-| A space separated list of security restrictions you'd like to lift for an
`iframe`.
-}
sandbox : String -> Html.WithContext.Attribute context msg
sandbox arg =
    Internal.Attribute (\_ -> Html.Attributes.sandbox arg)


{-| An HTML document that will be displayed as the body of an `iframe`. It will
override the content of the `src` attribute if it has been specified.
-}
srcdoc : String -> Html.WithContext.Attribute context msg
srcdoc arg =
    Internal.Attribute (\_ -> Html.Attributes.srcdoc arg)


{-| Defines the type of a `button`, `input`, `embed`, `object`, `script`,
`source`, `style`, or `menu`.
-}
type_ : String -> Html.WithContext.Attribute context msg
type_ arg =
    Internal.Attribute (\_ -> Html.Attributes.type_ arg)


{-| Defines a default value which will be displayed in a `button`, `option`,
`input`, `li`, `meter`, `progress`, or `param`.
-}
value : String -> Html.WithContext.Attribute context msg
value arg =
    Internal.Attribute (\_ -> Html.Attributes.value arg)


{-| Indicates whether an `input` of type checkbox is checked. -}
checked : Bool -> Html.WithContext.Attribute context msg
checked arg =
    Internal.Attribute (\_ -> Html.Attributes.checked arg)


{-| Provides a hint to the user of what can be entered into an `input` or
`textarea`.
-}
placeholder : String -> Html.WithContext.Attribute context msg
placeholder arg =
    Internal.Attribute (\_ -> Html.Attributes.placeholder arg)


{-| Defines which `option` will be selected on page load. -}
selected : Bool -> Html.WithContext.Attribute context msg
selected arg =
    Internal.Attribute (\_ -> Html.Attributes.selected arg)


{-| List of types the server accepts, typically a file type.
For `form` and `input`.
-}
accept : String -> Html.WithContext.Attribute context msg
accept arg =
    Internal.Attribute (\_ -> Html.Attributes.accept arg)


{-| List of supported charsets in a `form`. -}
acceptCharset : String -> Html.WithContext.Attribute context msg
acceptCharset arg =
    Internal.Attribute (\_ -> Html.Attributes.acceptCharset arg)


{-| The URI of a program that processes the information submitted via a `form`. -}
action : String -> Html.WithContext.Attribute context msg
action arg =
    Internal.Attribute (\_ -> Html.Attributes.action arg)


{-| Indicates whether a `form` or an `input` can have their values automatically
completed by the browser.
-}
autocomplete : Bool -> Html.WithContext.Attribute context msg
autocomplete arg =
    Internal.Attribute (\_ -> Html.Attributes.autocomplete arg)


{-| The element should be automatically focused after the page loaded.
For `button`, `input`, `select`, and `textarea`.
-}
autofocus : Bool -> Html.WithContext.Attribute context msg
autofocus arg =
    Internal.Attribute (\_ -> Html.Attributes.autofocus arg)


{-| Indicates whether the user can interact with a `button`, `fieldset`,
`input`, `optgroup`, `option`, `select` or `textarea`.
-}
disabled : Bool -> Html.WithContext.Attribute context msg
disabled arg =
    Internal.Attribute (\_ -> Html.Attributes.disabled arg)


{-| How `form` data should be encoded when submitted with the POST method.
Options include: application/x-www-form-urlencoded, multipart/form-data, and
text/plain.
-}
enctype : String -> Html.WithContext.Attribute context msg
enctype arg =
    Internal.Attribute (\_ -> Html.Attributes.enctype arg)


{-| Associates an `input` with a `datalist` tag. The datalist gives some
pre-defined options to suggest to the user as they interact with an input.
The value of the list attribute must match the id of a `datalist` node.
For `input`.
-}
list : String -> Html.WithContext.Attribute context msg
list arg =
    Internal.Attribute (\_ -> Html.Attributes.list arg)


{-| Defines the minimum number of characters allowed in an `input` or
`textarea`.
-}
minlength : Int -> Html.WithContext.Attribute context msg
minlength arg =
    Internal.Attribute (\_ -> Html.Attributes.minlength arg)


{-| Defines the maximum number of characters allowed in an `input` or
`textarea`.
-}
maxlength : Int -> Html.WithContext.Attribute context msg
maxlength arg =
    Internal.Attribute (\_ -> Html.Attributes.maxlength arg)


{-| Defines which HTTP method to use when submitting a `form`. Can be GET
(default) or POST.
-}
method : String -> Html.WithContext.Attribute context msg
method arg =
    Internal.Attribute (\_ -> Html.Attributes.method arg)


{-| Indicates whether multiple values can be entered in an `input` of type
email or file. Can also indicate that you can `select` many options.
-}
multiple : Bool -> Html.WithContext.Attribute context msg
multiple arg =
    Internal.Attribute (\_ -> Html.Attributes.multiple arg)


{-| Name of the element. For example used by the server to identify the fields
in form submits. For `button`, `form`, `fieldset`, `iframe`, `input`,
`object`, `output`, `select`, `textarea`, `map`, `meta`, and `param`.
-}
name : String -> Html.WithContext.Attribute context msg
name arg =
    Internal.Attribute (\_ -> Html.Attributes.name arg)


{-| This attribute indicates that a `form` shouldn't be validated when
submitted.
-}
novalidate : Bool -> Html.WithContext.Attribute context msg
novalidate arg =
    Internal.Attribute (\_ -> Html.Attributes.novalidate arg)


{-| Defines a regular expression which an `input`'s value will be validated
against.
-}
pattern : String -> Html.WithContext.Attribute context msg
pattern arg =
    Internal.Attribute (\_ -> Html.Attributes.pattern arg)


{-| Indicates whether an `input` or `textarea` can be edited. -}
readonly : Bool -> Html.WithContext.Attribute context msg
readonly arg =
    Internal.Attribute (\_ -> Html.Attributes.readonly arg)


{-| Indicates whether this element is required to fill out or not.
For `input`, `select`, and `textarea`.
-}
required : Bool -> Html.WithContext.Attribute context msg
required arg =
    Internal.Attribute (\_ -> Html.Attributes.required arg)


{-| For `input` specifies the width of an input in characters.

For `select` specifies the number of visible options in a drop-down list.
-}
size : Int -> Html.WithContext.Attribute context msg
size arg =
    Internal.Attribute (\_ -> Html.Attributes.size arg)


{-| The element ID described by this `label` or the element IDs that are used
for an `output`.
-}
for : String -> Html.WithContext.Attribute context msg
for arg =
    Internal.Attribute (\_ -> Html.Attributes.for arg)


{-| Indicates the element ID of the `form` that owns this particular `button`,
`fieldset`, `input`, `label`, `meter`, `object`, `output`, `progress`,
`select`, or `textarea`.
-}
form : String -> Html.WithContext.Attribute context msg
form arg =
    Internal.Attribute (\_ -> Html.Attributes.form arg)


{-| Indicates the maximum value allowed. When using an input of type number or
date, the max value must be a number or date. For `input`, `meter`, and `progress`.
-}
max : String -> Html.WithContext.Attribute context msg
max arg =
    Internal.Attribute (\_ -> Html.Attributes.max arg)


{-| Indicates the minimum value allowed. When using an input of type number or
date, the min value must be a number or date. For `input` and `meter`.
-}
min : String -> Html.WithContext.Attribute context msg
min arg =
    Internal.Attribute (\_ -> Html.Attributes.min arg)


{-| Add a step size to an `input`. Use `step "any"` to allow any floating-point
number to be used in the input.
-}
step : String -> Html.WithContext.Attribute context msg
step arg =
    Internal.Attribute (\_ -> Html.Attributes.step arg)


{-| Defines the number of columns in a `textarea`. -}
cols : Int -> Html.WithContext.Attribute context msg
cols arg =
    Internal.Attribute (\_ -> Html.Attributes.cols arg)


{-| Defines the number of rows in a `textarea`. -}
rows : Int -> Html.WithContext.Attribute context msg
rows arg =
    Internal.Attribute (\_ -> Html.Attributes.rows arg)


{-| Indicates whether the text should be wrapped in a `textarea`. Possible
values are "hard" and "soft".
-}
wrap : String -> Html.WithContext.Attribute context msg
wrap arg =
    Internal.Attribute (\_ -> Html.Attributes.wrap arg)


{-| When an `img` is a descendant of an `a` tag, the `ismap` attribute
indicates that the click location should be added to the parent `a`'s href as
a query string.
-}
ismap : Bool -> Html.WithContext.Attribute context msg
ismap arg =
    Internal.Attribute (\_ -> Html.Attributes.ismap arg)


{-| Specify the hash name reference of a `map` that should be used for an `img`
or `object`. A hash name reference is a hash symbol followed by the element's name or id.
E.g. `"#planet-map"`.
-}
usemap : String -> Html.WithContext.Attribute context msg
usemap arg =
    Internal.Attribute (\_ -> Html.Attributes.usemap arg)


{-| Declare the shape of the clickable area in an `a` or `area`. Valid values
include: default, rect, circle, poly. This attribute can be paired with
`coords` to create more particular shapes.
-}
shape : String -> Html.WithContext.Attribute context msg
shape arg =
    Internal.Attribute (\_ -> Html.Attributes.shape arg)


{-| A set of values specifying the coordinates of the hot-spot region in an
`area`. Needs to be paired with a `shape` attribute to be meaningful.
-}
coords : String -> Html.WithContext.Attribute context msg
coords arg =
    Internal.Attribute (\_ -> Html.Attributes.coords arg)


{-| Specifies the horizontal alignment of a `caption`, `col`, `colgroup`,
`hr`, `iframe`, `img`, `table`, `tbody`,  `td`,  `tfoot`, `th`, `thead`, or
`tr`.
-}
align : String -> Html.WithContext.Attribute context msg
align arg =
    Internal.Attribute (\_ -> Html.Attributes.align arg)


{-| Contains a URI which points to the source of the quote or change in a
`blockquote`, `del`, `ins`, or `q`.
-}
cite : String -> Html.WithContext.Attribute context msg
cite arg =
    Internal.Attribute (\_ -> Html.Attributes.cite arg)


{-| The URL of a linked resource, such as `a`, `area`, `base`, or `link`. -}
href : String -> Html.WithContext.Attribute context msg
href arg =
    Internal.Attribute (\_ -> Html.Attributes.href arg)


{-| Specify where the results of clicking an `a`, `area`, `base`, or `form`
should appear. Possible special values include:

  * _blank &mdash; a new window or tab
  * _self &mdash; the same frame (this is default)
  * _parent &mdash; the parent frame
  * _top &mdash; the full body of the window

You can also give the name of any `frame` you have created.
-}
target : String -> Html.WithContext.Attribute context msg
target arg =
    Internal.Attribute (\_ -> Html.Attributes.target arg)


{-| Indicates that clicking an `a` and `area` will download the resource
directly. The `String` argument determins the name of the downloaded file.
Say the file you are serving is named `hats.json`.

    download ""               -- hats.json
    download "my-hats.json"   -- my-hats.json
    download "snakes.json"    -- snakes.json

The empty `String` says to just name it whatever it was called on the server.
-}
download : String -> Html.WithContext.Attribute context msg
download arg =
    Internal.Attribute (\_ -> Html.Attributes.download arg)


{-| Two-letter language code of the linked resource of an `a`, `area`, or `link`. -}
hreflang : String -> Html.WithContext.Attribute context msg
hreflang arg =
    Internal.Attribute (\_ -> Html.Attributes.hreflang arg)


{-| Specifies a hint of the target media of a `a`, `area`, `link`, `source`,
or `style`.
-}
media : String -> Html.WithContext.Attribute context msg
media arg =
    Internal.Attribute (\_ -> Html.Attributes.media arg)


{-| Specify a URL to send a short POST request to when the user clicks on an
`a` or `area`. Useful for monitoring and tracking.
-}
ping : String -> Html.WithContext.Attribute context msg
ping arg =
    Internal.Attribute (\_ -> Html.Attributes.ping arg)


{-| Specifies the relationship of the target object to the link object.
For `a`, `area`, `link`.
-}
rel : String -> Html.WithContext.Attribute context msg
rel arg =
    Internal.Attribute (\_ -> Html.Attributes.rel arg)


{-| Indicates the date and time associated with the element.
For `del`, `ins`, `time`.
-}
datetime : String -> Html.WithContext.Attribute context msg
datetime arg =
    Internal.Attribute (\_ -> Html.Attributes.datetime arg)


{-| Indicates whether this date and time is the date of the nearest `article`
ancestor element. For `time`.
-}
pubdate : String -> Html.WithContext.Attribute context msg
pubdate arg =
    Internal.Attribute (\_ -> Html.Attributes.pubdate arg)


{-| Indicates whether an ordered list `ol` should be displayed in a descending
order instead of a ascending.
-}
reversed : Bool -> Html.WithContext.Attribute context msg
reversed arg =
    Internal.Attribute (\_ -> Html.Attributes.reversed arg)


{-| Defines the first number of an ordered list if you want it to be something
besides 1.
-}
start : Int -> Html.WithContext.Attribute context msg
start arg =
    Internal.Attribute (\_ -> Html.Attributes.start arg)


{-| The colspan attribute defines the number of columns a cell should span.
For `td` and `th`.
-}
colspan : Int -> Html.WithContext.Attribute context msg
colspan arg =
    Internal.Attribute (\_ -> Html.Attributes.colspan arg)


{-| A space separated list of element IDs indicating which `th` elements are
headers for this cell. For `td` and `th`.
-}
headers : String -> Html.WithContext.Attribute context msg
headers arg =
    Internal.Attribute (\_ -> Html.Attributes.headers arg)


{-| Defines the number of rows a table cell should span over.
For `td` and `th`.
-}
rowspan : Int -> Html.WithContext.Attribute context msg
rowspan arg =
    Internal.Attribute (\_ -> Html.Attributes.rowspan arg)


{-| Specifies the scope of a header cell `th`. Possible values are: col, row,
colgroup, rowgroup.
-}
scope : String -> Html.WithContext.Attribute context msg
scope arg =
    Internal.Attribute (\_ -> Html.Attributes.scope arg)


{-| Specifies the URL of the cache manifest for an `html` tag. -}
manifest : String -> Html.WithContext.Attribute context msg
manifest arg =
    Internal.Attribute (\_ -> Html.Attributes.manifest arg)