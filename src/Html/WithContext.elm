module Html.WithContext exposing (Attribute, Html, a, abbr, address, article, aside, audio, b, bdi, bdo, blockquote, br, button, canvas, caption, cite, code, col, colgroup, datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math, menu, menuitem, meter, nav, node, object, ol, optgroup, option, output, p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select, small, source, span, strong, sub, summary, sup, table, tbody, td, text, textarea, tfoot, th, thead, time, toHtml, tr, track, u, ul, var, video, wbr, withContext, withContextAttribute)

{-| 
@docs withContextAttribute, withContext, toHtml, menu, menuitem, summary, details, meter, progress, output, textarea, option, optgroup, datalist, select, button, input, label, legend, fieldset, form, th, td, tr, tfoot, thead, tbody, col, colgroup, caption, table, math, canvas, track, source, audio, video, param, object, embed, iframe, img, del, ins, wbr, br, span, bdo, bdi, rp, rt, ruby, mark, u, b, i, sup, sub, kbd, samp, var, code, time, abbr, dfn, q, cite, s, small, strong, em, a, div, figcaption, figure, dd, dt, dl, li, ul, ol, blockquote, pre, hr, p, main_, address, footer, header, h6, h5, h4, h3, h2, h1, aside, article, nav, section, map, text, node, Attribute, Html
-}


import Html
import Html.WithContext.Internal as Internal
import VirtualDom


{-| The core building block used to build up HTML. Here we create an `Html`
value with no attributes and one child:

    hello : Html msg
    hello =
      div [] [ text "Hello!" ]
-}
type alias Html context msg =
    Internal.Html context msg


{-| Set attributes on your `Html`. Learn more in the
[`Html.Attributes`](Html-Attributes) module.
-}
type alias Attribute context msg =
    Internal.Attribute context msg


{-| General way to create HTML nodes. It is used to define all of the helper
functions in this library.

    div : List (Attribute msg) -> List (Html msg) -> Html msg
    div attributes children =
        node "div" attributes children

You can use this to create custom nodes if you need to create something that
is not covered by the helper functions in this library.
-}
node :
    String
    -> List (Attribute context msg)
    -> List (Html context msg)
    -> Html context msg
node name attrs children =
    Internal.Html
        (\context ->
            Html.node
                name
                (List.map (Internal.runAttribute context) attrs)
                (List.map (Internal.runHtml context) children)
        )


{-| Just put plain text in the DOM. It will escape the string so that it appears
exactly as you specify.

    text "Hello World!"
-}
text : String -> Html context msg
text content =
    Internal.Html (\_ -> VirtualDom.text content)


{-| Transform the messages produced by some `Html`. In the following example,
we have `viewButton` that produces `()` messages, and we transform those values
into `Msg` values in `view`.

    type Msg = Left | Right

    view : model -> Html Msg
    view model =
      div []
        [ map (\_ -> Left) (viewButton "Left")
        , map (\_ -> Right) (viewButton "Right")
        ]

    viewButton : String -> Html ()
    viewButton name =
      button [ onClick () ] [ text name ]

This should not come in handy too often. Definitely read [this][reuse] before
deciding if this is what you want.

[reuse]: https://guide.elm-lang.org/reuse/
-}
map : (a -> b) -> Html context a -> Html context b
map f child =
    Internal.Html (\context -> Html.map f (Internal.runHtml context child))


{-| Defines a section in a document. -}
section :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
section attrs children =
    node "section" attrs children


{-| Defines a section that contains only navigation links. -}
nav :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
nav attrs children =
    node "nav" attrs children


{-| Defines self-contained content that could exist independently of the rest
of the content.
-}
article :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
article attrs children =
    node "article" attrs children


{-| Defines some content loosely related to the page content. If it is removed,
the remaining content still makes sense.
-}
aside :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
aside attrs children =
    node "aside" attrs children


{-|-}
h1 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h1 attrs children =
    node "h1" attrs children


{-|-}
h2 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h2 attrs children =
    node "h2" attrs children


{-|-}
h3 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h3 attrs children =
    node "h3" attrs children


{-|-}
h4 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h4 attrs children =
    node "h4" attrs children


{-|-}
h5 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h5 attrs children =
    node "h5" attrs children


{-|-}
h6 : List (Attribute context msg) -> List (Html context msg) -> Html context msg
h6 attrs children =
    node "h6" attrs children


{-| Defines the header of a page or section. It often contains a logo, the
title of the web site, and a navigational table of content.
-}
header :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
header attrs children =
    node "header" attrs children


{-| Defines the footer for a page or section. It often contains a copyright
notice, some links to legal information, or addresses to give feedback.
-}
footer :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
footer attrs children =
    node "footer" attrs children


{-| Defines a section containing contact information. -}
address :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
address attrs children =
    node "address" attrs children


{-| Defines the main or important content in the document. There is only one
`main` element in the document.
-}
main_ :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
main_ attrs children =
    node "main" attrs children


{-| Defines a portion that should be displayed as a paragraph. -}
p : List (Attribute context msg) -> List (Html context msg) -> Html context msg
p attrs children =
    node "p" attrs children


{-| Represents a thematic break between paragraphs of a section or article or
any longer content.
-}
hr : List (Attribute context msg) -> List (Html context msg) -> Html context msg
hr attrs children =
    node "hr" attrs children


{-| Indicates that its content is preformatted and that this format must be
preserved.
-}
pre :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
pre attrs children =
    node "pre" attrs children


{-| Represents a content that is quoted from another source. -}
blockquote :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
blockquote attrs children =
    node "blockquote" attrs children


{-| Defines an ordered list of items. -}
ol : List (Attribute context msg) -> List (Html context msg) -> Html context msg
ol attrs children =
    node "ol" attrs children


{-| Defines an unordered list of items. -}
ul : List (Attribute context msg) -> List (Html context msg) -> Html context msg
ul attrs children =
    node "ul" attrs children


{-| Defines a item of an enumeration list. -}
li : List (Attribute context msg) -> List (Html context msg) -> Html context msg
li attrs children =
    node "li" attrs children


{-| Defines a definition list, that is, a list of terms and their associated
definitions.
-}
dl : List (Attribute context msg) -> List (Html context msg) -> Html context msg
dl attrs children =
    node "dl" attrs children


{-| Represents a term defined by the next `dd`. -}
dt : List (Attribute context msg) -> List (Html context msg) -> Html context msg
dt attrs children =
    node "dt" attrs children


{-| Represents the definition of the terms immediately listed before it. -}
dd : List (Attribute context msg) -> List (Html context msg) -> Html context msg
dd attrs children =
    node "dd" attrs children


{-| Represents a figure illustrated as part of the document. -}
figure :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
figure attrs children =
    node "figure" attrs children


{-| Represents the legend of a figure. -}
figcaption :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
figcaption attrs children =
    node "figcaption" attrs children


{-| Represents a generic container with no special meaning. -}
div :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
div attrs children =
    node "div" attrs children


{-| Represents a hyperlink, linking to another resource. -}
a : List (Attribute context msg) -> List (Html context msg) -> Html context msg
a attrs children =
    node "a" attrs children


{-| Represents emphasized text, like a stress accent. -}
em : List (Attribute context msg) -> List (Html context msg) -> Html context msg
em attrs children =
    node "em" attrs children


{-| Represents especially important text. -}
strong :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
strong attrs children =
    node "strong" attrs children


{-| Represents a side comment, that is, text like a disclaimer or a
copyright, which is not essential to the comprehension of the document.
-}
small :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
small attrs children =
    node "small" attrs children


{-| Represents content that is no longer accurate or relevant. -}
s : List (Attribute context msg) -> List (Html context msg) -> Html context msg
s attrs children =
    node "s" attrs children


{-| Represents the title of a work. -}
cite :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
cite attrs children =
    node "cite" attrs children


{-| Represents an inline quotation. -}
q : List (Attribute context msg) -> List (Html context msg) -> Html context msg
q attrs children =
    node "q" attrs children


{-| Represents a term whose definition is contained in its nearest ancestor
content.
-}
dfn :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
dfn attrs children =
    node "dfn" attrs children


{-| Represents an abbreviation or an acronym; the expansion of the
abbreviation can be represented in the title attribute.
-}
abbr :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
abbr attrs children =
    node "abbr" attrs children


{-| Represents a date and time value; the machine-readable equivalent can be
represented in the datetime attribute.
-}
time :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
time attrs children =
    node "time" attrs children


{-| Represents computer code. -}
code :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
code attrs children =
    node "code" attrs children


{-| Represents a variable. Specific cases where it should be used include an
actual mathematical expression or programming context, an identifier
representing a constant, a symbol identifying a physical quantity, a function
parameter, or a mere placeholder in prose.
-}
var :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
var attrs children =
    node "var" attrs children


{-| Represents the output of a program or a computer. -}
samp :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
samp attrs children =
    node "samp" attrs children


{-| Represents user input, often from the keyboard, but not necessarily; it
may represent other input, like transcribed voice commands.
-}
kbd :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
kbd attrs children =
    node "kbd" attrs children


{-| Represent a subscript. -}
sub :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
sub attrs children =
    node "sub" attrs children


{-| Represent a superscript. -}
sup :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
sup attrs children =
    node "sup" attrs children


{-| Represents some text in an alternate voice or mood, or at least of
different quality, such as a taxonomic designation, a technical term, an
idiomatic phrase, a thought, or a ship name.
-}
i : List (Attribute context msg) -> List (Html context msg) -> Html context msg
i attrs children =
    node "i" attrs children


{-| Represents a text which to which attention is drawn for utilitarian
purposes. It doesn't convey extra importance and doesn't imply an alternate
voice.
-}
b : List (Attribute context msg) -> List (Html context msg) -> Html context msg
b attrs children =
    node "b" attrs children


{-| Represents a non-textual annotation for which the conventional
presentation is underlining, such labeling the text as being misspelt or
labeling a proper name in Chinese text.
-}
u : List (Attribute context msg) -> List (Html context msg) -> Html context msg
u attrs children =
    node "u" attrs children


{-| Represents text highlighted for reference purposes, that is for its
relevance in another context.
-}
mark :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
mark attrs children =
    node "mark" attrs children


{-| Represents content to be marked with ruby annotations, short runs of text
presented alongside the text. This is often used in conjunction with East Asian
language where the annotations act as a guide for pronunciation, like the
Japanese furigana.
-}
ruby :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
ruby attrs children =
    node "ruby" attrs children


{-| Represents the text of a ruby annotation. -}
rt : List (Attribute context msg) -> List (Html context msg) -> Html context msg
rt attrs children =
    node "rt" attrs children


{-| Represents parenthesis around a ruby annotation, used to display the
annotation in an alternate way by browsers not supporting the standard display
for annotations.
-}
rp : List (Attribute context msg) -> List (Html context msg) -> Html context msg
rp attrs children =
    node "rp" attrs children


{-| Represents text that must be isolated from its surrounding for
bidirectional text formatting. It allows embedding a span of text with a
different, or unknown, directionality.
-}
bdi :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
bdi attrs children =
    node "bdi" attrs children


{-| Represents the directionality of its children, in order to explicitly
override the Unicode bidirectional algorithm.
-}
bdo :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
bdo attrs children =
    node "bdo" attrs children


{-| Represents text with no specific meaning. This has to be used when no other
text-semantic element conveys an adequate meaning, which, in this case, is
often brought by global attributes like `class`, `lang`, or `dir`.
-}
span :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
span attrs children =
    node "span" attrs children


{-| Represents a line break. -}
br : List (Attribute context msg) -> List (Html context msg) -> Html context msg
br attrs children =
    node "br" attrs children


{-| Represents a line break opportunity, that is a suggested point for
wrapping text in order to improve readability of text split on several lines.
-}
wbr :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
wbr attrs children =
    node "wbr" attrs children


{-| Defines an addition to the document. -}
ins :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
ins attrs children =
    node "ins" attrs children


{-| Defines a removal from the document. -}
del :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
del attrs children =
    node "del" attrs children


{-| Represents an image. -}
img :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
img attrs children =
    node "img" attrs children


{-| Embedded an HTML document. -}
iframe :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
iframe attrs children =
    node "iframe" attrs children


{-| Represents a integration point for an external, often non-HTML,
application or interactive content.
-}
embed :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
embed attrs children =
    node "embed" attrs children


{-| Represents an external resource, which is treated as an image, an HTML
sub-document, or an external resource to be processed by a plug-in.
-}
object :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
object attrs children =
    node "object" attrs children


{-| Defines parameters for use by plug-ins invoked by `object` elements. -}
param :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
param attrs children =
    node "param" attrs children


{-| Represents a video, the associated audio and captions, and controls. -}
video :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
video attrs children =
    node "video" attrs children


{-| Represents a sound or audio stream. -}
audio :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
audio attrs children =
    node "audio" attrs children


{-| Allows authors to specify alternative media resources for media elements
like `video` or `audio`.
-}
source :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
source attrs children =
    node "source" attrs children


{-| Allows authors to specify timed text track for media elements like `video`
or `audio`.
-}
track :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
track attrs children =
    node "track" attrs children


{-| Represents a bitmap area for graphics rendering. -}
canvas :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
canvas attrs children =
    node "canvas" attrs children


{-| Defines a mathematical formula. -}
math :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
math attrs children =
    node "math" attrs children


{-| Represents data with more than one dimension. -}
table :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
table attrs children =
    node "table" attrs children


{-| Represents the title of a table. -}
caption :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
caption attrs children =
    node "caption" attrs children


{-| Represents a set of one or more columns of a table. -}
colgroup :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
colgroup attrs children =
    node "colgroup" attrs children


{-| Represents a column of a table. -}
col :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
col attrs children =
    node "col" attrs children


{-| Represents the block of rows that describes the concrete data of a table. -}
tbody :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
tbody attrs children =
    node "tbody" attrs children


{-| Represents the block of rows that describes the column labels of a table. -}
thead :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
thead attrs children =
    node "thead" attrs children


{-| Represents the block of rows that describes the column summaries of a table. -}
tfoot :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
tfoot attrs children =
    node "tfoot" attrs children


{-| Represents a row of cells in a table. -}
tr : List (Attribute context msg) -> List (Html context msg) -> Html context msg
tr attrs children =
    node "tr" attrs children


{-| Represents a data cell in a table. -}
td : List (Attribute context msg) -> List (Html context msg) -> Html context msg
td attrs children =
    node "td" attrs children


{-| Represents a header cell in a table. -}
th : List (Attribute context msg) -> List (Html context msg) -> Html context msg
th attrs children =
    node "th" attrs children


{-| Represents a form, consisting of controls, that can be submitted to a
server for processing.
-}
form :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
form attrs children =
    node "form" attrs children


{-| Represents a set of controls. -}
fieldset :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
fieldset attrs children =
    node "fieldset" attrs children


{-| Represents the caption for a `fieldset`. -}
legend :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
legend attrs children =
    node "legend" attrs children


{-| Represents the caption of a form control. -}
label :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
label attrs children =
    node "label" attrs children


{-| Represents a typed data field allowing the user to edit the data. -}
input :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
input attrs children =
    node "input" attrs children


{-| Represents a button. -}
button :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
button attrs children =
    node "button" attrs children


{-| Represents a control allowing selection among a set of options. -}
select :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
select attrs children =
    node "select" attrs children


{-| Represents a set of predefined options for other controls. -}
datalist :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
datalist attrs children =
    node "datalist" attrs children


{-| Represents a set of options, logically grouped. -}
optgroup :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
optgroup attrs children =
    node "optgroup" attrs children


{-| Represents an option in a `select` element or a suggestion of a `datalist`
element.
-}
option :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
option attrs children =
    node "option" attrs children


{-| Represents a multiline text edit control. -}
textarea :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
textarea attrs children =
    node "textarea" attrs children


{-| Represents the result of a calculation. -}
output :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
output attrs children =
    node "output" attrs children


{-| Represents the completion progress of a task. -}
progress :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
progress attrs children =
    node "progress" attrs children


{-| Represents a scalar measurement (or a fractional value), within a known
range.
-}
meter :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
meter attrs children =
    node "meter" attrs children


{-| Represents a widget from which the user can obtain additional information
or controls.
-}
details :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
details attrs children =
    node "details" attrs children


{-| Represents a summary, caption, or legend for a given `details`. -}
summary :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
summary attrs children =
    node "summary" attrs children


{-| Represents a command that the user can invoke. -}
menuitem :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
menuitem attrs children =
    node "menuitem" attrs children


{-| Represents a list of commands. -}
menu :
    List (Attribute context msg) -> List (Html context msg) -> Html context msg
menu attrs children =
    node "menu" attrs children


{-| Turn an `Html context msg` from elm-html-with-context into an `Html msg` from elm/html -}
toHtml : context -> Html context msg -> Html.Html msg
toHtml =
    Internal.runHtml


{-| Use the context passed in to create an Html node -}
withContext : (context -> Html context msg) -> Html context msg
withContext =
    Internal.withContext


{-| Use the context passed in to create an Attribute -}
withContextAttribute :
    (context -> Attribute context msg) -> Attribute context msg
withContextAttribute =
    Internal.withContextAttribute


