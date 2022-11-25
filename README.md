# html-with-context

This library wraps [`elm/core`](https://package.elm-lang.org/packages/elm/core/latest/) to provide a global context available while building the view.

A context is a global, _constant or mostly constant_ object. It can be used to store those things that you will need _almost everywhere_ in your `view` but don't change often, or at all.

Examples of things you could want to put in the context:

1. theme (dark/light/custom) - this is needed almost everwhere for colors, and styles, and changes very rarely;
2. language - this is needed for every single label for localization, and changes rarely or never;
3. timezone - this is needed to display local times for the user, and mostly doesn't change;
4. responsive class (phone/tablet/desktop) - this doesn't usually change (unless the user dramatically resizes the window);
5. user permissions (to disable buttons or inputs) - this changes very rarely and is needed in a lot of places;
6. user ID - this only changes on login/logout.

Example of things that you do _not_ want in the context:

1. time - this changes constantly;
2. window size - on desktop, this can change a lot while resizing.

A good test for inclusion is to think of this: does it make sense to completely redraw the user interface when the value changes? In particular, changing anything in the context will force the recalculation of all the `lazy` nodes.

## How to use it

1. Define a `Context` type (it will usually be a type alias);
2. replace any `import Html` and any `import Html.X` with:

   ```elm
   import Html.WithContext as Html
   import Html.WithContext.X
   ```

3. don't expose `Html` or `Attribute` in the `import`, but instead define your type aliases:

   ```elm
   type Html msg =
       Html.Html Context msg

   type Attribute msg =
       Html.Attribute Context msg
   ```

4. pass the context to `Html.toHtml`;
5. everything should work as before, but now you can use `withContext` and `withContextAttribute` to access your context.

## Example: localization

A nice way to do localization is to completely avoid exposing `text` from `Html`, and instead defining your custom one like this:

```elm
type Language
    = En
    | It
    | Fr


type alias L10N a =
    { en : a
    , it : a
    , fr : a
    }


text : L10N String -> Html { a | language : Language } msg
text { en, it, fr } =
    Html.withContext
        (\{ language } ->
            case language of
                En ->
                    Html.text en

                It ->
                    Html.text it

                Fr ->
                    Html.text fr
        )
```

So that you can use it like this: `text { en = "Hello", it = "Ciao", fr = "Bonjour" }` (you should also probably move all the localized strings into a `Localization` package).

This has the advantage of keeping a nice API while making it (almost) impossible to have untranslated labels.

Notice how `text` simply requires a context that includes a `language` field, so is very generic.

This tecnique can be adapted for image sources, title texts, and anything that needs localization.

Strings with placeholders can be represented as `L10N (a -> b -> String)` and used by defining an `apply : L10N (a -> b) -> a -> L10N b`. Beware: different languages can have very different rules on plurals, genders, special cases, ...

## Example: Theme

If you have a field `theme : Theme` in your context then you can replace any color constants in your code with `Theme -> Color` functions, and use them like this:

```elm
type Theme
    = Light
    | Dark


fontColor : Theme -> String
fontColor theme =
    case theme of
        Light ->
            "#333"

        Dark ->
            "#ddd"


someViewFunction =
    div
        [ Html.withContextAttribute <| \{theme} -> Attribute.style "color" <| fontColor theme ]
        (text "Hello")
```

This also has the advantage that you can "force" a particular theme in places that need it, like the theme picker, by just doing `fontColor Light`.

## API differences from the original `elm/html`

1. `Html msg` becomes `Html context msg` and `Attribute msg` becomes `Attribute context msg`,
2. added the `toHtml` function,
3. you have access to the `withContext` and `withContextAttribute` functions.

---

You should also have a look at [the original README](https://package.elm-lang.org/packages/elm/core/latest/).
